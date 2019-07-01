%%%-------------------------------------------------------------------
%%% @author Vanja Beljan
%%% @copyright (C) 2018, Vanja
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2018 by Vanja
%%%-------------------------------------------------------------------
-module(ppdbg).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([start/1, stop/0, tag/1,checkpoint/1]).

-define(SERVER, ?MODULE).
-define(CHK_EVENT, ppdbg_event).
-define(PROC_TAG, ppdbg_proc_tag).


-record(state, {logpath}).

-record(process, {pid,
                  name}).

-record(checkpoint, {timestamp,
                     checkpointname,
                     pid}).


%%%===================================================================
%%% API
%%%===================================================================


start(Opts) ->
   case ppdbg:start_link(Opts) of
        {ok, Pid} ->
            dbg("Started ppdbg, pid ~p", [Pid]);
        {error, Reason} ->
            dbg("Couldn't start ppdbg, reason: ~p", [Reason])
    end.

stop() ->
    cast(stop, ok).

tag(Tag) when is_list(Tag) ->
    call(tag, Tag),
    ok;
tag(_) ->
    dbg("Cannot tag process, please only use a string as a process tag."),
    nok.


checkpoint(ChckptName) when is_list(ChckptName) ->
    Checkpoint = #checkpoint{timestamp = take_timestamp(),
                             checkpointname = ChckptName,
                             pid = self()},
    dbg("Triggered checkpoint ~p", [ChckptName]),
    cast(checkpoint, Checkpoint),
    ok;

checkpoint(_) ->
    dbg("Invalid checkpoint name, please use strings."),
    nok.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(_Opts) -> {ok, Pid :: pid()} |
                          {error, Error :: {already_started, pid()}} |
                          {error, Error :: term()} |
                          ignore.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init(Opts) ->
    process_flag(trap_exit, true),
    Logpath = pl_get_value(Opts, logpath),

    case {init_table(memory, ?PROC_TAG), init_table(Logpath, ?CHK_EVENT)} of
        {_Ref,{ok, _}} ->
            {ok, #state{logpath = Logpath}};

    Errors ->
            dbg("DETS tables init failed: ~p", [Errors]),
            stop(),
            {ok,#state{}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({tag, ProcTag}, {Pid, _Ref}, State) ->
    Proc = #process{pid=Pid,
                    name=ProcTag},
    tag_process(Proc),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_cast({checkpoint, Chk}, State) ->
    pass_checkpoint(Chk),
    {noreply, State};

handle_cast({stop, _Reason}, State) ->
    dbg("Stopping PPDBG now..."),
    {stop, shutdown, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    dbg("Terminating PPDBG"),
    dets:close(?CHK_EVENT),
    dets:close(?PROC_TAG),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_table(memory, Name) ->
    EtsArgs = [set, 
               named_table, 
               protected],
    
    catch ets:new(Name, EtsArgs);

init_table(Logpath, Name) ->
    Namepath = filename:join(Logpath, Name),
    DetsArgs = [{file, Namepath},
            {access, read_write},
            {type, bag}],

    catch dets:open_file(Name, DetsArgs).

pl_get_value(PropList, Key) ->
    case lists:keyfind(Key, 1, PropList) of
        {_, Res} ->
            Res;
        _ ->
            false
    end.

call(Action, What) ->
    gen_server:call(?SERVER, {Action, What}).
cast(Action, What) ->
    gen_server:cast(?SERVER, {Action, What}).

dbg(Msg)->
    io:format("PPDBG: " ++ Msg ++ "~n").
dbg(Msg, Args) ->
    io:format("PPDBG: " ++ Msg ++ "~n", Args).

tag_process(Proc) ->
    case ets:insert_new(?PROC_TAG, {Proc#process.pid,
                                    Proc#process.name}) of
        true ->
            ok;
        false ->
            dbg("This process already tagged, doing nothing: ~p", [Proc]);
        Error ->
            dbg("Error while tagging process: ~p", [Error])
    end,
    ok.

pass_checkpoint(Chk) ->
    case ets:lookup(?PROC_TAG, Chk#checkpoint.pid) of
        [] ->
            %% The process triggering the checkpoint was not tagged, do nothing
            ok;
        [{_Pid, ProcName}] ->
            do_pass_checkpoint(ProcName, 
                               Chk#checkpoint.timestamp, 
                               Chk#checkpoint.checkpointname);
        Error ->
            dbg("Error looking up process: ~p", [Error])
    end.

do_pass_checkpoint(ProcName, ChkName, Time) ->
    try dets:insert(?CHK_EVENT, {ProcName, ChkName, Time}) of
        ok ->
            ok
    catch Error ->
            dbg("Error while registering checkpoint: ~p", [Error]),
            stop()
    end.

take_timestamp() ->
    {_, {H,M,S}} = erlang:localtime(),
    list_to_binary(io_lib:format("~p:~p:~p" , [H,M,S])).
    
