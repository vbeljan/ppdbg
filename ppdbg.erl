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
-export([start/1,tag/1,checkpoint/1]).

-define(SERVER, ?MODULE).

-record(state, {logpath}).

-record(process, {pid,
                  name}).

-record(checkevent, {eventid,
                     checkpointname,
                     timestamp,
                     procstate}).
                     

%%%===================================================================
%%% API
%%%===================================================================


start(Opts) ->
    case ppdbg_server:start_link(Opts) of 
        {ok, _Pid} ->
            dbg("Started ppdbg");
        {error, Reason} ->
            dbg("Couldn't start ppdbg, reason: ~p", [Reason])
    end.

tag(Param) ->
    call(tag, Param),
    ok.

checkpoint(ChckptName) ->
    call(chk, ChckptName),
    ok.

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
    {ok, #state{
            logpath = Logpath
           }}.

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
handle_call({tag, Params}, From, State) ->
    Name = case pl_get_value(Params, name) of
               false -> pid_to_list(From);
               N -> N end,

    Proc = #process{pid = From,
                 name = Name},
    NState = tag_process(State, Proc),
    {reply,

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

pl_get_value(PropList, Key) ->
    case lists:keyfind(Key, 1, PropList) of
        {_, Res} ->
            Res;
        _ ->
            false
    end.

call(Action, What) ->
    gen_server:call(self(), {Action, What}).

dbg(Msg)->
    io:format("PPDBG: " ++ Msg).
dbg(Msg, Args) ->
    io:format(Msg, Args).

tag_process(Params) ->
    Args = []
    dets:open_file(



    
    
