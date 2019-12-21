%% #!/usr/bin/env escript

-module(ppdbg_reader).
-define(CHK_EVENT, "ppdbg_event").
-define(PROC, 1).
-define(TIME, 2).
-define(CHK, 3).

-define(SORTBY, "SORTBY").
-define(SELECT, "SELECT").
-define(FROM, "FROM").
-define(TO, "TO").
-define(INVERT, "INVERT").
-export([main/1]).

-record(params,{
                sortby = proc,
                select = proc,
                from = undefined,
                to = undefined,
                invert = false}).

main([Path|Rest]) ->
    case parseargs(Rest) of
        error ->
            error;
        Args ->
            io:format("PARSED ARGS: ~p", [Args]),
            {ok,EventTable} = dets:open_file(filename:join([Path,?CHK_EVENT])),
            Selection = selection(EventTable, Args),
            io:format("SELECTION: ~p~n", [Selection]),
            R = dets:foldl(fun(Entry, Acc) -> arrange(Entry, Args#params.sortby, Acc) end, [], Selection),
            io:format("Report for ~p: ~n~p", [Path, R])
    end;

main(_) ->
    announce_bad_syntax(),
    io:format("Example: escript ppdbg_reader /home/ppdbg/logs SORTBY proc").

parseargs(Arglist) ->
    parseargs(#params{}, Arglist).

parseargs(P, []) ->
    P;
parseargs(P, [H|T]) ->
    [V|Rest] = T,
    case match(H,P,V) of
        {NewParams, ok} ->
            parseargs(NewParams, Rest);
        _Error ->
            announce_bad_syntax()
    end.

match(?SORTBY,P,Value) ->
    {P#params{sortby=list_to_atom(Value)}, ok};
match(?SELECT,P,Value) ->
    {P#params{select=list_to_atom(Value)}, ok};
match(?FROM,P,Value) ->
    {P#params{from=Value}, ok};
match(?TO,P,Value) ->
    {P#params{to=Value}, ok};
match(?INVERT,P,Value) when is_boolean(Value) ->
    P#params{invert=true};
match(_,_,_) ->
    undefined.

selection(List, Params) ->
    selection(List,
              Params#params.select,
              Params#params.from,
              Params#params.to,
              Params#params.invert).

selection(List, _Col, undefined, undefined, _Invert) ->
    List;

selection(List, Column, From, To, Invert) ->
    dets:foldl(fun(Input, Res) -> Res ++ select(Input, Column, From, To, Invert) end, [], List).

select(Input, Column, From, undefined, _Invert) ->
    case element(Column, Input) > From of
        true ->
            [Input];
        _ ->
            []
    end;

select(Input, Column, undefined, To, _Invert) ->
    case element(Column, Input) < To of
        true ->
            [Input];
        _ ->
            []
    end;

select(Input, Column, From, To, false) when element(Column,Input) < To,
                                            element (Column, Input) > From->
    [Input];

select(Input, Column, From, To, true) when element(Column,Input) > To,
                                            element (Column, Input) < From->
    [Input];

select(_,_,_,_,_) ->
    [].

arrange(Entry, What, Acc) when is_list(What) ->
    arrange(Entry, list_to_atom(What), Acc);
arrange({Proc, Time, Checkpoint}, proc, Acc) -> orddict:append(Proc,{Time, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, time, Acc) -> orddict:append(Time,{Proc, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, checkpoint, Acc) -> orddict:append(Checkpoint,{Proc, Time}, Acc).


announce_bad_syntax() ->
    io:format("Bad syntax in arguments~n"),
    error.
