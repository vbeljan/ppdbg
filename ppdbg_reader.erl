%% #!/usr/bin/env escript

-module(ppdbg_reader).
-define(REPORT_FILE, "report.csv").

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
                select = 1,
                from = undefined,
                to = undefined,
                invert = false}).

main([Path|Rest]) ->
    case parseargs(Rest) of
        error ->
            error;
        Args ->
            {ok,EventTable} = dets:open_file(filename:join([Path,?CHK_EVENT])),
            Selection = selection(EventTable, Args),
            R = lists:foldl(fun(Entry, Acc) -> arrange(Entry, Args#params.sortby, Acc) end, [], Selection),
            assemble_report(Path, R)
    end;

main(_) ->
    announce_bad_syntax(),
    io:format("Example: escript ppdbg_reader.beam /home/ppdbg/logs SORTBY proc").

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
    {P#params{select=selection_to_column_no(Value)}, ok};
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
    dets:foldl(fun(Input, Res) -> Res ++ [Input] end, [], List);

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

select(Input, Column, From, To, false) when (element(Column,Input) =< To) and
                                            (element (Column, Input) >= From)->
    [Input];

select(Input, Column, From, To, true) when (element(Column,Input) >= To) and
                                           (element (Column, Input) =< From)->
    [Input];

select(_,_,_,_,_) ->
    [].

arrange(Entry, What, Acc) when is_list(What) ->
    arrange(Entry, list_to_atom(What), Acc);
arrange({Proc, Time, Checkpoint}, proc, Acc) -> orddict:append(Proc,{Time, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, time, Acc) -> orddict:append(Time,{Proc, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, checkpoint, Acc) -> orddict:append(Checkpoint,{Proc, Time}, Acc).

assemble_report(Path, List) ->
    {ok,RF} = file:open(filename:join(Path, ?REPORT_FILE), write),
    do_assemble_report(RF, List),
    file:close(RF).

do_assemble_report(_File, []) ->
    ok;

do_assemble_report(File, [{Key,Value}|T]) ->
    lists:foreach(fun(Elem)-> write_entry(Key, Elem, File) end, Value),
    do_assemble_report(File, T).

write_entry(Key, {Val1,Val2}, File) ->
    C = list_to_binary(stringify(Key, Val1, Val2)),
    file:write(File, C);

write_entry(Key, Value, _File) ->
    io:format("Bad match in write_entry for key ~p, value ~p is not a tuple~n", [Key, Value]).

announce_bad_syntax() ->
    io:format("Bad syntax in arguments~n"),
    error.

stringify(K, V1, V2) ->
    string:join([stringify(K), stringify(V1), stringify(V2), "\n"], ",").

stringify(What) when is_binary(What) ->
    binary_to_list(What);
stringify(What) when is_list(What) ->
    What;
stringify(What) ->
    io:format("Warning, parameter ~p is neither binary (time) or list~n", [What]),
    What.


selection_to_column_no("proc")->
    1;
selection_to_column_no("time") ->
    2;
selection_to_column_no("checkpoint") ->
    3.
