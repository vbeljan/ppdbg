%% #!/usr/bin/env escript

-module(ppdbg_reader).
-define(CHK_EVENT, "ppdbg_event").
-define(PROC, 1).
-define(TIME, 2).
-define(CHK, 3).
-export([main/1]).

main([Path, Sortby]) ->
    {ok,EventTable} = dets:open_file(filename:join([Path,?CHK_EVENT])),
    R = dets:foldl(fun(Entry, Acc) -> arrange(Entry, Sortby, Acc) end, [], EventTable),
    io:format("REPORT: ~n~p", [R]);

main(_) ->
    io:format("Usage: escript ppdbg_reader /home/ppdbg/logs 2").

arrange(Entry, What, Acc) when is_list(What) ->
    arrange(Entry, list_to_atom(What), Acc);
arrange({Proc, Time, Checkpoint}, proc, Acc) -> orddict:append(Proc,{Time, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, time, Acc) -> orddict:append(Time,{Proc, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, checkpoint, Acc) -> orddict:append(Checkpoint,{Proc, Time}, Acc).
