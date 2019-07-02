%% #!/usr/bin/env escript

-module(ppdbg_reader).
-define(CHK_EVENT, ppdbg_event).
-define(PROC, 1).
-define(TIME, 2).
-define(CHK, 3).
-export([main/1]).

main([Path, Sortby]) ->
    dets:open_file(filename:join(Path, ?CHK_EVENT)),
    R = dets:foldl(fun(Entry, Acc) -> arrange(Entry, Sortby, Acc) end, [], ?CHK_EVENT),
    io:format("REPORT: ~n~p", [R]).

arrange({Proc, Time, Checkpoint}, proc, Acc) -> orddict:append(Proc,{Time, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, time, Acc) -> orddict:append(Time,{Proc, Checkpoint}, Acc);
arrange({Proc, Time, Checkpoint}, checkpoint, Acc) -> orddict:append(Checkpoint,{Proc, Time}, Acc).
