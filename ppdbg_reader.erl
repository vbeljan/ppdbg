%% #!/usr/bin/env escript

-module(ppdbg_reader).
-define(CHK_EVENT, ppdbg_event).
-define(PROC, 1).
-define(TIME, 2).
-define(CHK, 3).

main([Path, Sortby]) ->
    dets:open_file(filename:join(Path, ?CHK_EVENT)),
    Sort = getkey(Sortby),

    Delimiter = ",".
    dets:foldl(fun([]) ->
                       

getkey(proc) -> 1;
getkey(time) -> 2;
getkey(checkpoint) -> 3.
