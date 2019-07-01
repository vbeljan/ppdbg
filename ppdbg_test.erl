-module(ppdbg_test).

-export([test/0, testfun/1]).

test() ->
    ppdbg:start([{logpath, "/home/vanja/ppdbg"}]),
    spawn(?MODULE, testfun, [1]),
    spawn(?MODULE, testfun, [2]),
    spawn(?MODULE, testfun, [3]),
    timer:sleep(5000),
    dets:all(),
    ppdbg:stop().

testfun(Index)->
    ppdbg:tag(integer_to_list(Index)),
    io:format(standard_io,"Tester index ~p starting up~n", [Index]),
    timer:sleep(Index*1000),
    ppdbg:checkpoint("Prva"),
    timer:sleep(Index*1000),
    ppdbg:checkpoint("Druga"),
    ok.
