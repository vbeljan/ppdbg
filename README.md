# ppdbg - multiprocess logging tool

## Why?

Debugging and troubleshooting issues with large multiprocess erlang applications
can be quite a challenge and consistent logging of everything going on can be
tricky, especially if you have to analyze all your problems post-mortem. ppdbg
is a tiny and simple tool to make that a bit easier. Use it to:

- set named logging checkpoints in your code that will register whenever a certain
  process passed through it

- tag processes with readable and recognizable indices or names

- generate a csv out of the table of all checkpoint hits, pre-sorted by process,
  time or checkpoint

## Installation

Just compile ppdbg.erl and ppdbg_reader.erl and put the beams into your project.

## How do I use it?

### Adding ppdbg logging in your code

ppdbg is a gen-server, so first start it with

    ppdbg:start([{logpath, "/path/to/logs"}])

The 'logpath' parameter will tell ppdbg where to store the logs.
Next, find a good spot to tag the processes and call

    ppdbg:tag(Tag)

...where Tag is a string (list) with a readable and logical name you want to
assign to a process. I.e. you might want to generate an index based on
parameters given to the process as it's initialized and use it as a process tag.

Finally, identify key checkpoints in the code that the tracked process are
expected to run and place a ppdbg checkpoint call where appropriate:

    ppdbg:checkpoint("First checkpoint")

As an argument, pick a representative name for the checkpoint. Make sure that
it's a string of course.

Finally, stop ppdbg when you want it to stop logging with

    ppdbg:stop()

### Outputting the data

Once you run your program with ppdbg calls included, there should be a dets
table in the log directory specified when starting ppdbg in your code. Now you
can use ppdbg_reader to generate a readable report out of it. The reader also
has a rudimentary mechanism to sort and narrow down the logs by any of the parameters required.
Use escript to call it like in following examples:

    escript ppdbg_reader.beam /path/to/logdir SORTBY proc

This will list all the checkpoint events sorted by the process name. Use 'time'
or 'checkpoint' to sort by timestamp or checkpoint name instead.

    escript ppdbg_reader.beam /path/to/logdir SORTBY time SELECT time FROM
    <<"10:45:15">> TO <<"10:45:30">>

Sort checkpoint events by time, but only print out those where time ranges from
10:45:15 to 10:45:30. SELECT defines which of the 3 columns (proc, checkpoint,
time) is going to be selected for narrowing down and FROM, TO are used to
specify the range. If string parameters (proc, checkpoint) are used for
selection, they will be checked alphabetically.
