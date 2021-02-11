# How to make riker tell you things

## Rebuilding with output

To run a rebuild that prints each command as it is run:

```
$ rkr build --show
```

The following invocation has no subcommand, so it uses the `build` subcommand by default. Therefore, it is the same as the previous command:

```
$ rkr build --show
```

## Changed commands

To plan a rebuild and print all commands that would run:

```
$ rkr check
```

To see the above, plus warnings (potential rkr runtime failues), run:

```
$ rkr check --log=all
```

## Last trace

To print the last trace, run:

```
$ rkr trace
```
