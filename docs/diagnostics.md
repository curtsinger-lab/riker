# How to make dodo tell you things

## Rebuilding with output

To run a rebuild that prints each command as it is run:

```
$ dodo build --show
```

The following invocation has no subcommand, so it uses the `build` subcommand by default. Therefore, it is the same as the previous command:

```
$ dodo build --show
```

## Changed commands

To plan a rebuild and print all commands that would run:

```
$ dodo check
```

To see the above, plus warnings (potential dodo runtime failues), run:

```
$ dodo check -v
```

To see the above, plus info (dependence edges), run:

```
$ dodo check -vv
```

To see the above, plus logs (changes), run:

```
$ dodo check -vvv
```

## Last trace

To print the last trace, run:

```
$ dodo trace
```
