# How to make dodo tell you things

## Changed commands

To see all of the commands that need to run:

```
$ dodo --show
```

## Verbose changed commands

To see commands and their arguments, run

```
$ dodo check
```

To see the above, plus warnings (potential dodo runtime failues), run

```
$ dodo check -v
```

To see the above, plus info (dependence edges), run

```
$ dodo check -vv
```

To see the above, plus logs (changes), run

```
$ dodo check -vvv
```

