# Testing Instructions
This file describes the test suite for dodo, and the process for creating new test cases. If you are looking for instructions on how to run tests, go back up to the root of the project and run `make test`.

## Existing Tests

**`ABbuild`** 
Generate output with a sequence of shell commands

**`buildfile`** 
Test builds with executable and non-executable build files, as well as a build file that does not run with /bin/sh.

**`graph`** 
Test output and options for the graph subcommand

**`hello`** 
Test builds of a simple hello world C program compiled with gcc

**`readlink`** 
Test explicit dependencies on the contents of symbolic links. The test uses the destination of a link both as raw input (like file contents) and to resolve to another file.

**`stats`** 
Generate build stats output

**`symlink`** 
Make sure we are resolving paths with symlinks correctly. The test covers what happens when a symlink changes how artifacts resolve between builds.

## Writing Tests
Becasue Dodo is primarily a command-line tool, all tests are driven through the command line. Test cases use the [cram](https://pypi.org/project/cram/) tool to run commands and validate their output.

The `cram` tool has a few quirks, so here's some information on workarounds.

### Running Tests in the Right Directory
First, `cram` runs tests in a temporary directory by default. This causes problems for Dodo, since we want to run the `dodo` command from this working copy, and the test cases must be run in the directory where builds will take place. As a result, all `.t` files begin with the following lines:

```
Move to test directory
  $ cd $TESTDIR
```

The `$TESTDIR` variable will always be set to the directory that holds the `.t` file currently being executed. The first line of this header is a comment because it is not indented. The second line, which begins with two spaces and a `$` is run on the command line.

### Running Dodo
The test run will set the `DODO` environment variable to refer to the correct executable. Use this variable to invoke dodo. At some point in the future, we may want to run tests against a version of dodo built with AddressSanitizer, so having the ability to swap in a different `dodo` executable will be useful.

### Matching Commands
Currently, `dodo` prints out all commands it runs to `stdout`. This may change (at least as a default), but for now that means we can match commands. However, commands are often platform-specific, including information about the architecture, compiler version, and other details we won't expect to match across machines. The `simple` tests use `cram`'s regular expression matching to check for commands without matching full paths or argument strings. Here is a section from the `simple` test that runs `dodo` and checks to make sure it runs the `cc1` and `rm` commands:

```
Run a rebuild. We should compile to assembly, then stop.
  $ $DODO
  .*/cc1 .* (re)
  rm .*\.s (re)
```

Again, the unindented line is a comment, and the line indented by two spaces and marked with a `$` character is the command to run. Any other indented lines are treated as expected output. By default, lines must match exactly. The lines in the example above end with `(re)`, separated from the line by a space. This tells `cram` to interpret the line as a regular expression. We may eventually want to match commands more precisely than in the example above, but we should avoid being overly-restrictive by introducing platform-specific pattern rules. In general, try to match only the command file name and not its path.

### Checking Exit Status
We will expect `dodo` to fail in some test cases. To check for this, you can write cases like the following:

```
Check for error when missing Dodofile
  $ $DODO
  Unable to access Dodofile, which is required for the build.
  See http://dodo.build for instructions.
  [1]
```

The comment, command, and expected output are not new in this example. The line `[1]` indicates that `cram` should require that the command exited with code 1.

### Skipping Tests
Some tests will not work at the time they are written, but we do not want them to cause our CI checks to fail. To skip a test, simply exit with code 80. `cram` will interpret this special exit code as a skip.

```
SKIP! This test does not work.
  $ exit 80
```

Always include the text "SKIP!" in a comment on tests to make it easy to find skipped tests in the future.
