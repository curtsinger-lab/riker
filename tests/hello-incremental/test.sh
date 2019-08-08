#!/bin/sh

# Set up
cp hello-original.c hello.c

echo "Prepare and run the seed build"
cp hello-original.c hello.c
../../dodo ./build.sh

echo

echo "Rebuild with no changes"
echo "TODO: verify that this runs no commands"
../../dodo-build

echo

echo "Change the source file to add a comment, then rebuild"
echo "TODO: verify that this runs cc1, but nothing else"
cp hello-comment.c hello.c
../../dodo-build

echo

echo "Restore code and rebuild"
echo "TODO: verify that this runs cc1, but not as or collect2"
cp hello-original.c hello.c
../../dodo-build

echo

echo "Change the source file to modify a string, then rebuild"
echo "TODO: verify that this runs cc1, as, and collect2"
cp hello-string.c hello.c
../../dodo-build

echo

echo "Restore code"
cp hello-original.c hello.c

