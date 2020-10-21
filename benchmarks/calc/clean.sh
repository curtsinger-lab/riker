#!/bin/sh

MAKE=/usr/bin/make

$MAKE clean
$MAKE -f Makefile.ship clobber
