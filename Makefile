CC  = gcc
CXX = g++
COMMON_CFLAGS = -Wall -g -flto
CXXFLAGS = $(COMMON_CFLAGS) --std=c++14
LDFLAGS = -lcapnp -lkj -flto

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile

all: dodo

.PHONY: all test

.SUFFIXES:

dodo: objs/db.capnp.o objs/trace.o objs/middle.o objs/graph.o objs/fingerprint.o objs/blake2s-wrapper.o objs/blake2sp-wrapper.o objs/driver.o objs/dodorun.o
	$(CXX) $^ -o $@ $(LDFLAGS)

.submodules-cleared:
	git submodule deinit --all --force
	touch $@

.submodules-updated: .submodules-cleared
	git submodule update --init
	touch $@

src/%.capnp.cc src/%.capnp.h: src/%.capnp
	capnpc --output=c++ $<
	mv $(addsuffix .c++,$<) $(addsuffix .cc,$<)

objs/%.o: src/%.cc $(wildcard src/*.h) $(wildcard src/*.hh) $(wildcard src/*.capnp) #.submodules-updated
	mkdir -p objs/
	$(CXX) $(CXXFLAGS) $(filter %.cc,$^) -c -o $@

test: dodo
	@echo "Running test cases"
	@for test in $(TESTS); do \
		echo "[$$test]"; \
		cram tests/$$test/*.t; \
		echo; \
	done
