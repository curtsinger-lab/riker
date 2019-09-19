CC  = gcc
CXX = g++
COMMON_CFLAGS = -Wall -g -flto -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -flto -lcapnp -lkj

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile

all: dodo
	
clean:
	rm -rf dodo objs db.dodo src/*.capnp.cc src/*.capnp.h

.PHONY: all clean test selftest

.SUFFIXES:

dodo: objs/db.capnp.o objs/ptrace.o objs/middle.o objs/graphviz.o objs/blake2s-wrapper.o objs/blake2sp-wrapper.o objs/driver.o objs/dodorun.o objs/util.o objs/file.o
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
		cram  tests/$$test/*.t; \
		echo; \
	done

selftest: dodo
	@echo "Running self test"
	@rm -f db.dodo
	./dodo
