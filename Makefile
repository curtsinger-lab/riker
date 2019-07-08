CC  = gcc
CXX = g++
COMMON_CFLAGS = -g -Wall
CXXFLAGS = $(COMMON_CFLAGS) --std=c++14
LDFLAGS = -lcapnp -lkj

all: dodo dodo-visualize dodo-dryrun

.PHONY: all

.SUFFIXES:

dodo: objs/db.capnp.o objs/trace.o objs/middle.o objs/graph.o
	$(CXX) $^ -o $@ $(LDFLAGS)

dodo-visualize: objs/db.capnp.o objs/graph.o objs/visualize.o
	$(CXX) $^ -o $@ $(LDFLAGS)


dodo-dryrun: objs/db.capnp.o objs/dodorun.o
	$(CXX) $^ -o $@ $(LDFLAGS)

src/%.capnp.cc src/%.capnp.h: src/%.capnp
	capnpc --output=c++ $<
	mv $(addsuffix .c++,$<) $(addsuffix .cc,$<)

objs/%.o: src/%.cc $(wildcard src/*.h) $(wildcard src/*.hh) $(wildcard src/*.capnp)
	mkdir -p objs/
	$(CXX) $(CXXFLAGS) $(filter %.cc,$^) -c -o $@
