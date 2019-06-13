CC  = gcc
CXX = g++
COMMON_CFLAGS = -g -Wall
CXXFLAGS = $(COMMON_CFLAGS) --std=c++14

dodo: objs/trace.o objs/middle.o
	$(CXX) $^ -o $@

objs/%.o: src/%.cc $(wildcard src/*.h)
	mkdir -p objs/
	$(CXX) $(CXXFLAGS) $(filter %.cc,$^) -c -o $@
