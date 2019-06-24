CC  = gcc
CXX = g++
COMMON_CFLAGS = -g -Wall
CXXFLAGS = $(COMMON_CFLAGS) --std=c++14

graph: graph.o
	$(CXX) $^ -o $@

%.o: %.cc $(wildcard *.h)
	$(CXX) $(CXXFLAGS) $(filter %.cc,$^) -c -o $@
