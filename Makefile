
all: trace

clean:
	rm -f trace

trace: trace.cc
	clang++ --std=c++17 -o trace trace.cc

