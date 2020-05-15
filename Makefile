CC  = clang
CXX = clang++

COMMON_CFLAGS = -Isrc -Icereal/include -ICLI11/include -Wall -g -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lstdc++fs

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.[h]*")

TESTS :=
TESTS := $(TESTS) simple                  # Compile a hello world program with gcc
TESTS := $(TESTS) incremental             # Compile a hello world program to .o, then link
TESTS := $(TESTS) readonly-Dodofile       # Check if dodo correctly runs read-only Dodofiles with sh
TESTS := $(TESTS) non-sh-Dodofile         # Check if dodo correctly runs a Dodofile that is not a shell script
TESTS := $(TESTS) inaccessible-Dodofile   # Check if dodo fails with an error when Dodofile is not accessible
TESTS := $(TESTS) ABbuild-nocache         # Generate output by running a sequence of shell commands with caching disabled
TESTS := $(TESTS) ABbuild-cache           # Generate output by running a sequence of shell commands with caching enabled
TESTS := $(TESTS) graph                   # Generate and render graphviz output
TESTS := $(TESTS) stats                   # Generate build stats output

all: dodo
	
clean:
	rm -rf dodo objs .dodo

.PHONY: all clean test selftest

.SUFFIXES:

dodo: $(OBJS)
	$(CXX) $^ -o $@ $(LDFLAGS)
	
$(OBJS): objs/%.o: src/%.cc $(HEADERS) Makefile
	@mkdir -p `dirname $@`
	$(CXX) $(CXXFLAGS) $(filter %.cc,$^) -c -o $@

test: dodo
	@echo "Running test cases"
	cram $(addsuffix /*.t,$(addprefix tests/, $(TESTS)))

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo
