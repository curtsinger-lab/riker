CC  = gcc
CXX = g++
COMMON_CFLAGS = -Isrc -Wall -g -flto -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -flto -lcapnp -lkj

DB := src/db/db.capnp
SRCS := $(DB).cc $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(DB).h $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.h+")

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile

all: dodo
	
clean:
	rm -rf dodo objs db.dodo $(DB).cc $(DB).h

.PHONY: all clean test selftest

.SUFFIXES:

dodo: $(OBJS)
	$(CXX) $^ -o $@ $(LDFLAGS)

src/%.capnp.cc src/%.capnp.h: src/%.capnp
	capnpc --output=c++ $<
	mv $(addsuffix .c++, $<) $(addsuffix .cc, $<)

$(OBJS): objs/%.o: src/%.cc $(HEADERS) $(wildcard src/*.capnp)
	@mkdir -p `dirname $@`
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
