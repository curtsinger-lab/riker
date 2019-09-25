CC  = clang
CXX = clang++
COMMON_CFLAGS = -Isrc -Wall -g -flto -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lcapnp -lkj -flto

DB := src/db/db.capnp
SRCS := $(DB).cc $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(DB).h $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.[h]*")

IWYU := iwyu 
IWYUFLAGS := -Xiwyu --mapping_file=.iwyu-mappings \
             -Isrc \
             -isystem /usr/include/c++/8/ \
             -isystem /usr/lib/llvm-8/lib/clang/8.0.0/include/
IWYU_SKIP := $(DB)% src/fingerprint/%
IWYU_SRCS := $(filter-out $(IWYU_SKIP), $(SRCS))

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile

all: dodo
	
clean:
	rm -rf dodo objs db.dodo $(DB).cc $(DB).h

.PHONY: all clean iwyu test selftest

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

iwyu:
	@for src in $(IWYU_SRCS); do \
		$(IWYU) $(IWYUFLAGS) $(CXXFLAGS) $$src; \
	done || true
