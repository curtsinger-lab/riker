CC  = clang
CXX = clang++

COMMON_CFLAGS = -Isrc -Icereal/include -Wall -g -flto -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -flto -lstdc++fs

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.[h]*")

IWYU := iwyu 
IWYUFLAGS := -Xiwyu --mapping_file=.iwyu-mappings \
             -Isrc \
             -isystem /usr/include/c++/8/ \
             -isystem /usr/lib/llvm-8/lib/clang/8.0.0/include/
IWYU_SKIP := src/fingerprint/%
IWYU_SRCS := $(filter-out $(IWYU_SKIP), $(SRCS))

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile ABbuild

all: dodo
	
clean:
	rm -rf dodo objs .dodo.db

.PHONY: all clean iwyu test selftest

.SUFFIXES:

dodo: $(OBJS)
	$(CXX) $^ -o $@ $(LDFLAGS)
	
$(OBJS): objs/%.o: src/%.cc $(HEADERS)
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
	@rm -f .dodo.db
	./dodo

iwyu:
	@for src in $(IWYU_SRCS); do \
		$(IWYU) $(IWYUFLAGS) $(CXXFLAGS) $$src; \
	done || true
