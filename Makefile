CC  = clang
CXX = clang++
MAKEFLAGS = -j

COMMON_CFLAGS = -Isrc -Icereal/include -ICLI11/include -Wall -g -flto -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -flto -lstdc++fs

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.[h]*")

TESTS = simple incremental readonly-Dodofile non-sh-Dodofile inaccessible-Dodofile ABbuild

all: dodo
	
clean:
	rm -rf dodo objs .dodo

.PHONY: all clean test selftest

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
		cram tests/$$test/*.t; \
		echo; \
	done

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo
