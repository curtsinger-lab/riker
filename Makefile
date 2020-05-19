CC  = clang
CXX = clang++

COMMON_CFLAGS = -Isrc -Icereal/include -ICLI11/include -Wall -g -Wfatal-errors
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lstdc++fs

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, objs/%.o, $(SRCS))
HEADERS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.[h]*")

TESTS := $(shell find tests -type d)

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
	@cram --quiet tests/*/*.t || ( \
		echo "Failed tests:" && \
		for fail in `find tests | grep .t.err | xargs dirname | xargs basename | uniq`; do \
		  echo -n "  $$fail ("; \
			echo -n `ls tests/$$fail | grep .t.err | rev | cut -c5- | rev`; \
			echo ")"; \
		done && echo && /bin/false)

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo
