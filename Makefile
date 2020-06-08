CC  = clang
CXX = clang++
MAKEFLAGS += -j

COMMON_CFLAGS = -Isrc -Ideps/cereal/include -Ideps/CLI11/include -Wall -g -Wfatal-errors -O3
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lstdc++fs

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, .obj/%.o, $(SRCS))
DEPS := $(patsubst src/%.cc, .obj/%.d, $(SRCS))

all: dodo dodo-launch
	
clean:
	rm -rf dodo .obj .dodo

.PHONY: all clean test selftest

.SUFFIXES:

dodo: $(OBJS)
	$(CXX) $^ -o $@ $(LDFLAGS)
	
$(OBJS): .obj/%.o: src/%.cc Makefile
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) $(filter %.cc,$^) -c -o $@

dodo-launch: launch/launch.c
	$(CC) -o $@ $^

test: dodo dodo-launch
	@echo "Running test cases"
	@rm -f tests/*/*.t.err
	@DODO="$(PWD)/dodo" cram --quiet tests/*/*.t || ( \
		echo "\nFailed tests:" && \
		for fail in `find tests | grep .t.err | xargs -L 1 dirname | xargs -L 1 basename | uniq`; do \
		  echo -n "  $$fail ("; \
			echo -n `ls tests/$$fail | grep .t.err | rev | cut -c 5- | rev`; \
			echo ")"; \
		done && echo && /bin/false)

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo

-include $(DEPS)
