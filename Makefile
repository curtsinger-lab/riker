CC  = clang
CXX = clang++
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)

COMMON_CFLAGS = -Isrc -Ideps/cereal/include -Ideps/CLI11/include -Wall -g -Wfatal-errors -O3
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lstdc++fs -lfmt

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
	@./runtests.py

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo

-include $(DEPS)
