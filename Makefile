CC  = clang
CXX = clang++
AR = ar
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)

OPT = -O3
BLAKESRCDIR = deps/BLAKE3/c
BLAKEOBJDIR = .obj/deps/blake3
COMMON_CFLAGS = -Isrc \
				-Ideps/cereal/include \
				-Ideps/CLI11/include \
				-I$(BLAKESRCDIR) \
				-Wall \
				-g \
				-Wfatal-errors \
				$(OPT) \
				-fstandalone-debug
CXXFLAGS = $(COMMON_CFLAGS) --std=c++17
LDFLAGS = -lstdc++fs -lfmt -L$(BLAKEOBJDIR) -lblake3

SRCS := $(shell find src -type f -regextype sed -regex "src/[a-zA-Z0-9/]*\.cc")
OBJS := $(patsubst src/%.cc, .obj/%.o, $(SRCS))
DEPS := $(patsubst src/%.cc, .obj/%.d, $(SRCS))

all: dodo dodo-launch

clean:
	rm -rf dodo dodo-launch .obj .dodo

.PHONY: all clean test selftest

.SUFFIXES:

dodo: $(OBJS) $(BLAKEOBJDIR)/libblake3.a
	$(CXX) $^ -o $@ $(LDFLAGS)

$(OBJS): .obj/%.o: src/%.cc Makefile
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) $(filter %.cc,$^) -c -o $@

dodo-launch: launch/launch.c
	$(CC) -o $@ $^

$(BLAKEOBJDIR)/libblake3.a: $(BLAKEOBJDIR)/blake3.o \
							$(BLAKEOBJDIR)/blake3_dispatch.o \
						 	$(BLAKEOBJDIR)/blake3_portable.o \
							$(BLAKEOBJDIR)/blake3_sse2.o \
						 	$(BLAKEOBJDIR)/blake3_sse41.o \
						 	$(BLAKEOBJDIR)/blake3_avx2.o \
						 	$(BLAKEOBJDIR)/blake3_avx512.o
	$(AR) rc $@ $^

$(BLAKEOBJDIR)/blake3.o: $(BLAKESRCDIR)/blake3.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -o $@

$(BLAKEOBJDIR)/blake3_dispatch.o: $(BLAKESRCDIR)/blake3_dispatch.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -msse2 -o $@

$(BLAKEOBJDIR)/blake3_portable.o: $(BLAKESRCDIR)/blake3_portable.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -msse2 -o $@

$(BLAKEOBJDIR)/blake3_sse2.o: $(BLAKESRCDIR)/blake3_sse2.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -msse2 -o $@

$(BLAKEOBJDIR)/blake3_sse41.o: $(BLAKESRCDIR)/blake3_sse41.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -msse4.1 -o $@

$(BLAKEOBJDIR)/blake3_avx2.o: $(BLAKESRCDIR)/blake3_avx2.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -mavx2 -o $@

$(BLAKEOBJDIR)/blake3_avx512.o: $(BLAKESRCDIR)/blake3_avx512.c
	@mkdir -p `dirname $@`
	$(CC) $(COMMON_CFLAGS) -c $^ -mavx512f -mavx512vl -o $@

test: dodo dodo-launch
	@./runtests.py

selftest: dodo
	@echo "Running self test"
	@rm -f .dodo
	./dodo

-include $(DEPS)
