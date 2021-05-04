CC  = clang
CXX = clang++
AR = ar
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)

BLAKE3 := deps/BLAKE3/c

OPT = -O3 -flto -g
CFLAGS := $(OPT)
CXXFLAGS := -Isrc \
					 	-Ideps/cereal/include \
					 	-Ideps/CLI11/include \
					 	-I$(BLAKE3) \
					 	-Wall \
					 	-Wfatal-errors \
					 	$(OPT) \
						--std=c++17 \
						-DNDEBUG

LDFLAGS = $(OPT) -lstdc++fs -lfmt

SRCS := $(wildcard src/*/*.cc)
OBJS := $(patsubst src/%.cc, .obj/%.o, $(SRCS))
DEPS := $(patsubst src/%.cc, .obj/%.d, $(SRCS))

BLAKE_SRCS := $(BLAKE3)/blake3.c \
						 	$(BLAKE3)/blake3_dispatch.c \
						 	$(BLAKE3)/blake3_portable.c \
						 	$(BLAKE3)/blake3_sse2.c \
						 	$(BLAKE3)/blake3_sse41.c \
						 	$(BLAKE3)/blake3_avx2.c \
						 	$(BLAKE3)/blake3_avx512.c
BLAKE_OBJS := $(patsubst $(BLAKE3)/%.c, .obj/blake3/%.o, $(BLAKE_SRCS))

all: rkr rkr-launch

clean:
	rm -rf rkr rkr-launch .obj .rkr

.PHONY: all clean test selftest

.SUFFIXES:

rkr: $(OBJS) $(BLAKE_OBJS)
	$(CXX) $^ -o $@ $(LDFLAGS)

$(OBJS): .obj/%.o: src/%.cc Makefile
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) -o $@ -c $<

rkr-launch: launch/launch.c Makefile
	$(CC) $(CFLAGS) -o $@ $<
	strip $@

$(BLAKE_OBJS):: .obj/blake3/%.o: $(BLAKE3)/%.c Makefile
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -o $@ -c $<

.obj/blake3/blake3_sse2.o:: CFLAGS += -msse2
.obj/blake3/blake3_sse41.o:: CFLAGS += -msse4.1
.obj/blake3/blake3_avx2.o:: CFLAGS += -mavx2
.obj/blake3/blake3_avx512.o:: CFLAGS += -mavx512f -mavx512vl

test: rkr rkr-launch
	@./runtests.py

selftest: rkr
	@echo "Running self test"
	@rm -f .rkr
	./rkr

-include $(DEPS)
