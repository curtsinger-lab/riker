CC  = clang
CXX = clang++
AR = ar

BLAKE3 := deps/BLAKE3/c

OPT = -O3
CFLAGS := $(OPT) \
					-Isrc \
					-Ideps/cereal/include \
					-Ideps/CLI11/include \
					-I$(BLAKE3) \
					-Wall \
					-Wfatal-errors
CXXFLAGS := $(CFLAGS) --std=c++17

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

$(OBJS): .obj/%.o: src/%.cc
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) -o $@ -c $<

rkr-launch: launch/launch.c
	$(CC) $(CFLAGS) -o $@ $<

$(BLAKE_OBJS):: .obj/blake3/%.o: $(BLAKE3)/%.c
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
