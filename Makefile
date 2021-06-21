CC  = clang
CXX = clang++
AR = ar
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)

BLAKE3 := deps/BLAKE3/c

# Flags shared by both debug and release builds
COMMON_CFLAGS := -Wall -Wfatal-errors -Isrc -I$(BLAKE3)
COMMON_CXXFLAGS := $(COMMON_CFLAGS) --std=c++17 -Ideps/cereal/include -Ideps/CLI11/include
COMMON_LDFLAGS := -lstdc++fs -lfmt

# Debug settings
DEBUG_DIR := debug
DEBUG_CFLAGS := -O3 -g -fstandalone-debug $(COMMON_CFLAGS)
DEBUG_CXXFLAGS := -O3 -g -fstandalone-debug $(COMMON_CXXFLAGS)
DEBUG_LDFLAGS := $(COMMON_LDFLAGS)

# Release settings
RELEASE_DIR := release
RELEASE_CFLAGS := -DNDEBUG -O3 -flto $(COMMON_CFLAGS)
RELEASE_CXXFLAGS := -DNDEBUG -O3 -flto $(COMMON_CXXFLAGS)
RELEASE_LDFLAGS := -O3 -flto $(COMMON_LDFLAGS)

# Set up variables used for the bild
SRCS := $(wildcard src/*/*.cc)
DEBUG_OBJS := $(patsubst src/%.cc, $(DEBUG_DIR)/.obj/%.o, $(SRCS))
DEBUG_DEPS := $(patsubst src/%.cc, $(DEBUG_DIR)/.obj/%.d, $(SRCS))
RELEASE_OBJS := $(patsubst src/%.cc, $(RELEASE_DIR)/.obj/%.o, $(SRCS))
RELEASE_DEPS := $(patsubst src/%.cc, $(RELEASE_DIR)/.obj/%.d, $(SRCS))

BLAKE_SRCS := $(BLAKE3)/blake3.c \
						 	$(BLAKE3)/blake3_dispatch.c \
						 	$(BLAKE3)/blake3_portable.c \
						 	$(BLAKE3)/blake3_sse2.c \
						 	$(BLAKE3)/blake3_sse41.c \
						 	$(BLAKE3)/blake3_avx2.c \
						 	$(BLAKE3)/blake3_avx512.c
BLAKE_DEBUG_OBJS := \
	$(patsubst $(BLAKE3)/%.c, $(DEBUG_DIR)/.obj/blake3/%.o, $(BLAKE_SRCS))
BLAKE_RELEASE_OBJS := \
	$(patsubst $(BLAKE3)/%.c, $(RELEASE_DIR)/.obj/blake3/%.o, $(BLAKE_SRCS))

######## Begin Make Targets ########

all: debug

debug: CFLAGS = $(DEBUG_CFLAGS)
debug: CXXFLAGS = $(DEBUG_CXXFLAGS)
debug: LDFLAGS = $(DEBUG_LDFLAGS)
debug: $(DEBUG_DIR)/bin/rkr \
			 $(DEBUG_DIR)/bin/rkr-launch \
			 $(DEBUG_DIR)/share/rkr/rkr-inject.so

release: CFLAGS = $(RELEASE_CFLAGS)
release: CXXFLAGS = $(RELEASE_CXXFLAGS)
release: LDFLAGS = $(RELEASE_LDFLAGS)
release: $(RELEASE_DIR)/bin/rkr \
				 $(RELEASE_DIR)/bin/rkr-launch \
				 $(RELEASE_DIR)/share/rkr/rkr-inject.so

clean: clean-debug clean-release

clean-debug:
	rm -rf $(DEBUG_DIR)

clean-release:
	rm -rf $(RELEASE_DIR)

test: test-debug

test-debug: debug
	@RKR=$(PWD)/$(DEBUG_DIR)/bin/rkr ./runtests.py

test-release: release
	@RKR=$(PWD)/$(RELEASE_DIR)/bin/rkr ./runtests.py

$(DEBUG_DIR)/bin/rkr: $(DEBUG_OBJS) $(BLAKE_DEBUG_OBJS)
$(RELEASE_DIR)/bin/rkr: $(RELEASE_OBJS) $(BLAKE_RELEASE_OBJS)
$(DEBUG_DIR)/bin/rkr $(RELEASE_DIR)/bin/rkr:
	@mkdir -p `dirname $@`
	$(CXX) $^ -o $@ $(LDFLAGS)

$(DEBUG_OBJS): $(DEBUG_DIR)/.obj/%.o: src/%.cc Makefile
$(RELEASE_OBJS): $(RELEASE_DIR)/.obj/%.o: src/%.cc Makefile
$(DEBUG_OBJS) $(RELEASE_OBJS):
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) -o $@ -c $<

$(DEBUG_DIR)/bin/rkr-launch $(RELEASE_DIR)/bin/rkr-launch: launch/launch.c Makefile
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -o $@ $<

$(DEBUG_DIR)/share/rkr/rkr-inject.so $(RELEASE_DIR)/share/rkr/rkr-inject.so: inject/inject.c Makefile
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -fPIC -shared -Isrc/ -o $@ $< -ldl

# Set file-specific flags for blake3 build
%/.obj/blake3/blake3_sse2.o: CFLAGS += -msse2
%/.obj/blake3/blake3_sse41.o: CFLAGS += -msse4.1
%/.obj/blake3/blake3_avx2.o: CFLAGS += -mavx2
%/.obj/blake3/blake3_avx512.o: CFLAGS += -mavx512f -mavx512vl

$(BLAKE_DEBUG_OBJS): $(DEBUG_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.c Makefile
$(BLAKE_RELEASE_OBJS): $(RELEASE_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.c Makefile
$(BLAKE_DEBUG_OBJS) $(BLAKE_RELEASE_OBJS):
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -o $@ -c $<

-include $(DEPS)

.PHONY: all debug release clean clean-debug clean-release test test-debug test-release

.SUFFIXES:
