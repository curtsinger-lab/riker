CC  = clang
CXX = clang++
AR = ar
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)

BLAKE3 := deps/BLAKE3/c

# Flags shared by both debug and release builds
COMMON_CFLAGS := -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I$(BLAKE3)
COMMON_CXXFLAGS := $(COMMON_CFLAGS) --std=c++17 -Ideps/CLI11/include
COMMON_LDFLAGS := -lstdc++fs -lfmt -lpthread

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

# Set up variables used for the build
RKR_SRCS := $(wildcard src/rkr/*/*.cc)
RKR_DEBUG_OBJS := $(patsubst src/%.cc, $(DEBUG_DIR)/.obj/%.o, $(RKR_SRCS))
RKR_DEBUG_DEPS := $(patsubst src/%.cc, $(DEBUG_DIR)/.obj/%.d, $(RKR_SRCS))
RKR_RELEASE_OBJS := $(patsubst src/%.cc, $(RELEASE_DIR)/.obj/%.o, $(RKR_SRCS))
RKR_RELEASE_DEPS := $(patsubst src/%.cc, $(RELEASE_DIR)/.obj/%.d, $(RKR_SRCS))

# Create parallel compiler wrappers with the following names
WRAPPER_NAMES := clang clang++ gcc g++ cc c++
DEBUG_WRAPPERS := $(addprefix $(DEBUG_DIR)/share/rkr/wrappers/, $(WRAPPER_NAMES))
RELEASE_WRAPPERS := $(addprefix $(RELEASE_DIR)/share/rkr/wrappers/, $(WRAPPER_NAMES))

# TODO: Don't hard-code the architecture-specific assembly file
RKR_INJECT_SRCS := $(wildcard src/inject/*.c) src/inject/syscall-amd64.s

BLAKE_C_SRCS := $(BLAKE3)/blake3.c \
						 	  $(BLAKE3)/blake3_dispatch.c \
						 	  $(BLAKE3)/blake3_portable.c
BLAKE_DEBUG_C_OBJS := $(patsubst $(BLAKE3)/%.c, $(DEBUG_DIR)/.obj/blake3/%.o, $(BLAKE_C_SRCS))
BLAKE_RELEASE_C_OBJS := $(patsubst $(BLAKE3)/%.c, $(RELEASE_DIR)/.obj/blake3/%.o, $(BLAKE_C_SRCS))

BLAKE_S_SRCS :=	$(BLAKE3)/blake3_sse2_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_sse41_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_avx2_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_avx512_x86-64_unix.S
BLAKE_DEBUG_S_OBJS := $(patsubst $(BLAKE3)/%.S, $(DEBUG_DIR)/.obj/blake3/%.o, $(BLAKE_S_SRCS))
BLAKE_RELEASE_S_OBJS := $(patsubst $(BLAKE3)/%.S, $(RELEASE_DIR)/.obj/blake3/%.o, $(BLAKE_S_SRCS))

######## Begin Make Targets ########

all: debug

debug: CFLAGS = $(DEBUG_CFLAGS)
debug: CXXFLAGS = $(DEBUG_CXXFLAGS)
debug: LDFLAGS = $(DEBUG_LDFLAGS)
debug: $(DEBUG_DIR)/bin/rkr \
			 $(DEBUG_DIR)/bin/rkr-launch \
			 $(DEBUG_DIR)/share/rkr/rkr-inject.so \
			 $(DEBUG_WRAPPERS)

release: CFLAGS = $(RELEASE_CFLAGS)
release: CXXFLAGS = $(RELEASE_CXXFLAGS)
release: LDFLAGS = $(RELEASE_LDFLAGS)
release: $(RELEASE_DIR)/bin/rkr \
				 $(RELEASE_DIR)/bin/rkr-launch \
				 $(RELEASE_DIR)/share/rkr/rkr-inject.so \
				 $(RELEASE_WRAPPERS)

clean: clean-debug clean-release

clean-debug:
	rm -rf $(DEBUG_DIR)

clean-release:
	rm -rf $(RELEASE_DIR)

test: test-debug

test-debug: debug
	@PATH=$(PWD)/$(DEBUG_DIR)/bin:$(PATH) \
		LD_LIBRARY_PATH=$(PWD)/$(DEBUG_DIR)/lib:$(LD_LIBRARY_PATH) \
		./runtests.py

test-release: release
	@PATH=$(PWD)/$(RELEASE_DIR)/bin:$(PATH) \
		LD_LIBRARY_PATH=$(PWD)/$(RELEASE_DIR)/lib:$(LD_LIBRARY_PATH) \
		./runtests.py

$(DEBUG_DIR)/bin/rkr: $(RKR_DEBUG_OBJS) $(BLAKE_DEBUG_C_OBJS) $(BLAKE_DEBUG_S_OBJS)
$(RELEASE_DIR)/bin/rkr: $(RKR_RELEASE_OBJS) $(BLAKE_RELEASE_C_OBJS) $(BLAKE_RELEASE_S_OBJS)
$(DEBUG_DIR)/bin/rkr $(RELEASE_DIR)/bin/rkr:
	@mkdir -p `dirname $@`
	$(CXX) $^ -o $@ $(LDFLAGS)

$(RKR_DEBUG_OBJS): $(DEBUG_DIR)/.obj/%.o: src/%.cc Makefile
$(RKR_RELEASE_OBJS): $(RELEASE_DIR)/.obj/%.o: src/%.cc Makefile
$(RKR_DEBUG_OBJS) $(RKR_RELEASE_OBJS):
	@mkdir -p `dirname $@`
	$(CXX) -MMD -MP $(CXXFLAGS) -o $@ -c $<

$(DEBUG_DIR)/bin/rkr-launch $(RELEASE_DIR)/bin/rkr-launch: src/rkr-launch/launch.c Makefile
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -o $@ $<

$(DEBUG_DIR)/share/rkr/rkr-inject.so $(RELEASE_DIR)/share/rkr/rkr-inject.so: $(RKR_INJECT_SRCS) src/rkr/tracing/inject.h Makefile
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -fPIC -shared -Isrc/ -o $@ $(RKR_INJECT_SRCS) -ldl -lpthread

$(DEBUG_DIR)/share/rkr/rkr-wrapper $(RELEASE_DIR)/share/rkr/rkr-wrapper: src/wrapper/wrapper.cc Makefile
	@mkdir -p `dirname $@`
	$(CXX) $(CXXFLAGS) -o $@ src/wrapper/wrapper.cc -ldl

$(DEBUG_WRAPPERS): $(DEBUG_DIR)/share/rkr/rkr-wrapper
	@mkdir -p `dirname $@`
	@rm -f $@
	ln $(DEBUG_DIR)/share/rkr/rkr-wrapper $@

$(RELEASE_WRAPPERS): $(RELEASE_DIR)/share/rkr/rkr-wrapper
	@mkdir -p `dirname $@`
	@rm -f $@
	ln $(RELEASE_DIR)/share/rkr/rkr-wrapper $@

$(BLAKE_DEBUG_C_OBJS): $(DEBUG_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.c Makefile
$(BLAKE_DEBUG_S_OBJS): $(DEBUG_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.S Makefile
$(BLAKE_RELEASE_C_OBJS): $(RELEASE_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.c Makefile
$(BLAKE_RELEASE_S_OBJS): $(RELEASE_DIR)/.obj/blake3/%.o: $(BLAKE3)/%.S Makefile
$(BLAKE_DEBUG_C_OBJS) $(BLAKE_DEBUG_S_OBJS) $(BLAKE_RELEASE_C_OBJS) $(BLAKE_RELEASE_S_OBJS):
	@mkdir -p `dirname $@`
	$(CC) $(CFLAGS) -o $@ -c $<

-include $(RKR_DEBUG_DEPS)
-include $(RKR_RELEASE_DEPS)

.PHONY: all debug release clean clean-debug clean-release test test-debug test-release

.SUFFIXES:
