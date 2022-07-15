CC  = clang
CXX = clang++
AR = ar
MAKEFLAGS += -j$(shell ls /sys/devices/system/cpu | grep -E cpu\[0-9\]+ | wc -l)
ARCH := $(shell uname -p)

# Install prefix
PREFIX ?= /usr

BLAKE3 := deps/BLAKE3/c

# Flags shared by both debug and release builds
COMMON_CFLAGS := -Wall -Wfatal-errors -Isrc/common -Isrc/rkr -I$(BLAKE3)
COMMON_CXXFLAGS := $(COMMON_CFLAGS) --std=c++17 -Ideps/CLI11/include
COMMON_LDFLAGS := -lstdc++fs -lfmt -lpthread

# Debug settings
DEBUG_DIR := debug
DEBUG_CFLAGS := -O3 -g -fstandalone-debug $(COMMON_CFLAGS) -I$(DEBUG_DIR)
DEBUG_CXXFLAGS := -O3 -g -fstandalone-debug $(COMMON_CXXFLAGS) -I$(DEBUG_DIR)
DEBUG_LDFLAGS := $(COMMON_LDFLAGS)

# Release settings
RELEASE_DIR := release
RELEASE_CFLAGS := -DNDEBUG -O3 -flto $(COMMON_CFLAGS) -I$(RELEASE_DIR)
RELEASE_CXXFLAGS := -DNDEBUG -O3 -flto $(COMMON_CXXFLAGS) -I$(RELEASE_DIR)
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

# Set up sources for the injected library
RKR_INJECT_SRCS := $(wildcard src/inject/*.c)

# Architecture-specific options for injected library
ifeq ($(ARCH),x86_64)
RKR_INJECT_SRCS := $(RKR_INJECT_SRCS) src/inject/syscall-amd64.s
else ifeq ($(ARCH),aarch64)
RKR_INJECT_SRCS := $(RKR_INJECT_SRCS) src/inject/syscall-arm64.s
endif

# Set up BLAKE3 source files
BLAKE_C_SRCS := $(BLAKE3)/blake3.c \
						 	  $(BLAKE3)/blake3_dispatch.c \
						 	  $(BLAKE3)/blake3_portable.c
BLAKE_S_SRCS :=

# Architecture-specific BLAKE3 options
ifeq ($(ARCH),x86_64)
BLAKE_S_SRCS := $(BLAKE_S_SRCS) \
							  $(BLAKE3)/blake3_sse2_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_sse41_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_avx2_x86-64_unix.S \
						 	  $(BLAKE3)/blake3_avx512_x86-64_unix.S
else ifeq ($(ARCH),aarch64)
BLAKE_C_SRCS := $(BLAKE_C_SRCS) $(BLAKE3)/blake3_neon.c
endif

BLAKE_DEBUG_C_OBJS := $(patsubst $(BLAKE3)/%.c, $(DEBUG_DIR)/.obj/blake3/%.o, $(BLAKE_C_SRCS))
BLAKE_RELEASE_C_OBJS := $(patsubst $(BLAKE3)/%.c, $(RELEASE_DIR)/.obj/blake3/%.o, $(BLAKE_C_SRCS))

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

install: install-debug

install-debug:
	@echo Installing debug build to prefix $(PREFIX)...
	@(install -d $(PREFIX)/bin $(PREFIX)/share/rkr/wrappers && \
		install $(DEBUG_DIR)/bin/rkr $(DEBUG_DIR)/bin/rkr-launch $(PREFIX)/bin && \
		install $(DEBUG_DIR)/share/rkr/rkr-inject.so $(DEBUG_DIR)/share/rkr/rkr-wrapper $(PREFIX)/share/rkr/ && \
		install $(DEBUG_WRAPPERS) $(PREFIX)/share/rkr/wrappers && \
		echo Done. && \
		echo CAUTION: Riker is alpha-quality software. Please report bugs at https://rkr.sh.) || \
		(echo && \
		echo "ERROR: Failed to install debug build. Have you run \`make debug\`?" && \
		exit 0)

install-release:
	@echo Installing release build to prefix $(PREFIX)... 
	@(install -d $(PREFIX)/bin $(PREFIX)/share/rkr/wrappers && \
		install $(RELEASE_DIR)/bin/rkr $(RELEASE_DIR)/bin/rkr-launch $(PREFIX)/bin && \
		install $(RELEASE_DIR)/share/rkr/rkr-inject.so $(RELEASE_DIR)/share/rkr/rkr-wrapper $(PREFIX)/share/rkr/ && \
		install $(RELEASE_WRAPPERS) $(PREFIX)/share/rkr/wrappers && \
		echo Done. && \
		echo CAUTION: Riker is alpha-quality software. Please report bugs at https://rkr.sh.) || \
		(echo && \
		echo "ERROR: Failed to install release build. Have you run \`make release\`?" && \
		exit 0)

uninstall:
	@echo Removing installed version under prefix $(PREFIX)
	@rm -rf $(PREFIX)/bin/rkr $(PREFIX)/bin/rkr-launch $(PREFIX)/share/rkr

clean: clean-debug clean-release

clean-debug:
	rm -rf $(DEBUG_DIR)

clean-release:
	rm -rf $(RELEASE_DIR)

test: test-debug

test-debug: debug
	@PATH=$(PWD)/$(DEBUG_DIR)/bin:$(PATH) \
		LD_LIBRARY_PATH=$(PWD)/$(DEBUG_DIR)/lib:$(LD_LIBRARY_PATH) \
		scripts/runtests.py

test-release: release
	@PATH=$(PWD)/$(RELEASE_DIR)/bin:$(PATH) \
		LD_LIBRARY_PATH=$(PWD)/$(RELEASE_DIR)/lib:$(LD_LIBRARY_PATH) \
		scripts/runtests.py

test-installed:
	scripts/runtests.py

$(DEBUG_DIR)/bin/rkr: $(RKR_DEBUG_OBJS) $(BLAKE_DEBUG_C_OBJS) $(BLAKE_DEBUG_S_OBJS)
$(RELEASE_DIR)/bin/rkr: $(RKR_RELEASE_OBJS) $(BLAKE_RELEASE_C_OBJS) $(BLAKE_RELEASE_S_OBJS)
$(DEBUG_DIR)/bin/rkr $(RELEASE_DIR)/bin/rkr:
	@mkdir -p `dirname $@`
	$(CXX) $^ -o $@ $(LDFLAGS)

$(DEBUG_DIR)/platform-config.h: $(DEBUG_DIR)/bin/platform-config
$(RELEASE_DIR)/platform-config.h: $(RELEASE_DIR)/bin/platform-config
$(DEBUG_DIR)/platform-config.h $(RELEASE_DIR)/platform-config.h:
	$< > $@

$(DEBUG_DIR)/bin/platform-config $(RELEASE_DIR)/bin/platform-config: src/platform-config/platform-config.cc
	@mkdir -p `dirname $@`
	$(CXX) $(CXXFLAGS) -o $@ $<

$(RKR_DEBUG_OBJS): $(DEBUG_DIR)/.obj/%.o: src/%.cc Makefile $(DEBUG_DIR)/platform-config.h
$(RKR_RELEASE_OBJS): $(RELEASE_DIR)/.obj/%.o: src/%.cc Makefile $(RELEASE_DIR)/platform-config.h
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

.PHONY: all debug release install install-debug install-release uninstall clean clean-debug clean-release test test-debug test-release test-installed

.SUFFIXES:
