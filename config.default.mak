VERSION = 1.22.1

# Use libfirm subdir if it exists, otherwise use pkg-config
ifneq ("$(wildcard $(top_srcdir)/libfirm)", "")
FIRM_HOME     ?= $(top_srcdir)/libfirm
FIRM_CPPFLAGS ?= -I$(FIRM_HOME)/include -I$(FIRM_HOME)/build/gen/include/libfirm
FIRM_LIBS     ?= -lm
LIBFIRM_FILE_BASE ?= build/$(variant)/libfirm.a
LIBFIRM_FILE  ?= $(FIRM_HOME)/$(LIBFIRM_FILE_BASE)
LIBFIRM_FILE_DLL_BASE ?= build/$(variant)/libfirm$(DLLEXT)
LIBFIRM_FILE_DLL ?= $(FIRM_HOME)/$(LIBFIRM_FILE_DLL_BASE)
else
PKG_CONFIG    ?= pkg-config
FIRM_CPPFLAGS ?= $(shell $(PKG_CONFIG) --cflags libfirm)
FIRM_LIBS     ?= $(shell $(PKG_CONFIG) --libs   libfirm)
LIBFIRM_FILE =
LIBFIRM_FILE_DLL =
endif

ifeq ("$(shell uname)", "Darwin")
# Query xcrun if /usr/include does not exist (new darwin versions)
ifeq ("$(wildcard /usr/include)", "")
SYSTEM_INCLUDE_DIR ?= "$(shell xcrun -show-sdk-path)/usr/include"
endif
endif

# location of the system/libc headers
SYSTEM_INCLUDE_DIR ?= /usr/include
# if MULTILIB_M32_TRIPLE is defined, then we append a directory with the
# machine triple to the system and local directory. i.e. if the target triple
# is i386-linux-gnu we append $SYSTEM_INCLUDE_DIR/i386-linux-gnu and
ifneq ("$(wildcard $(SYSTEM_INCLUDE_DIR)/x86_64-linux-gnu)","")
# $LOCAL_INCLUDE_DIR/i386-linux-gnu.
# -m32 triple:
MULTILIB_M32_TRIPLE ?= i386-linux-gnu
# -m64 triple:
MULTILIB_M64_TRIPLE ?= x86_64-linux-gnu
endif

# location of additional headers
LOCAL_INCLUDE_DIR ?= /usr/local/include
# location of the compiler provided headers. If PREFIX is not set we assume that
# we have a developer who wants to run cparser from source/builddir without
# installing it.
ifndef PREFIX
COMPILER_INCLUDE_DIR ?= "$(abspath $(srcdir))/include"
else
COMPILER_INCLUDE_DIR ?= $(PREFIX)/lib/cparser/$(VERSION)/include
endif
