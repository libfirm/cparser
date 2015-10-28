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
FIRM_CPPFLAGS ?= `pkg-config --cflags libfirm`
FIRM_LIBS     ?= `pkg-config --libs libfirm`
LIBFIRM_FILE =
LIBFIRM_FILE_DLL =
endif

ifeq ("$(shell uname)", "Darwin")
# See if /usr/include exists (old darwin version)
ifneq ("$(wildcard /usr/include)", "")
SYSTEM_INCLUDE_DIR ?= -DSYSTEM_INCLUDE_DIR=\"/usr/include\"
else
# Use xcrun to get the include directory of the default toolchain
SYSTEM_INCLUDE_DIR ?= -DSYSTEM_INCLUDE_DIR=\"$(shell xcrun -show-sdk-path)/usr/include\"
endif
else
# location of the system/libc headers
SYSTEM_INCLUDE_DIR   ?= -DSYSTEM_INCLUDE_DIR=\"/usr/include\"
# if APPEND_MULTILIB_DIRS is defined, then we append a directory with the
# machine triple to the system and local directory. i.e. if the target triple
# is i386-linux-gnu we append $SYSTEM_INCLUDE_DIR/i386-linux-gnu and
# $LOCAL_INCLUDE_DIR/i386-linux-gnu
MULTILIB_INCLUDE_DIR ?= -DAPPEND_MULTILIB_DIRS
# hardcoded machine triple for multiarch dir when just -m32 is given
# (may be empty)
MULTILIB_M32_TRIPLE ?= -DMULTILIB_M32_TRIPLE=\"i386-linux-gnu\"
# hardcoded machine triple for multiarch dir when just -m64 is given
# (may be empty)
MULTILIB_M64_TRIPLE ?= -DMULTILIB_M64_TRIPLE=\"x86_64-linux-gnu\"
endif

# location of additional headers (may be undefined)
LOCAL_INCLUDE_DIR    ?= -DLOCAL_INCLUDE_DIR=\"/usr/local/include\"
# location of the compiler provided headers
COMPILER_INCLUDE_DIR ?= -DCOMPILER_INCLUDE_DIR=\"$(abspath $(srcdir))/include\"
# hardcoded machine triple for the host machine (may be undefined)
HOST_TRIPLE         ?=
