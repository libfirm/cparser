/* make sure we have some posix functions available. Include this file first! */

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>

/* no eXecute on Win32 */
#define X_OK 0
#define W_OK 2
#define R_OK 4

#define O_RDWR          _O_RDWR
#define O_CREAT         _O_CREAT
#define O_EXCL          _O_EXCL
#define O_BINARY        _O_BINARY

/* remap some names, we are not in the POSIX world */
#define access(fname, mode)      _access(fname, mode)
#define fdopen(fd, mode)         _fdopen(fd, mode)
#define isatty(fd)               _isatty(fd)
#define mktemp(tmpl)             _mktemp(tmpl)
#define open(fname, oflag, mode) _open(fname, oflag, mode)
#define pclose(file)             _pclose(file)
#define popen(cmd, mode)         _popen(cmd, mode)
#define unlink(filename)         _unlink(filename)

#else
#define _POSIX_C_SOURCE 2000112L
#include <unistd.h>
#define HAVE_MKSTEMP
#endif
