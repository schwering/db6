#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>

#ifndef O_NOATIME
  #define O_NOATIME 	0
#endif

#ifndef WIN32
  #define O_BINARY 	0
#endif

#ifndef O_DIRECT
  //#error O_DIRECT not available
  #define O_DIRECT	0
#endif

/*
#if defined(DB_DIRECT_IO) && !defined(O_DIRECT)
  #error Cannot surpass systems cache because O_DIRECT is not available.
#endif

#ifdef DB_DIRECT_IO
  #define DIRECT_IO_FLAG	O_DIRECT
#else
  #define DIRECT_IO_FLAG	0
#endif
*/


/* second argument for open() */
#define OPEN_RO_FLAGS		(O_NOATIME | O_BINARY)
#define OPEN_RW_FLAGS		(OPEN_RO_FLAGS | O_RDWR)
#define CREATE_FLAGS		(OPEN_RW_FLAGS | O_CREAT | O_TRUNC | O_EXCL)
const int db_io_low_level_open_ro_flags = OPEN_RO_FLAGS;
const int db_io_low_level_open_rw_flags = OPEN_RW_FLAGS;
const int db_io_low_level_create_flags = CREATE_FLAGS;


/* second arguments for open(), direct IO */
#define OPEN_RO_DIRECT_FLAGS	(O_DIRECT | OPEN_RO_FLAGS)
#define OPEN_RW_DIRECT_FLAGS	(O_DIRECT | OPEN_RW_FLAGS)
#define CREATE_DIRECT_FLAGS	(O_DIRECT | CREATE_FLAGS)
const int db_io_low_level_open_ro_direct_flags = OPEN_RO_DIRECT_FLAGS;
const int db_io_low_level_open_rw_direct_flags = OPEN_RW_DIRECT_FLAGS;
const int db_io_low_level_create_direct_flags = CREATE_DIRECT_FLAGS;


/* third argument for open() */
#define FILE_MODE		(S_IRUSR | S_IWUSR)
const int db_io_low_level_file_mode = FILE_MODE;


/* second argument type for lseek64() and lseek64 itself */
#ifndef HAVE_OFF64_T
  typedef off_t off64_t;
  #define lseek64 lseek
#endif


/* third arguments for lseek64() */
#ifndef SEEK_SET
  #define SEEK_SET	0
  #define SEEK_CUR	1
  #define SEEK_END	2
#endif
const int db_io_low_level_seek_set = SEEK_SET;
const int db_io_low_level_seek_cur = SEEK_CUR;
const int db_io_low_level_seek_end = SEEK_END;


inline int db_io_low_level_open(char *path, int flags, int mode)
{
	return open(path, flags, mode);
}

inline int db_io_low_level_unlink(char *path)
{
	return unlink(path);
}

inline int db_io_low_level_close(int fd)
{
	return close(fd);
}

inline int db_io_low_level_lock(int fd, int excl, int wait)
{
#ifndef WIN32
	/*int cmd;
	struct flock lock;

	cmd = wait ? F_SETLKW : F_SETLK;
	lock.l_type = excl ? F_WRLCK : F_RDLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = 0;
	lock.l_len = 0;
	lock.l_pid = getpid();
	return fcntl(fd, cmd, &lock);*/
	return flock(fd, (excl ? LOCK_EX : LOCK_SH) + (wait ? 0 : LOCK_NB));
#else
	return 0;
#endif
}

inline int db_io_low_level_unlock(int fd)
{
#ifndef WIN32
	/*int cmd;
	struct flock lock;

	cmd = F_SETLK;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = 0;
	lock.l_len = 0;
	lock.l_pid = getpid();
	return fcntl(fd, cmd, &lock);*/
	return flock(fd, LOCK_UN);
#else
	return 0;
#endif
}

inline off64_t db_io_low_level_lseek(int fd, off64_t offset, int whence)
{
	return lseek64(fd, offset, whence);
}

inline off64_t db_io_low_level_size(int fd)
{
	struct stat st;

	if (fstat(fd, &st) == -1)
		return -1;
	return st.st_size;
}

inline ssize_t db_io_low_level_read(int fd, void *buf, size_t nbytes)
{
	return read(fd, buf, nbytes);
}

inline ssize_t db_io_low_level_pread(int fd, void *buf, size_t nbytes,
		off64_t offset)
{
#ifndef HAVE_PREAD
	db_io_low_level_lseek(fd, offset, SEEK_SET);
	return read(fd, buf, nbytes);
#else
	return pread(fd, buf, nbytes, offset);
#endif
}

inline ssize_t db_io_low_level_write(int fd, const void *buf, size_t nbytes)
{
	return write(fd, buf, nbytes);
}

inline ssize_t db_io_low_level_pwrite(int fd, const void *buf, size_t nbytes,
		off64_t offset)
{
#ifndef HAVE_PWRITE
	db_io_low_level_lseek(fd, offset, SEEK_SET);
	return write(fd, buf, nbytes);
#else
	return pwrite(fd, buf, nbytes, offset);
#endif
}

inline ssize_t db_io_low_level_read_direct(int fd, void *buf, size_t nbytes)
{
#ifndef HAVE_POSIX_MEMALIGN
	return -1;
#else
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	retval = read(fd, abuf, nbytes);
	memcpy(buf, abuf, retval);
	free(abuf);
	return retval;
#endif
}

inline ssize_t db_io_low_level_write_direct(int fd, const void *buf,
		size_t nbytes)
{
#ifndef HAVE_POSIX_MEMALIGN
	return -1;
#else
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	memcpy(abuf, buf, nbytes);
	retval = write(fd, abuf, nbytes);
	free(abuf);
	return retval;
#endif
}

inline int db_io_low_level_errno(void)
{
#ifndef WIN32
	return errno;
#else
	return 0;
#endif
}

