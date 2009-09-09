#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>

#ifndef SEEK_SET
  #define SEEK_SET	0
  #define SEEK_CUR	1
  #define SEEK_END	2
#endif

#if defined(DB_DIRECT_IO) && !defined(O_DIRECT)
  #error Cannot surpass systems cache because O_DIRECT is not available.
#endif

#ifndef O_NOATIME
  #define O_NOATIME 	0
#endif

#ifndef WIN32
  #define O_BINARY 	0
#endif

#ifdef DB_DIRECT_IO
  #define DIRECT_IO_FLAG	O_DIRECT
#else
  #define DIRECT_IO_FLAG	0
#endif

#define OPEN_RO_FLAGS	(DIRECT_IO_FLAG | O_NOATIME | O_BINARY)
#define OPEN_RW_FLAGS	(OPEN_RO_FLAGS | O_RDWR)
#define CREATE_FLAGS	(OPEN_RW_FLAGS | O_CREAT | O_TRUNC | O_EXCL)

#define FILE_MODE		(S_IRUSR | S_IWUSR)

#ifndef HAVE_OFF64_T
  typedef off_t off64_t;
  #define lseek64 lseek
#endif

/* second argument for open() */
const int db_io_low_level_open_ro_flags = OPEN_RO_FLAGS;

/* second argument for open() */
const int db_io_low_level_open_rw_flags = OPEN_RW_FLAGS;

/* second argument for open() */
const int db_io_low_level_create_flags = CREATE_FLAGS;

/* third argument for open() */
const int db_io_low_level_file_mode = FILE_MODE;


const int db_io_low_level_seek_set = SEEK_SET;
const int db_io_low_level_seek_cur = SEEK_CUR;
const int db_io_low_level_seek_end = SEEK_END;

inline int db_io_low_level_open(char *path, int flags, int mode)
{
	return open(path, flags, mode);
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

inline off64_t db_io_low_level_lseek(int fd, off64_t pos, int whence)
{
	return lseek64(fd, pos, whence);
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
#ifdef DB_DIRECT_IO
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	retval = read(fd, abuf, nbytes);
	memcpy(buf, abuf, retval);
	free(abuf);
	return retval;
#else
	return read(fd, buf, nbytes);
#endif
}

inline ssize_t db_io_low_level_pread(int fd, void *buf, size_t nbytes,
		off64_t offset)
{
#ifdef DB_DIRECT_IO
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	retval = pread(fd, abuf, nbytes, offset);
	memcpy(buf, abuf, retval);
	free(abuf);
	return retval;
#else
	return pread(fd, buf, nbytes, offset);
#endif
}

inline ssize_t db_io_low_level_write(int fd, const void *buf, size_t nbytes)
{
#ifdef DB_DIRECT_IO
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	memcpy(abuf, buf, nbytes);
	retval = write(fd, abuf, nbytes);
	free(abuf);
	return retval;
#else
	return write(fd, buf, nbytes);
#endif
}

inline ssize_t db_io_low_level_pwrite(int fd, const void *buf, size_t nbytes,
		off64_t offset)
{
#ifdef DB_DIRECT_IO
	ssize_t retval;
	void *abuf;

	if (posix_memalign(&abuf, 512, nbytes))
		return -1;
	memcpy(abuf, buf, nbytes);
	retval = pwrite(fd, abuf, nbytes);
	free(abuf);
	return retval;
#else
	return pwrite(fd, buf, nbytes, offset);
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

