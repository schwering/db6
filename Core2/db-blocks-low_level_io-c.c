#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include <pthread.h>

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
const int db_blocks_low_level_io_open_ro_flags = OPEN_RO_FLAGS;
const int db_blocks_low_level_io_open_rw_flags = OPEN_RW_FLAGS;
const int db_blocks_low_level_io_create_flags = CREATE_FLAGS;


/* second arguments for open(), direct IO */
#define OPEN_RO_DIRECT_FLAGS	(O_DIRECT | OPEN_RO_FLAGS)
#define OPEN_RW_DIRECT_FLAGS	(O_DIRECT | OPEN_RW_FLAGS)
#define CREATE_DIRECT_FLAGS	(O_DIRECT | CREATE_FLAGS)
const int db_blocks_low_level_io_open_ro_direct_flags = OPEN_RO_DIRECT_FLAGS;
const int db_blocks_low_level_io_open_rw_direct_flags = OPEN_RW_DIRECT_FLAGS;
const int db_blocks_low_level_io_create_direct_flags = CREATE_DIRECT_FLAGS;


/* third argument for open() */
#define FILE_MODE		(S_IRUSR | S_IWUSR)
const int db_blocks_low_level_io_file_mode = FILE_MODE;


/* second argument type for lseek64() and lseek64 itself */
#ifndef HAVE_OFF64_T
  typedef off_t off64_t;
  #define lseek64 lseek
#endif

static pthread_spinlock_t spinlock;
static bool spinlock_initialized = false;

inline int db_blocks_low_level_io_open(char *path, int flags, int mode)
{
	if (!spinlock_initialized) {
		pthread_spin_init(&spinlock, 0);
		spinlock_initialized = true;
	}
	return open(path, flags, mode);
}

inline int db_blocks_low_level_io_unlink(char *path)
{
	return unlink(path);
}

inline int db_blocks_low_level_io_close(int fd)
{
	int retval;
	pthread_spin_lock(&spinlock);
	retval = close(fd);
	pthread_spin_unlock(&spinlock);
	return retval;
}

inline ssize_t db_blocks_low_level_io_pread(int fd, void *buf, size_t nbytes,
		off64_t offset)
{
	ssize_t retval;
	pthread_spin_lock(&spinlock);
	retval = pread(fd, buf, nbytes, offset);
	pthread_spin_unlock(&spinlock);
	return retval;
}

inline ssize_t db_blocks_low_level_io_pwrite(int fd, const void *buf,
		size_t nbytes, off64_t offset)
{
	ssize_t retval;
	pthread_spin_lock(&spinlock);
	retval = pwrite(fd, buf, nbytes, offset);
	pthread_spin_unlock(&spinlock);
	return retval;
}

static int try_lock(int fd, off64_t offset, size_t len)
{
	int cmd;
	struct flock lock;

	cmd = F_SETLK;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = offset;
	lock.l_len = len;
	//lock.l_pid = getpid();
	return fcntl(fd, cmd, &lock) != -1;
}

static inline int lock(int fd, off64_t offset, size_t len)
{
	int cmd;
	struct flock lock;

	cmd = F_SETLKW;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = offset;
	lock.l_len = len;
	//lock.l_pid = getpid();
	return fcntl(fd, cmd, &lock) != -1;
}

static inline int unlock(int fd, off64_t offset, size_t len)
{
	int cmd;
	struct flock lock;

	cmd = F_SETLK;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	lock.l_start = offset;
	lock.l_len = len;
	//lock.l_pid = getpid();
	return fcntl(fd, cmd, &lock) != -1;
}

inline off64_t db_blocks_low_level_io_alloc(int fd, size_t nbytes)
{
	off64_t offset;
	void *zeros;

	zeros = malloc(nbytes);
	memset(zeros, 0, nbytes);
retry:
	offset = lseek64(fd, 0, SEEK_END);
	if (offset == (off64_t)-1)
		return (off64_t)-1;
	if (!try_lock(fd, offset, nbytes))
		goto retry;
	if (lseek64(fd, 0, SEEK_END) != offset) {
		unlock(fd, offset, nbytes);
		goto retry;
	}
	pwrite(fd, zeros, nbytes, offset);
	free(zeros);
	unlock(fd, offset, nbytes);
	return offset;
}

inline off64_t db_blocks_low_level_io_seek_end(int fd)
{
	return lseek64(fd, 0, SEEK_END);
}

inline int db_blocks_low_level_io_lock(int fd, off64_t offset, size_t len)
{
	return lock(fd, offset, len);
}

inline int db_blocks_low_level_io_unlock(int fd, off64_t offset, size_t len)
{
	return unlock(fd, offset, len);
}

inline int db_blocks_low_level_io_errno(void)
{
#ifndef WIN32
	return errno;
#else
	return 0;
#endif
}

