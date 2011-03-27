#include <assert.h>
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
  #error O_DIRECT not available
  #define O_DIRECT	0
#endif


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


/********************
 * Internal locking *
 ********************/

/* The IO lock is used to ensure that during one read no write can happen.
 * This would lead to invalid blocks returned from the read operation.
 * For this purpose, the locks could be more fine-grained.
 * Maybe the global is even necessary because pread() and pwrite() might be
 * non-atomic operations, but I guess that they are atomic under Linux / glibc.
 */

static pthread_mutex_t mutex;
static bool mutex_initialized = false;

static inline void init_io_lock()
{
	if (!mutex_initialized) {
		pthread_mutex_init(&mutex, NULL);
		mutex_initialized = true;
	}
}

static inline void io_lock()
{
	pthread_mutex_lock(&mutex);
}

static inline void io_unlock()
{
	pthread_mutex_unlock(&mutex);
}


/*************************
 * General file handling *
 *************************/

int db_blocks_low_level_io_open(char *path, int flags, int mode)
{
	init_io_lock();
	return open(path, flags, mode);
}

int db_blocks_low_level_io_unlink(char *path)
{
	return unlink(path);
}

int db_blocks_low_level_io_close(int fd)
{
	int retval;
	io_lock();
	retval = close(fd);
	io_unlock();
	return retval;
}

off64_t db_blocks_low_level_io_seek_end(int fd)
{
	return lseek64(fd, 0, SEEK_END);
}


/**********************
 * Default read/write *
 **********************/

ssize_t db_blocks_low_level_io_read(int fd, void *buf, size_t nbytes,
		off64_t offset)
{
	ssize_t retval;
	io_lock();
	retval = pread(fd, buf, nbytes, offset);
	io_unlock();
	return retval;
}

ssize_t db_blocks_low_level_io_write(int fd, const void *buf,
		size_t nbytes, off64_t offset)
{
	ssize_t retval;
	io_lock();
	retval = pwrite(fd, buf, nbytes, offset);
	io_unlock();
	return retval;
}

off64_t db_blocks_low_level_io_write_new(int fd, const void *buf,
		size_t nbytes)
{
	off64_t offset;

	io_lock();
	offset = lseek64(fd, 0, SEEK_END);
	if (offset == (off64_t)-1) {
		io_unlock();
		return (off64_t)-1;
	}
	pwrite(fd, buf, nbytes, offset);
	io_unlock();
	return offset;
}


/*********************
 * Direct read/write *
 *********************/


static void *aligned_buf = NULL;
static size_t aligned_nbytes = 0;

static inline void init_aligned_buf(size_t nbytes)
{
#ifndef HAVE_POSIX_MEMALIGN
	assert(false);
#else
	if (aligned_nbytes < nbytes) {
		free(aligned_buf);
		posix_memalign(&aligned_buf, 512, nbytes);
		aligned_nbytes = nbytes;
	}
#endif
}

ssize_t db_blocks_low_level_io_read_direct(int fd, void *buf, size_t nbytes,
		off64_t offset)
{
	ssize_t retval;
	io_lock();
	init_aligned_buf(nbytes);
	retval = pread(fd, aligned_buf, nbytes, offset);
	memcpy(buf, aligned_buf, nbytes);
	io_unlock();
	return retval;
}

ssize_t db_blocks_low_level_io_write_direct(int fd, const void *buf,
		size_t nbytes, off64_t offset)
{
	ssize_t retval;
	io_lock();
	init_aligned_buf(nbytes);
	memcpy(aligned_buf, buf, nbytes);
	retval = pwrite(fd, aligned_buf, nbytes, offset);
	io_unlock();
	return retval;
}

off64_t db_blocks_low_level_io_write_new_direct(int fd, const void *buf,
		size_t nbytes)
{
	off64_t offset;

	io_lock();
	offset = lseek64(fd, 0, SEEK_END);
	if (offset == (off64_t)-1) {
		io_unlock();
		return (off64_t)-1;
	}
	init_aligned_buf(nbytes);
	memcpy(aligned_buf, buf, nbytes);
	pwrite(fd, aligned_buf, nbytes, offset);
	io_unlock();
	return offset;
}


/*******************
 * In-file locking *
 *******************/

static int lock(int fd, off64_t offset, size_t len)
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

static int unlock(int fd, off64_t offset, size_t len)
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

int db_blocks_low_level_io_lock(int fd, off64_t offset, size_t len)
{
	return lock(fd, offset, len);
}

int db_blocks_low_level_io_unlock(int fd, off64_t offset, size_t len)
{
	return unlock(fd, offset, len);
}

int db_blocks_low_level_io_errno(void)
{
#ifndef WIN32
	return errno;
#else
	return 0;
#endif
}

