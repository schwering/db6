#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>

#ifdef WIN32
  #define gettimeofday(tv, tz)	(!0)
#endif

inline unsigned long db_utils_timer_clock(void)
{
	return (unsigned long)clock();
}

inline unsigned long db_utils_timer_time(void)
{
	struct timeval tv;

	if (gettimeofday(&tv, NULL))
		return 0;
	/*printf("10E6 * %lu + %lu\n", (unsigned long)tv.tv_sec, (unsigned long)tv.tv_usec);*/
	return 10E6 * tv.tv_sec + tv.tv_usec;
}

const unsigned long db_utils_timer_CLOCKS_PER_SEC = CLOCKS_PER_SEC;
const unsigned long db_utils_timer_TIMES_PER_SEC = 10E6;

