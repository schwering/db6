#include <zlib.h>

#define DEFLATE_LEVEL Z_DEFAULT_COMPRESSION

int db_compression_deflate(void *src, int srclen, void *dst, int dstbuflen)
{
	int ret, dstlen;
	z_stream strm;

	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	ret = deflateInit(&strm, DEFLATE_LEVEL);
	if (ret != Z_OK) {
		deflateEnd(&strm);
		return 0;
	}

	strm.avail_in  = srclen;
	strm.next_in   = src;
	strm.avail_out = dstbuflen;
	strm.next_out  = dst;
	ret = deflate(&strm, Z_FINISH);
	if (ret != Z_STREAM_END) {
		deflateEnd(&strm);
		return 0;
	}
	dstlen = dstbuflen - strm.avail_out;

	deflateEnd(&strm);
	return dstlen;
}

int db_compression_inflate(void *src, int srclen, void *dst, int dstbuflen)
{
	int ret, dstlen;
	z_stream strm;

	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	strm.avail_in = 0;
	strm.next_in = Z_NULL;
	ret = inflateInit(&strm);
	if (ret != Z_OK) {
		inflateEnd(&strm);
		return 0;
	}

	strm.avail_in  = srclen;
	strm.next_in   = src;
	strm.avail_out = dstbuflen;
	strm.next_out  = dst;
	ret = inflate(&strm, Z_FINISH);
	if (ret != Z_STREAM_END) {
		inflateEnd(&strm);
		return 0;
	}
	dstlen = dstbuflen - strm.avail_out;

	inflateEnd(&strm);
	return dstlen;
}

