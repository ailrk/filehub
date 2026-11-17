#include <sys/statvfs.h>
#include <stdint.h>

int hs_get_disk_usage(const char *path, uint64_t *total, uint64_t *used) {
    struct statvfs buf;
    if (statvfs(path, &buf) != 0) return -1;
    uint64_t blksize = (uint64_t)buf.f_frsize;
    *total = blksize * (uint64_t)buf.f_blocks;
    *used  = blksize * (uint64_t)(buf.f_blocks - buf.f_bfree);
    return 0;
}
