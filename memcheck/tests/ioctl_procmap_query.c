#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/fs.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char** argv)
{
	char name[256];
        char cwd[256];
        getcwd(cwd, sizeof(cwd));
	struct procmap_query pq = {
		.size = sizeof(pq), .query_addr = (uintptr_t)main,
		.vma_name_size = 256, .vma_name_addr = (uintptr_t)name
	};
	int fd = open("/proc/self/maps", O_RDONLY);
	ioctl(fd, PROCMAP_QUERY, &pq);
	puts(name + strlen(cwd) + 1);
}

