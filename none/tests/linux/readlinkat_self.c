#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "../../config.h"

int main(int argc, char** argv)
{
   char buf[PATH_MAX];
   memset(buf, 0, PATH_MAX);
   int ret = readlinkat(99, "/proc/self/exe", buf, PATH_MAX);
   if (argc > 1) {
      printf("ret = %d, buf = %.64s\n", ret, buf);
   }
   char resolved[PATH_MAX];
   realpath(argv[0], resolved);
   assert(strcmp(resolved, buf) == 0);
}

