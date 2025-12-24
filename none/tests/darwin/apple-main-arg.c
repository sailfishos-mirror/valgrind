#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/syslimits.h>
#include "../../../config.h"

// On Darwin there's this secret fourth argument, 'apple', which is a pointer
// to a string that contains the executable path, like argv[0], but unlike
// argv[0] it can't be changed using exec().

int main(int argc, char *argv[], char *envp[], char *apple[])
{
   char *pargv = calloc((PATH_MAX+1), sizeof(char)),
        *pappl = calloc((PATH_MAX+1), sizeof(char));
   int i;

   for (i = 0; envp[i]; i++)
      ;

   // envp[i]==NULL; envp[i+1]==apple[0]==executable_path
   //assert(envp[i+1] == apple[0]);
   fprintf(stderr, "DEBUG: apple %p\n", apple);
   fprintf(stderr, "DEBUG: envp[i+1] %p apple[0] %p\n", envp[i+1], apple[0]);
   fprintf(stderr, "DEBUG: apple[-1] %p\n", apple[-1]);
   fprintf(stderr, "DEBUG: apple[-2] %p\n", apple[-2]);
   fprintf(stderr, "DEBUG: apple[-3] %p\n", apple[-3]);
   fprintf(stderr, "DEBUG: apple[+1] %p\n", apple[+1]);
   fprintf(stderr, "DEBUG: apple[+2] %p\n", apple[+2]);
   fprintf(stderr, "DEBUG: &envp[i+1] %p &apple[0] %p\n", &envp[i+1], &apple[0]);
   if (envp[i+1] != apple[0]) {
      fprintf(stderr, "DEBUG: oh shit\n");
      if (apple[0]) {
         fprintf(stderr, "DEBUG: apple zero is %s\n", apple[0]);
      } else {
         fprintf(stderr, "DEBUG: apple zero is NULL\n");
         if (apple[-1]) {
            fprintf(stderr, "DEBUG: apple minus one is %p %s\n", apple[-1], apple[-1]);
         } else {
            fprintf(stderr, "DEBUG: apple minus one is NULL\n");
         }
         if (apple[-2]) {
            fprintf(stderr, "DEBUG: apple minus two is %p %s\n", apple[-2], apple[-2]);
         } else {
            fprintf(stderr, "DEBUG: apple minus two is NULL\n");
         }
      }
      if (envp[i+1]) {
         fprintf(stderr, "DEBUG: after NULL after envp is %s\n", envp[i+1]);
      } else {
         fprintf(stderr, "DEBUG: after NULL after envp is NULL\n");
      }
   }

   return 0;

   // Make sure realpath(argv[0]) == realpath(apple[0]).  (realpath resolves
   // symlinks.)
   // PJF this changed with macOS 10.14, apple path now has a prefix
#if (DARWIN_VERS >= DARWIN_10_14)
   const char prefix[] = "executable_path=";
   const size_t prefix_len = strlen(prefix);
   assert(strncmp(apple[0], prefix, prefix_len) == 0);
   realpath(apple[0]+prefix_len, pappl);
#else
   realpath(apple[0], pappl);
#endif
   realpath(argv[0], pargv);
   assert(0 == strcmp(pargv, pappl));

   free(pargv);
   free(pappl);

   return 0;
}

