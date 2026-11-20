#define _GNU_SOURCE
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define TEST_STRING_A "ABCD"

static int pg_sz;

// check mremap MREMAP_MAYMOVE | MREMAP_DONTUNMAP
static void check1(void) {
   char *old_addr, *new_addr;

   old_addr = mmap(NULL, pg_sz, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
   assert(old_addr != MAP_FAILED);

   memcpy(old_addr, TEST_STRING_A, sizeof(TEST_STRING_A));
   memcpy(old_addr + pg_sz - sizeof(TEST_STRING_A) - 1,
          TEST_STRING_A, sizeof(TEST_STRING_A));

   new_addr = mremap(old_addr, pg_sz, pg_sz,
                     MREMAP_MAYMOVE | MREMAP_DONTUNMAP, NULL);
   assert(new_addr != MAP_FAILED);

   // make sure new mapping is at different address
   assert(old_addr != new_addr);

   // make sure the new mapping has the original content
   assert(strcmp(new_addr, TEST_STRING_A) == 0);
   assert(strcmp(new_addr + pg_sz - sizeof(TEST_STRING_A) - 1,
                 TEST_STRING_A) == 0);

   // make sure the old mapping is still accesssible but zeroed
   for (int i = 0; i < pg_sz; i++)
      assert((unsigned char)old_addr[i] == 0);

   printf("check1 OK\n");

   munmap(old_addr, pg_sz);
   munmap(new_addr, pg_sz);
}


// check mremap MREMAP_MAYMOVE | MREMAP_FIXED | MREMAP_DONTUNMAP
static void check2(void) {
   char *old_addr, *fixed_addr, *new_addr;

   old_addr = mmap(NULL, pg_sz, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
   assert(old_addr != MAP_FAILED);

   fixed_addr = mmap(NULL, pg_sz, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
   assert(fixed_addr != MAP_FAILED);

   memcpy(old_addr, TEST_STRING_A, sizeof(TEST_STRING_A));
   memcpy(old_addr + pg_sz - sizeof(TEST_STRING_A) - 1,
          TEST_STRING_A, sizeof(TEST_STRING_A));

   new_addr = mremap(old_addr, pg_sz, pg_sz,
                     MREMAP_MAYMOVE | MREMAP_FIXED | MREMAP_DONTUNMAP,
                     fixed_addr);
   assert(new_addr != MAP_FAILED);

   // make sure new mapping is at different address
   // and new_addr is the one we wanted
   assert(old_addr != new_addr);
   assert(new_addr == fixed_addr);

   // make sure the new mapping has the original content
   assert(strcmp(new_addr, TEST_STRING_A) == 0);
   assert(strcmp(new_addr + pg_sz - sizeof(TEST_STRING_A) - 1,
                 TEST_STRING_A) == 0);

   // make sure the old mapping is still accesssible but zeroed
   for (int i = 0; i < pg_sz; i++)
      assert((unsigned char)old_addr[i] == 0);

   printf("check2 OK\n");

   munmap(old_addr, pg_sz);
   munmap(new_addr, pg_sz);
}

int main(void) {
   pg_sz = getpagesize();
   check1();
   check2();
   return 0;
}
