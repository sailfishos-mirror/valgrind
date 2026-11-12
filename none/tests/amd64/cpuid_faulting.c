#include <stdio.h>
#include <cpuid.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <stdlib.h>

#define ARCH_SET_CPUID 0x1012
#define ARCH_GET_CPUID 0x1011

static sigjmp_buf jump_buf;
volatile sig_atomic_t segv_caught = 0;

static void segv_handler(int sig) {
    segv_caught = 1;
    siglongjmp(jump_buf, 1);
}

void test_cpuid(void) {
    unsigned int eax, ebx, ecx, edx;
    __get_cpuid(0, &eax, &ebx, &ecx, &edx);
    printf("cpuid succeeded\n");
}

int main(void) {

    // Set up a SEGV handler
    struct sigaction sa = {};
    sa.sa_handler = segv_handler;
    sa.sa_flags = 0;
    if (sigaction(SIGSEGV, &sa, NULL) != 0) {
        perror("FAILED sigaction()\n");
        exit(1);
    }

    // CPUID enabled by default, calling it should succeed
    test_cpuid(); // Should pass

    // Disable CPUID
    if (syscall(SYS_arch_prctl, ARCH_SET_CPUID, 0) != 0) {
        if (errno == ENODEV) {
            printf("Disabling CPUID not supported on this CPU");
            exit(77);
        }
        perror("FAILED disabling CPUID instruction\n");
        exit(1);
    }

    // CPUID disabled, calling it should SEGV
    if (sigsetjmp(jump_buf, 1) == 0) {
        test_cpuid();
    }

    // Check the expected SEGV
    if (segv_caught == 1) {
        printf("expected cpuid fault\n");
    } else {
        printf("missing expected segv\n");
        exit(1);
    }
    // Enable CPUID again
    if (syscall(SYS_arch_prctl, ARCH_SET_CPUID, 1) != 0) {
        perror("FAILED enabling CPUID\n");
        exit(1);
    }

    // CPUID enabled, calling it should succeed
    test_cpuid();

    return 0;
}
