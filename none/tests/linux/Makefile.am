
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	blockfault.stderr.exp blockfault.vgtest \
	brk-overflow1.stderr.exp brk-overflow1.vgtest \
	brk-overflow2.stderr.exp brk-overflow2.vgtest \
	bug498317.stderr.exp bug498317.supp bug498317.vgtest \
	bug506910.stderr.exp bug506910.vgtest \
	clonev.stdout.exp clonev.stderr.exp clonev.vgtest \
        membarrier.stderr.exp membarrier.vgtest \
	mremap.stderr.exp mremap.stderr.exp-glibc27 mremap.stdout.exp \
	    mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.vgtest \
	mremap3.stderr.exp mremap3.stdout.exp mremap3.vgtest \
	mremap4.stderr.exp mremap4.vgtest \
	mremap5.stderr.exp mremap5.vgtest \
	mremap6.stderr.exp mremap6.vgtest \
	open_client.stderr.exp open_client.vgtest \
	pthread-stack.stderr.exp pthread-stack.vgtest \
	stack-overflow.stderr.exp stack-overflow.vgtest

check_PROGRAMS = \
	blockfault \
	brk-overflow1 \
	brk-overflow2 \
	bug498317 \
	clonev \
	mremap \
	mremap2 \
	mremap3 \
	mremap4 \
	mremap5 \
	mremap6 \
	pthread-stack \
	stack-overflow

if HAVE_OPENAT2
        check_PROGRAMS += bug506910 open_client
endif

if HAVE_NR_MEMBARRIER
        check_PROGRAMS += membarrier
endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# Special needs
if HAVE_OPENAT2
bug506910_SOURCES = bug506910.cpp
open_client_SOURCES = open_client.cpp
endif
clonev_LDADD = -lpthread
pthread_stack_LDADD = -lpthread

stack_overflow_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@ \
			@FLAG_W_NO_INFINITE_RECURSION@
