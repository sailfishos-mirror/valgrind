
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
		      filter_stderr_solaris \
		      filter_helgrind \
		      filter_xml \
                      filter_fiw \
		      filter_freebsd.awk \
		      filter_stderr_freebsd \
		      filter_bug392331

noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	annotate_hbefore.vgtest annotate_hbefore.stdout.exp \
		annotate_hbefore.stderr.exp \
	annotate_rwlock.vgtest annotate_rwlock.stdout.exp \
		annotate_rwlock.stderr.exp \
	annotate_smart_pointer.vgtest annotate_smart_pointer.stdout.exp \
		annotate_smart_pointer.stderr.exp \
	bug322621.vgtest bug322621.stderr.exp \
	bug327548.vgtest bug327548.stderr.exp \
	bug392331.vgtest bug392331.stdout.exp bug392331.stderr.exp \
	bug392331_supp.vgtest bug392331_supp.stdout.exp bug392331_supp.stderr.exp \
	bug392331.supp \
	bug484480.vgtest bug484480.stderr.exp bug484480.stdout.exp \
	cond_init_destroy.vgtest cond_init_destroy.stderr.exp \
	cond_timedwait_invalid.vgtest cond_timedwait_invalid.stdout.exp \
		cond_timedwait_invalid.stderr.exp \
	cond_timedwait_test.vgtest cond_timedwait_test.stdout.exp \
		cond_timedwait_test.stderr.exp \
	bar_bad.vgtest bar_bad.stdout.exp bar_bad.stderr.exp \
		bar_bad.stderr.exp-destroy-hang \
		bar_bad.stderr.exp-freebsd \
		bar_bad.stderr.exp-illumos \
	bar_trivial.vgtest bar_trivial.stdout.exp bar_trivial.stderr.exp \
	free_is_write.vgtest free_is_write.stdout.exp \
		free_is_write.stderr.exp \
	getaddrinfo.vgtest getaddrinfo.stderr.exp \
	hg01_all_ok.vgtest hg01_all_ok.stdout.exp hg01_all_ok.stderr.exp \
	hg02_deadlock.vgtest hg02_deadlock.stdout.exp hg02_deadlock.stderr.exp \
	hg03_inherit.vgtest hg03_inherit.stdout.exp hg03_inherit.stderr.exp \
	hg04_race.vgtest hg04_race.stdout.exp hg04_race.stderr.exp \
	hg04_race_h9.vgtest hg04_race_h9.stdout.exp hg04_race_h9.stderr.exp \
	hg05_race2.vgtest hg05_race2.stdout.exp hg05_race2.stderr.exp \
	hg06_readshared.vgtest hg06_readshared.stdout.exp \
		hg06_readshared.stderr.exp \
	locked_vs_unlocked1_fwd.vgtest \
		locked_vs_unlocked1_fwd.stderr.exp \
		locked_vs_unlocked1_fwd.stdout.exp \
	locked_vs_unlocked1_rev.vgtest \
		locked_vs_unlocked1_rev.stderr.exp \
		locked_vs_unlocked1_rev.stdout.exp \
	locked_vs_unlocked2.vgtest \
		locked_vs_unlocked2.stderr.exp \
		locked_vs_unlocked2.stdout.exp \
		locked_vs_unlocked2.stderr.exp-freebsd \
	locked_vs_unlocked3.vgtest \
		locked_vs_unlocked3.stderr.exp \
		locked_vs_unlocked3.stdout.exp \
		locked_vs_unlocked3.stderr.exp-freebsd \
	pth_barrier1.vgtest pth_barrier1.stdout.exp pth_barrier1.stderr.exp \
	pth_barrier2.vgtest pth_barrier2.stdout.exp pth_barrier2.stderr.exp \
	pth_barrier3.vgtest pth_barrier3.stdout.exp pth_barrier3.stderr.exp \
	pth_destroy_cond.vgtest \
		pth_destroy_cond.stdout.exp pth_destroy_cond.stderr.exp \
	pth_cond_destroy_busy.vgtest pth_cond_destroy_busy.stderr.exp \
		pth_cond_destroy_busy.stderr.exp-ppc64 \
		pth_cond_destroy_busy.stderr.exp-solaris \
		pth_cond_destroy_busy.stderr.exp-freebsd \
	pth_mempcpy_false_races.vgtest pth_mempcpy_false_races.stderr.exp \
	pth_spinlock.vgtest pth_spinlock.stdout.exp pth_spinlock.stderr.exp \
	rwlock_race.vgtest rwlock_race.stdout.exp rwlock_race.stderr.exp \
	rwlock_test.vgtest rwlock_test.stdout.exp rwlock_test.stderr.exp \
	sem_clockwait_np.vgtest sem_clockwait_np.stdout.exp \
		sem_clockwait_np.stderr.exp \
	sem_timedwait.vgtest sem_timedwait.stdout.exp sem_timedwait.stderr.exp \
	shared_timed_mutex.vgtest shared_timed_mutex.stderr.exp \
	shmem_abits.vgtest shmem_abits.stdout.exp shmem_abits.stderr.exp \
	stackteardown.vgtest stackteardown.stdout.exp stackteardown.stderr.exp \
	t2t_laog.vgtest t2t_laog.stdout.exp t2t_laog.stderr.exp \
	tc01_simple_race.vgtest tc01_simple_race.stdout.exp \
		tc01_simple_race.stderr.exp \
	tc02_simple_tls.vgtest tc02_simple_tls.stdout.exp \
		tc02_simple_tls.stderr.exp \
	tc03_re_excl.vgtest tc03_re_excl.stdout.exp \
		tc03_re_excl.stderr.exp \
	tc04_free_lock.vgtest tc04_free_lock.stdout.exp \
		tc04_free_lock.stderr.exp \
		tc04_free_lock.stderr.exp-freebsd \
	tc05_simple_race.vgtest tc05_simple_race.stdout.exp \
		tc05_simple_race.stderr.exp \
	tc06_two_races.vgtest tc06_two_races.stdout.exp \
		tc06_two_races.stderr.exp \
	tc06_two_races_xml.vgtest tc06_two_races_xml.stdout.exp \
		tc06_two_races_xml.stderr.exp \
		tc06_two_races_xml.stderr.exp-freebsd \
	tc07_hbl1.vgtest tc07_hbl1.stdout.exp tc07_hbl1.stderr.exp \
	tc08_hbl2.vgtest tc08_hbl2.stdout.exp tc08_hbl2.stderr.exp \
	tc09_bad_unlock.vgtest tc09_bad_unlock.stdout.exp \
		tc09_bad_unlock.stderr.exp tc09_bad_unlock.stderr.exp-solaris \
		tc09_bad_unlock.stderr.exp-freebsd \
	tc10_rec_lock.vgtest tc10_rec_lock.stdout.exp tc10_rec_lock.stderr.exp \
	tc11_XCHG.vgtest tc11_XCHG.stdout.exp tc11_XCHG.stderr.exp \
	tc12_rwl_trivial.vgtest tc12_rwl_trivial.stdout.exp \
		tc12_rwl_trivial.stderr.exp \
		tc12_rwl_trivial.stderr.exp-darwin970 \
		tc12_rwl_trivial.stderr.exp-solaris \
		tc12_rwl_trivial.stderr.exp-freebsd \
	tc13_laog1.vgtest tc13_laog1.stdout.exp tc13_laog1.stderr.exp \
	tc14_laog_dinphils.vgtest tc14_laog_dinphils.stdout.exp \
		tc14_laog_dinphils.stderr.exp \
	tc15_laog_lockdel.vgtest tc15_laog_lockdel.stdout.exp \
		tc15_laog_lockdel.stderr.exp \
	tc16_byterace.vgtest tc16_byterace.stdout.exp \
		tc16_byterace.stderr.exp \
	tc17_sembar.vgtest tc17_sembar.stdout.exp \
		tc17_sembar.stderr.exp \
	tc18_semabuse.vgtest tc18_semabuse.stdout.exp \
		tc18_semabuse.stderr.exp \
		tc18_semabuse.stderr.exp-linux-mips32 \
		tc18_semabuse.stderr.exp-linux-mips32-b \
		tc18_semabuse.stderr.exp-solaris \
		tc18_semabuse.stderr.exp-freebsd \
	tc19_shadowmem.vgtest tc19_shadowmem.stdout.exp \
		tc19_shadowmem.stderr.exp tc19_shadowmem.stderr.exp-mips32 \
		tc19_shadowmem.stderr.exp-mips32-BE \
	tc20_verifywrap.vgtest tc20_verifywrap.stdout.exp \
		tc20_verifywrap.stderr.exp \
		tc20_verifywrap.stderr.exp-glibc-2.18 \
		tc20_verifywrap.stderr.exp-glibc-2.21 \
		tc20_verifywrap.stderr.exp-mips32 \
		tc20_verifywrap.stderr.exp-mips32-b \
		tc20_verifywrap.stderr.exp-s390x \
		tc20_verifywrap.stderr.exp-solaris \
	tc21_pthonce.vgtest tc21_pthonce.stdout.exp tc21_pthonce.stderr.exp \
	tc22_exit_w_lock.vgtest tc22_exit_w_lock.stdout.exp \
		tc22_exit_w_lock.stderr.exp \
		tc22_exit_w_lock.stderr.exp-kfail-x86 \
		tc22_exit_w_lock.stderr.exp-solaris \
		tc22_exit_w_lock.stderr.exp-freebsd15 \
	tc23_bogus_condwait.vgtest tc23_bogus_condwait.stdout.exp \
		tc23_bogus_condwait.stderr.exp \
		tc23_bogus_condwait.stderr.exp-mips32 \
		tc23_bogus_condwait.stderr.exp-freebsd \
	tc24_nonzero_sem.vgtest tc24_nonzero_sem.stdout.exp \
		tc24_nonzero_sem.stderr.exp \
	tls_threads.vgtest tls_threads.stdout.exp \
		tls_threads.stderr.exp \
	tls_threads2.vgtest tls_threads2.stderr.exp \
	trylock.vgtest trylock.stderr.exp

# Wrapper headers used by some check programs.
noinst_HEADERS = safe-pthread.h safe-semaphore.h

# XXX: tc18_semabuse uses operations that are unsupported on Darwin.  It
# should be conditionally compiled like tc20_verifywrap is.
check_PROGRAMS = \
	annotate_hbefore \
	bug327548 \
	bug484480 \
	cond_init_destroy \
	cond_timedwait_invalid \
	cond_timedwait_test \
	free_is_write \
	hg01_all_ok \
	hg02_deadlock \
	hg03_inherit \
	hg04_race \
	hg05_race2 \
	hg06_readshared \
	locked_vs_unlocked1 \
	locked_vs_unlocked2 \
	locked_vs_unlocked3 \
	pth_destroy_cond \
	pth_mempcpy_false_races \
	shmem_abits \
	stackteardown \
	t2t \
	tc01_simple_race \
	tc02_simple_tls \
	tc03_re_excl \
	tc04_free_lock \
	tc05_simple_race \
	tc06_two_races \
	tc07_hbl1 \
	tc08_hbl2 \
	tc09_bad_unlock \
	tc10_rec_lock \
	tc11_XCHG \
	tc12_rwl_trivial \
	tc13_laog1 \
	tc14_laog_dinphils \
	tc15_laog_lockdel \
	tc16_byterace \
	tc17_sembar \
	tc18_semabuse \
	tc19_shadowmem \
	tc21_pthonce \
	tc23_bogus_condwait \
	tc24_nonzero_sem \
	tls_threads

# DDD: it seg faults, and then the Valgrind exit path hangs
# JRS 29 July 09: it craps out in the stack unwinder, in
#==13480==    at 0xF00B81FF: ??? f00b8180 VG_(get_StackTrace_wrk)
#==13480==    by 0xF00B83F8: ??? f00b8340 VG_(get_StackTrace)
#==13480==    by 0xF009FE19: ??? f009fd70 record_ExeContext_wrk
#==13480==    by 0xF009D92E: ??? f009d8c0 construct_error
#==13480==    by 0xF009F001: ??? f009eef0 VG_(maybe_record_error)
#==13480==    by 0xF0081F80: ??? f0081f00 HG_(record_error_misc)
#==13480==    by 0xF0089C00: ??? f0089b80 evh__pre_thread_ll_exit
#==13480==    by 0xF01111D1: ??? f0111070 run_a_thread_NORETURN
#==13480==    by 0xF0111512: ??? f0111500 start_thread_NORETURN
# when the thread being unwound is at __bsdthread_terminate+0
#
# Like Tom says, the stack unwinder protection is bollocks.
# We should junk all previous schemes and simply get the 
# stack unwinder to consult aspacem at each frame (cache-accelerated,
# of course) to check each page it visits is accessible.
#
if ! VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
   check_PROGRAMS += \
	tc22_exit_w_lock
endif

if VGCONF_PLATFORMS_INCLUDE_ARM_LINUX
annotate_hbefore_CFLAGS = $(AM_CFLAGS) -mcpu=cortex-a8
tc07_hbl1_CFLAGS        = $(AM_CFLAGS) -mcpu=cortex-a8
tc08_hbl2_CFLAGS        = $(AM_CFLAGS) -mcpu=cortex-a8
else
annotate_hbefore_CFLAGS = $(AM_CFLAGS)
tc07_hbl1_CFLAGS        = $(AM_CFLAGS)
tc08_hbl2_CFLAGS        = $(AM_CFLAGS)
endif

if HAVE_PTHREAD_BARRIER
check_PROGRAMS += bar_bad bar_trivial bug322621
endif

if HAVE_PTHREAD_MUTEX_TIMEDLOCK
check_PROGRAMS += tc20_verifywrap
endif

if HAVE_BUILTIN_ATOMIC
check_PROGRAMS += annotate_rwlock
annotate_rwlock_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
endif

if HAVE_GETADDRINFO
check_PROGRAMS += getaddrinfo
endif

if HAVE_SEM_CLOCKWAIT_NP
check_PROGRAMS += sem_clockwait_np
endif

if HAVE_SEM_TIMEDWAIT
check_PROGRAMS += sem_timedwait
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

LDADD = -lpthread

if VGCONF_OS_IS_DARWIN
annotate_hbefore_CFLAGS = $(AM_CFLAGS) -mdynamic-no-pic
else
annotate_hbefore_CFLAGS = $(AM_CFLAGS)
endif

tc09_bad_unlock_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@
bug322621_SOURCES = bug322621.cpp
if HAVE_CXX17
check_PROGRAMS += bug392331
bug392331_SOURCES = bug392331.cpp
bug392331_CXXFLAGS = $(AM_CXXFLAGS) -std=c++17
endif

