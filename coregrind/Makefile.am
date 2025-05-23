
# Be very careful when renaming any files, targets, whatever, in this
# Makefile.  Various parts of the system rely on these names having
# particular forms.

include $(top_srcdir)/Makefile.all.am

#----------------------------------------------------------------------------
# Basics, flags
#----------------------------------------------------------------------------

AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@ += \
	-I$(top_srcdir)/coregrind \
	-DVG_LIBDIR="\"$(pkglibexecdir)"\" \
	-DVG_PLATFORM="\"@VGCONF_ARCH_PRI@-@VGCONF_OS@\""
if VGCONF_HAVE_PLATFORM_SEC
AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@ += \
	-I$(top_srcdir)/coregrind \
	-DVG_LIBDIR="\"$(pkglibexecdir)"\" \
	-DVG_PLATFORM="\"@VGCONF_ARCH_SEC@-@VGCONF_OS@\""
endif


EXTRA_DIST = \
	m_debuginfo/README.txt \
	m_gdbserver/README_DEVELOPERS \
	docs/vgdb-manpage.xml

#----------------------------------------------------------------------------
# The launcher.  Built for the primary target only.
#----------------------------------------------------------------------------

bin_PROGRAMS = \
	valgrind \
	vgdb

bin_SCRIPTS = vgstack

if VGCONF_OS_IS_LINUX
valgrind_SOURCES = \
	launcher-linux.c \
	m_debuglog.c
endif
if VGCONF_OS_IS_DARWIN
valgrind_SOURCES = \
	launcher-darwin.c \
	m_debuglog.c
endif
if VGCONF_OS_IS_SOLARIS
valgrind_SOURCES = \
	launcher-linux.c \
	m_debuglog.c
endif
if VGCONF_OS_IS_FREEBSD
valgrind_SOURCES = \
	launcher-freebsd.c \
	m_debuglog.c
endif

# for valgrind coregrind building, use the LTO versions, in case they differ from non lto versions
AR = ${LTO_AR}
RANLIB = ${LTO_RANLIB}

valgrind_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
valgrind_CFLAGS    = $(AM_CFLAGS_PRI) $(LTO_CFLAGS) -fhosted
valgrind_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_LDFLAGS   = $(AM_CFLAGS_PRI) @LIB_UBSAN@
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
valgrind_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif
# On Android we must ask for non-executable stack, not sure why.
if VGCONF_PLATVARIANT_IS_ANDROID
valgrind_CFLAGS += -static
valgrind_LDFLAGS   += -Wl,-z,noexecstack
endif
if VGCONF_OS_IS_SOLARIS
valgrind_LDFLAGS   += -Wl,-M,/usr/lib/ld/map.noexstk
endif


vgdb_SOURCES = vgdb.c m_gdbserver/remote-utils-shared.c
if VGCONF_OS_IS_LINUX
if VGCONF_PLATVARIANT_IS_ANDROID
vgdb_SOURCES += vgdb-invoker-none.c
else
vgdb_SOURCES += vgdb-invoker-ptrace.c
endif
endif
if VGCONF_OS_IS_DARWIN
# Some darwin specific stuff is needed as ptrace is not
# fully supported on MacOS. Till we find someone courageous
# having access to Darwin, 'none' implementation is used.
vgdb_SOURCES += vgdb-invoker-none.c
endif
if VGCONF_OS_IS_SOLARIS
vgdb_SOURCES += vgdb-invoker-solaris.c
endif
if VGCONF_OS_IS_FREEBSD
vgdb_SOURCES += vgdb-invoker-freebsd.c
endif

vgdb_CPPFLAGS  = $(AM_CPPFLAGS_PRI) $(GDB_SCRIPTS_DIR)
vgdb_CFLAGS    = $(AM_CFLAGS_PRI) $(LTO_CFLAGS) -fhosted
vgdb_CCASFLAGS = $(AM_CCASFLAGS_PRI)
vgdb_LDFLAGS   = $(AM_CFLAGS_PRI) @LIB_UBSAN@
if VGCONF_PLATVARIANT_IS_ANDROID
vgdb_CFLAGS    += -static
endif
if VGCONF_OS_IS_SOLARIS
vgdb_LDADD     = -lsocket
else
if !VGCONF_PLATVARIANT_IS_ANDROID
vgdb_LDADD     = -lpthread
endif
endif
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
vgdb_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif

#----------------------------------------------------------------------------
# Darwin Mach stuff
#----------------------------------------------------------------------------

# Mach RPC interface definitions
# Here are some more .defs files that are not used, but could be in the
# future:
#	clock.defs \
#	clock_priv.defs \
#	clock_reply.defs \
#	exc.defs \
#	host_priv.defs \
#	host_security.defs \
#	ledger.defs \
#	lock_set.defs \
#	mach_host.defs \
#	mach_port.defs \
#	notify.defs \
#	processor.defs \
#	processor_set.defs \
#
mach_user_srcs =
mach_server_srcs =
mach_hdrs =
mach_defs =
if VGCONF_OS_IS_DARWIN
mach_user_srcs += \
	m_mach/mach_vmUser.c \
        m_mach/taskUser.c \
        m_mach/thread_actUser.c \
        m_mach/vm_mapUser.c
mach_server_srcs += \
	m_mach/mach_vmServer.c \
        m_mach/taskServer.c \
        m_mach/thread_actServer.c \
        m_mach/vm_mapServer.c
mach_hdrs += \
	m_mach/mach_vm.h \
        m_mach/task.h \
        m_mach/thread_act.h \
        m_mach/vm_map.h
mach_defs += \
	@XCODE_DIR@/mach/mach_vm.defs \
        @XCODE_DIR@/mach/task.defs \
        @XCODE_DIR@/mach/thread_act.defs \
        @XCODE_DIR@/mach/vm_map.defs
endif

# Be careful w.r.t. parallel builds.  See section 27.9 of the automake info
# page, "Handling Tools that Produce many Outputs".
$(abs_builddir)/m_mach: 
	mkdir -p $@
$(mach_user_srcs): $(mach_defs) $(abs_builddir)/m_mach
	(cd m_mach && mig $(mach_defs))
$(mach_hdrs): $(mach_defs) $(mach_user_srcs) $(abs_builddir)/m_mach
	(cd m_mach && mig $(mach_defs))

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

noinst_HEADERS = \
	pub_core_addrinfo.h	\
	pub_core_aspacehl.h	\
	pub_core_aspacemgr.h	\
	pub_core_basics.h	\
	pub_core_basics_asm.h	\
	pub_core_clientstate.h	\
	pub_core_clreq.h	\
	pub_core_commandline.h	\
	pub_core_coredump.h	\
	pub_core_cpuid.h	\
	pub_core_deduppoolalloc.h \
	pub_core_debuginfo.h	\
	pub_core_debuglog.h	\
	pub_core_demangle.h	\
	pub_core_dispatch.h	\
	pub_core_dispatch_asm.h	\
	pub_core_errormgr.h	\
	pub_core_execontext.h	\
	pub_core_extension.h    \
	pub_core_gdbserver.h	\
	pub_core_guest.h	\
	pub_core_hashtable.h	\
	pub_core_initimg.h	\
	pub_core_inner.h	\
	pub_core_libcbase.h	\
	pub_core_libcassert.h	\
	pub_core_libcfile.h	\
	pub_core_libcprint.h	\
	pub_core_libcproc.h	\
	pub_core_libcsetjmp.h	\
	pub_core_libcsignal.h	\
	pub_core_mach.h		\
	pub_core_machine.h	\
	pub_core_mallocfree.h	\
	pub_core_options.h	\
	pub_core_oset.h		\
	pub_core_pathscan.h     \
	pub_core_poolalloc.h	\
	pub_core_rangemap.h	\
	pub_core_redir.h	\
	pub_core_replacemalloc.h\
	pub_core_sbprofile.h	\
	pub_core_scheduler.h	\
	pub_core_seqmatch.h	\
	pub_core_sigframe.h	\
	pub_core_signals.h	\
	pub_core_sparsewa.h	\
	pub_core_stacks.h	\
	pub_core_stacktrace.h	\
	pub_core_syscall.h	\
	pub_core_syswrap.h	\
	pub_core_threadstate.h	\
	pub_core_tooliface.h	\
	pub_core_trampoline.h	\
	pub_core_translate.h	\
	pub_core_transtab.h	\
	pub_core_transtab_asm.h	\
	pub_core_ume.h		\
	pub_core_vki.h		\
	pub_core_vkiscnums.h	\
	pub_core_vkiscnums_asm.h\
	pub_core_wordfm.h	\
	pub_core_xarray.h	\
	pub_core_xtree.h	\
	pub_core_xtmemory.h	\
	m_aspacemgr/priv_aspacemgr.h \
	m_debuginfo/priv_misc.h	\
	m_debuginfo/priv_storage.h	\
	m_debuginfo/priv_tytypes.h      \
	m_debuginfo/priv_readpdb.h	\
	m_debuginfo/priv_d3basics.h	\
	m_debuginfo/priv_readdwarf.h	\
	m_debuginfo/priv_readdwarf3.h	\
	m_debuginfo/priv_readelf.h	\
	m_debuginfo/priv_readexidx.h	\
	m_debuginfo/priv_readmacho.h	\
	m_debuginfo/priv_image.h	\
	m_debuginfo/lzoconf.h		\
	m_debuginfo/lzodefs.h		\
	m_debuginfo/minilzo.h		\
	m_debuginfo/zstd.h              \
	m_demangle/ansidecl.h	\
	m_demangle/cp-demangle.h \
	m_demangle/dyn-string.h	\
	m_demangle/demangle.h	\
	m_demangle/safe-ctype.h \
	m_demangle/vg_libciface.h \
	m_extension/priv_extension.h \
	m_extension/priv_types_n_macros.h \
	m_gdbserver/regcache.h \
	m_gdbserver/regdef.h \
	m_gdbserver/server.h \
	m_gdbserver/remote-utils-shared.h \
	m_gdbserver/target.h \
	m_gdbserver/valgrind_low.h \
	m_gdbserver/gdb/signals.h \
	m_scheduler/priv_sema.h \
	m_scheduler/priv_sched-lock.h \
	m_scheduler/priv_sched-lock-impl.h \
	m_sigframe/priv_sigframe.h \
	m_syswrap/priv_types_n_macros.h \
	m_syswrap/priv_syswrap-generic.h \
	m_syswrap/priv_syswrap-linux.h \
	m_syswrap/priv_syswrap-linux-variants.h \
	m_syswrap/priv_syswrap-freebsd.h \
	m_syswrap/priv_syswrap-darwin.h \
	m_syswrap/priv_syswrap-solaris.h \
	m_syswrap/priv_syswrap-main.h \
	m_syswrap/priv_syswrap-xen.h \
	m_ume/priv_ume.h \
	vgdb.h

#----------------------------------------------------------------------------
# libcoregrind-<platform>.a
#----------------------------------------------------------------------------

BUILT_SOURCES = $(mach_user_srcs)
CLEANFILES    = $(mach_user_srcs) $(mach_server_srcs) $(mach_hdrs)

pkglib_LIBRARIES  = libcoregrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
if VGCONF_HAVE_PLATFORM_SEC
pkglib_LIBRARIES += libcoregrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

COREGRIND_SOURCES_COMMON = \
	m_addrinfo.c \
	m_cache.c \
	m_commandline.c \
	m_compiler.c \
	m_clientstate.c \
	m_cpuid.S \
	m_deduppoolalloc.c \
	m_debuglog.c \
	m_errormgr.c \
	m_execontext.c \
	m_hashtable.c \
	m_libcbase.c \
	m_libcassert.c \
	m_libcfile.c \
	m_libcprint.c \
	m_libcproc.c \
	m_libcsignal.c \
	m_machine.c \
	m_mallocfree.c \
	m_options.c \
	m_oset.c \
	m_pathscan.c \
	m_poolalloc.c \
	m_rangemap.c \
	m_redir.c \
	m_sbprofile.c \
	m_seqmatch.c \
	m_signals.c \
	m_sparsewa.c \
	m_stacktrace.c \
	m_syscall.c \
	m_threadstate.c \
	m_tooliface.c \
	m_trampoline.S \
	m_translate.c \
	m_transtab.c \
	m_vki.c \
	m_vkiscnums.c \
	m_wordfm.c \
	m_xarray.c \
	m_xtree.c \
	m_xtmemory.c \
	m_aspacehl.c \
	m_aspacemgr/aspacemgr-common.c \
	m_aspacemgr/aspacemgr-linux.c \
	m_aspacemgr/aspacemgr-segnames.c \
	m_coredump/coredump-elf.c \
	m_coredump/coredump-macho.c \
	m_coredump/coredump-solaris.c \
	m_debuginfo/misc.c \
	m_debuginfo/d3basics.c \
	m_debuginfo/debuginfo.c \
	m_debuginfo/image.c \
	m_debuginfo/minilzo-inl.c \
	m_debuginfo/readdwarf.c \
	m_debuginfo/readdwarf3.c \
	m_debuginfo/readelf.c \
	m_debuginfo/readexidx.c \
	m_debuginfo/readmacho.c \
	m_debuginfo/readpdb.c \
	m_debuginfo/storage.c \
	m_debuginfo/tinfl.c \
	m_debuginfo/tytypes.c \
	m_debuginfo/zstddeclib.c \
	m_demangle/cp-demangle.c \
	m_demangle/cplus-dem.c \
	m_demangle/demangle.c \
	m_demangle/dyn-string.c \
	m_demangle/d-demangle.c \
	m_demangle/rust-demangle.c \
	m_demangle/safe-ctype.c \
	m_dispatch/dispatch-x86-linux.S \
	m_dispatch/dispatch-amd64-linux.S \
	m_dispatch/dispatch-ppc32-linux.S \
	m_dispatch/dispatch-ppc64be-linux.S \
	m_dispatch/dispatch-ppc64le-linux.S \
	m_dispatch/dispatch-arm-linux.S \
	m_dispatch/dispatch-arm64-linux.S \
	m_dispatch/dispatch-s390x-linux.S \
	m_dispatch/dispatch-mips32-linux.S \
	m_dispatch/dispatch-mips64-linux.S \
	m_dispatch/dispatch-nanomips-linux.S \
	m_dispatch/dispatch-riscv64-linux.S \
	m_dispatch/dispatch-x86-freebsd.S \
	m_dispatch/dispatch-amd64-freebsd.S \
	m_dispatch/dispatch-arm64-freebsd.S \
	m_dispatch/dispatch-x86-darwin.S \
	m_dispatch/dispatch-amd64-darwin.S \
	m_dispatch/dispatch-x86-solaris.S \
	m_dispatch/dispatch-amd64-solaris.S \
	m_extension/extension-main.c \
	m_extension/extension-s390x.c \
	m_gdbserver/inferiors.c \
	m_gdbserver/regcache.c \
	m_gdbserver/remote-utils.c \
	m_gdbserver/remote-utils-shared-gdbserver.c \
	m_gdbserver/server.c \
	m_gdbserver/signals.c \
	m_gdbserver/target.c \
	m_gdbserver/utils.c \
	m_gdbserver/valgrind-low-x86.c \
	m_gdbserver/valgrind-low-amd64.c \
	m_gdbserver/valgrind-low-arm.c \
	m_gdbserver/valgrind-low-arm64.c \
	m_gdbserver/valgrind-low-ppc32.c \
	m_gdbserver/valgrind-low-ppc64.c \
	m_gdbserver/valgrind-low-s390x.c \
	m_gdbserver/valgrind-low-mips32.c \
	m_gdbserver/valgrind-low-mips64.c \
	m_gdbserver/valgrind-low-nanomips.c \
	m_gdbserver/valgrind-low-riscv64.c \
	m_gdbserver/version.c \
	m_initimg/initimg-linux.c \
	m_initimg/initimg-freebsd.c \
	m_initimg/initimg-darwin.c \
	m_initimg/initimg-solaris.c \
	m_mach/mach_basics.c \
	m_mach/mach_msg.c \
	m_mach/mach_traps-x86-darwin.S \
	m_mach/mach_traps-amd64-darwin.S \
	m_replacemalloc/replacemalloc_core.c \
	m_scheduler/sched-lock.c \
	m_scheduler/sched-lock-generic.c \
	m_scheduler/scheduler.c \
	m_scheduler/sema.c \
	m_sigframe/sigframe-common.c \
	m_sigframe/sigframe-x86-linux.c \
	m_sigframe/sigframe-amd64-linux.c \
	m_sigframe/sigframe-x86-freebsd.c \
	m_sigframe/sigframe-amd64-freebsd.c \
	m_sigframe/sigframe-arm64-freebsd.c \
	m_sigframe/sigframe-ppc32-linux.c \
	m_sigframe/sigframe-ppc64-linux.c \
	m_sigframe/sigframe-arm-linux.c \
	m_sigframe/sigframe-arm64-linux.c \
	m_sigframe/sigframe-s390x-linux.c \
	m_sigframe/sigframe-mips32-linux.c \
	m_sigframe/sigframe-mips64-linux.c \
	m_sigframe/sigframe-nanomips-linux.c \
	m_sigframe/sigframe-riscv64-linux.c \
	m_sigframe/sigframe-x86-darwin.c \
	m_sigframe/sigframe-amd64-darwin.c \
	m_sigframe/sigframe-solaris.c \
	m_syswrap/syscall-x86-linux.S \
	m_syswrap/syscall-amd64-linux.S \
	m_syswrap/syscall-ppc32-linux.S \
	m_syswrap/syscall-ppc64be-linux.S \
	m_syswrap/syscall-ppc64le-linux.S \
	m_syswrap/syscall-arm-linux.S \
	m_syswrap/syscall-arm64-linux.S \
	m_syswrap/syscall-s390x-linux.S \
	m_syswrap/syscall-mips32-linux.S \
	m_syswrap/syscall-mips64-linux.S \
	m_syswrap/syscall-nanomips-linux.S \
	m_syswrap/syscall-riscv64-linux.S \
	m_syswrap/syscall-x86-freebsd.S \
	m_syswrap/syscall-amd64-freebsd.S \
	m_syswrap/syscall-arm64-freebsd.S \
	m_syswrap/syscall-x86-darwin.S \
	m_syswrap/syscall-amd64-darwin.S \
	m_syswrap/syscall-x86-solaris.S \
	m_syswrap/syscall-amd64-solaris.S \
	m_syswrap/syswrap-main.c \
	m_syswrap/syswrap-generic.c \
	m_syswrap/syswrap-linux.c \
	m_syswrap/syswrap-linux-variants.c \
	m_syswrap/syswrap-freebsd.c \
	m_syswrap/syswrap-darwin.c \
	m_syswrap/syswrap-solaris.c \
	m_syswrap/syswrap-x86-linux.c \
	m_syswrap/syswrap-amd64-linux.c \
	m_syswrap/syswrap-ppc32-linux.c \
	m_syswrap/syswrap-ppc64-linux.c \
	m_syswrap/syswrap-x86-freebsd.c \
	m_syswrap/syswrap-amd64-freebsd.c \
	m_syswrap/syswrap-arm64-freebsd.c \
	m_syswrap/syswrap-arm-linux.c \
	m_syswrap/syswrap-arm64-linux.c \
	m_syswrap/syswrap-s390x-linux.c \
	m_syswrap/syswrap-mips32-linux.c \
	m_syswrap/syswrap-mips64-linux.c \
	m_syswrap/syswrap-nanomips-linux.c \
	m_syswrap/syswrap-riscv64-linux.c \
	m_syswrap/syswrap-x86-darwin.c \
	m_syswrap/syswrap-amd64-darwin.c \
	m_syswrap/syswrap-xen.c \
	m_syswrap/syswrap-x86-solaris.c \
	m_syswrap/syswrap-amd64-solaris.c \
	m_ume/elf.c \
	m_ume/macho.c \
	m_ume/main.c \
	m_ume/script.c

# The below files cannot be compiled with lto, otherwise that gives
# undefined symbols at link time. So, define a noinst library to
# build the needed .o with specific flags.
# These objects are added to the libcoregrind library.
NOLTO_COREGRIND_SOURCES_COMMON = \
	m_libcsetjmp.c \
	m_main.c \
	m_stacks.c \
	m_gdbserver/m_gdbserver.c
noinst_LIBRARIES = libnolto_coregrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
libnolto_coregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
    $(NOLTO_COREGRIND_SOURCES_COMMON)
libnolto_coregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CPPFLAGS = \
    $(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libnolto_coregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS = \
    $(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libnolto_coregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CCASFLAGS = \
    $(AM_CCASFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)

if VGCONF_HAVE_PLATFORM_SEC
noinst_LIBRARIES += libnolto_coregrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
libnolto_coregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
    $(NOLTO_COREGRIND_SOURCES_COMMON)
libnolto_coregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CPPFLAGS = \
    $(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libnolto_coregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS = \
    $(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libnolto_coregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CCASFLAGS = \
    $(AM_CCASFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif


libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
    $(COREGRIND_SOURCES_COMMON)
nodist_libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
    $(BUILT_SOURCES)
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CPPFLAGS = \
    $(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS = $(LTO_CFLAGS) \
    $(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CCASFLAGS = \
    $(AM_CCASFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
if ENABLE_LINUX_TICKET_LOCK_PRIMARY
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES += \
    m_scheduler/ticket-lock-linux.c
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS += \
    -DENABLE_LINUX_TICKET_LOCK
endif
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_LIBADD = \
    $(libnolto_coregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_OBJECTS)
libcoregrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_DEPENDENCIES = \
    libnolto_coregrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a

if VGCONF_HAVE_PLATFORM_SEC
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
    $(COREGRIND_SOURCES_COMMON)
nodist_libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
    $(BUILT_SOURCES)
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CPPFLAGS = \
    $(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS =  $(LTO_CFLAGS) \
    $(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CCASFLAGS = \
    $(AM_CCASFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
if ENABLE_LINUX_TICKET_LOCK_SECONDARY
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES += \
    m_scheduler/ticket-lock-linux.c
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS += \
    -DENABLE_LINUX_TICKET_LOCK
endif
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_LIBADD = \
    $(libnolto_coregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_OBJECTS)
libcoregrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_DEPENDENCIES = \
    libnolto_coregrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

#----------------------------------------------------------------------------
# libgcc-sup-<platform>.a
# Special supplemental library for functions normally supplied by glibc
# used by libgcc.
#----------------------------------------------------------------------------

pkglib_LIBRARIES  += libgcc-sup-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
if VGCONF_HAVE_PLATFORM_SEC
pkglib_LIBRARIES += libgcc-sup-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

libgcc_sup_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
	m_libgcc_sup.c
libgcc_sup_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libgcc_sup_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@)
if VGCONF_HAVE_PLATFORM_SEC
libgcc_sup_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
	m_libgcc_sup.c
libgcc_sup_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libgcc_sup_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@)
endif

#----------------------------------------------------------------------------
# libreplacemalloc_toolpreload-<platform>.a
#----------------------------------------------------------------------------

pkglib_LIBRARIES += libreplacemalloc_toolpreload-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a
if VGCONF_HAVE_PLATFORM_SEC
pkglib_LIBRARIES += libreplacemalloc_toolpreload-@VGCONF_ARCH_SEC@-@VGCONF_OS@.a
endif

libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_SOURCES = \
	m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
libreplacemalloc_toolpreload_@VGCONF_ARCH_PRI@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@)
if VGCONF_HAVE_PLATFORM_SEC
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_SOURCES = \
	m_replacemalloc/vg_replace_malloc.c
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
libreplacemalloc_toolpreload_@VGCONF_ARCH_SEC@_@VGCONF_OS@_a_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@)
endif

#----------------------------------------------------------------------------
# vgpreload_core-<platform>.a
#----------------------------------------------------------------------------

noinst_PROGRAMS  = vgpreload_core-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_core-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

if GDBSCRIPTS
  GDB_SCRIPTS_DIR=-DVG_GDBSCRIPTS_DIR="\"@VG_GDBSCRIPTS_DIR@\""
else
  GDB_SCRIPTS_DIR=
endif

vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES = vg_preloaded.c
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) $(GDB_SCRIPTS_DIR)
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
if VGCONF_HAVE_PLATFORM_SEC
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES = vg_preloaded.c
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) $(GDBSCRIPTS_DIR)
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif

if VGCONF_OS_IS_SOLARIS
# Give the vgpreload_core library a proper soname so it can be easily
# recognized during reading of debug information.
vgpreload_core_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS += \
	-Wl,-soname -Wl,vgpreload_core.so.0
if VGCONF_HAVE_PLATFORM_SEC
vgpreload_core_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS += \
	-Wl,-soname -Wl,vgpreload_core.so.0
endif
endif

#----------------------------------------------------------------------------
# gdbserver xml target descriptions
#----------------------------------------------------------------------------

GDBSERVER_XML_FILES = \
	m_gdbserver/32bit-core-valgrind-s1.xml \
	m_gdbserver/32bit-core-valgrind-s2.xml \
	m_gdbserver/32bit-core.xml \
	m_gdbserver/32bit-linux-valgrind-s1.xml \
	m_gdbserver/32bit-linux-valgrind-s2.xml \
	m_gdbserver/32bit-linux.xml \
	m_gdbserver/32bit-sse-valgrind-s1.xml \
	m_gdbserver/32bit-sse-valgrind-s2.xml \
	m_gdbserver/32bit-sse.xml \
	m_gdbserver/64bit-avx-valgrind-s2.xml \
	m_gdbserver/64bit-avx-valgrind-s1.xml \
	m_gdbserver/64bit-avx.xml \
	m_gdbserver/64bit-core-valgrind-s1.xml \
	m_gdbserver/64bit-core-valgrind-s2.xml \
	m_gdbserver/64bit-core.xml \
	m_gdbserver/64bit-linux-valgrind-s1.xml \
	m_gdbserver/64bit-linux-valgrind-s2.xml \
	m_gdbserver/64bit-linux.xml \
	m_gdbserver/64bit-sse-valgrind-s1.xml \
	m_gdbserver/64bit-sse-valgrind-s2.xml \
	m_gdbserver/64bit-sse.xml \
	m_gdbserver/amd64-avx-coresse-valgrind.xml \
	m_gdbserver/amd64-avx-coresse.xml \
	m_gdbserver/amd64-avx-linux-valgrind.xml \
	m_gdbserver/amd64-avx-linux.xml \
	m_gdbserver/amd64-coresse-valgrind.xml \
	m_gdbserver/amd64-linux-valgrind.xml \
	m_gdbserver/arm-core-valgrind-s1.xml \
	m_gdbserver/arm-core-valgrind-s2.xml \
	m_gdbserver/arm-core.xml \
	m_gdbserver/arm-vfpv3-valgrind-s1.xml \
	m_gdbserver/arm-vfpv3-valgrind-s2.xml \
	m_gdbserver/arm-vfpv3.xml \
	m_gdbserver/arm-with-vfpv3-valgrind.xml \
	m_gdbserver/arm-with-vfpv3.xml \
	m_gdbserver/i386-coresse-valgrind.xml \
	m_gdbserver/i386-linux-valgrind.xml \
	m_gdbserver/power64-core-valgrind-s1.xml \
	m_gdbserver/power64-core-valgrind-s2.xml \
	m_gdbserver/power64-core.xml \
	m_gdbserver/power64-core2-valgrind-s1.xml \
	m_gdbserver/power64-core2-valgrind-s2.xml \
	m_gdbserver/power64-linux-valgrind-s1.xml \
	m_gdbserver/power64-linux-valgrind-s2.xml \
	m_gdbserver/power64-linux.xml \
	m_gdbserver/power-altivec-valgrind-s1.xml \
	m_gdbserver/power-altivec-valgrind-s2.xml \
	m_gdbserver/power-altivec.xml \
	m_gdbserver/power-vsx-valgrind-s1.xml \
	m_gdbserver/power-vsx-valgrind-s2.xml \
	m_gdbserver/power-vsx.xml \
	m_gdbserver/power-core-valgrind-s1.xml \
	m_gdbserver/power-core-valgrind-s2.xml \
	m_gdbserver/power-core.xml \
	m_gdbserver/power-fpu-valgrind-s1.xml \
	m_gdbserver/power-fpu-valgrind-s2.xml \
	m_gdbserver/power-fpu.xml \
	m_gdbserver/power-linux-valgrind-s1.xml \
	m_gdbserver/power-linux-valgrind-s2.xml \
	m_gdbserver/power-linux.xml \
	m_gdbserver/powerpc-altivec32l-valgrind.xml \
	m_gdbserver/powerpc-altivec32l.xml \
	m_gdbserver/powerpc-altivec64l-valgrind.xml \
	m_gdbserver/powerpc-altivec64l.xml \
	m_gdbserver/s390-acr-valgrind-s1.xml \
	m_gdbserver/s390-acr-valgrind-s2.xml \
	m_gdbserver/s390-acr.xml \
	m_gdbserver/s390-fpr-valgrind-s1.xml \
	m_gdbserver/s390-fpr-valgrind-s2.xml \
	m_gdbserver/s390-fpr.xml \
	m_gdbserver/s390x-core64-valgrind-s1.xml \
	m_gdbserver/s390x-core64-valgrind-s2.xml \
	m_gdbserver/s390x-core64.xml \
	m_gdbserver/s390x-generic-valgrind.xml \
	m_gdbserver/s390x-generic.xml \
	m_gdbserver/s390x-linux64-valgrind-s1.xml \
	m_gdbserver/s390x-linux64-valgrind-s2.xml \
	m_gdbserver/s390x-linux64.xml \
	m_gdbserver/s390-vx-valgrind-s1.xml \
	m_gdbserver/s390-vx-valgrind-s2.xml \
	m_gdbserver/s390-vx.xml \
	m_gdbserver/s390x-vx-linux-valgrind.xml \
	m_gdbserver/s390x-vx-linux.xml \
	m_gdbserver/mips-cp0-valgrind-s1.xml \
	m_gdbserver/mips-cp0-valgrind-s2.xml \
	m_gdbserver/mips-cp0.xml \
	m_gdbserver/mips-cpu-valgrind-s1.xml \
	m_gdbserver/mips-cpu-valgrind-s2.xml \
	m_gdbserver/mips-cpu.xml \
	m_gdbserver/mips-linux.xml \
	m_gdbserver/mips-linux-valgrind.xml \
	m_gdbserver/mips-fpu-valgrind-s1.xml \
	m_gdbserver/mips-fpu-valgrind-s2.xml \
	m_gdbserver/mips-fpu.xml \
	m_gdbserver/mips64-cp0-valgrind-s1.xml \
	m_gdbserver/mips64-cp0-valgrind-s2.xml \
	m_gdbserver/mips64-cp0.xml \
	m_gdbserver/mips64-cpu-valgrind-s1.xml \
	m_gdbserver/mips64-cpu-valgrind-s2.xml \
	m_gdbserver/mips64-cpu.xml \
	m_gdbserver/mips64-linux.xml \
	m_gdbserver/mips64-linux-valgrind.xml \
	m_gdbserver/mips64-fpu-valgrind-s1.xml \
	m_gdbserver/mips64-fpu-valgrind-s2.xml \
	m_gdbserver/mips64-fpu.xml \
	m_gdbserver/riscv64-cpu-valgrind-s1.xml \
	m_gdbserver/riscv64-cpu-valgrind-s2.xml \
	m_gdbserver/riscv64-cpu.xml \
	m_gdbserver/riscv64-linux.xml \
	m_gdbserver/riscv64-linux-valgrind.xml \
	m_gdbserver/riscv64-fpu-valgrind-s1.xml \
	m_gdbserver/riscv64-fpu-valgrind-s2.xml \
	m_gdbserver/riscv64-fpu.xml

# so as to make sure these get copied into the install tree
vglibdir = $(pkglibexecdir)
vglib_DATA  = $(GDBSERVER_XML_FILES)

if GDBSCRIPTS
gdbscriptsdir = @VG_GDBSCRIPTS_DIR@
gdbscripts_DATA  = m_gdbserver/valgrind-monitor.py
gdbscripts_DATA += m_gdbserver/valgrind-monitor-def.py
endif

# so as to make sure these get copied into the tarball
EXTRA_DIST  += $(GDBSERVER_XML_FILES)
EXTRA_DIST  += m_gdbserver/valgrind-monitor.py
EXTRA_DIST  += m_gdbserver/valgrind-monitor-def.py

#----------------------------------------------------------------------------
# General stuff
#----------------------------------------------------------------------------

all-local: inplace-noinst_PROGRAMS inplace-noinst_DSYMS
	mkdir -p $(inplacedir); \
	for f in $(vglib_DATA); do \
	  rm -f $(inplacedir)/$$f; \
	  ln -f -s ../$(subdir)/$$f $(inplacedir); \
	done

clean-local: clean-noinst_DSYMS

install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS

uninstall-local: uninstall-noinst_PROGRAMS uninstall-noinst_DSYMS

#----------------------------------------------------------------------------
# Darwin linker kludges
#----------------------------------------------------------------------------

if VGCONF_OS_IS_DARWIN

BUILT_SOURCES += fixup_macho_loadcmds
fixup_macho_loadcmds: fixup_macho_loadcmds.c
	$(CC) -I$(top_srcdir) -I$(top_builddir) -g -Wall -o $@ $<

CLEANFILES += fixup_macho_loadcmds

endif

EXTRA_DIST += fixup_macho_loadcmds.c
