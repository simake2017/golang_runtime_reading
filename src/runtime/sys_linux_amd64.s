// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//
// System calls and other sys.stuff for AMD64, Linux
//

#include "go_asm.h"
#include "go_tls.h"
#include "textflag.h"

#define AT_FDCWD -100

#define SYS_read		0
#define SYS_write		1
#define SYS_close		3
#define SYS_mmap		9
#define SYS_munmap		11
#define SYS_brk 		12
#define SYS_rt_sigaction	13
#define SYS_rt_sigprocmask	14
#define SYS_rt_sigreturn	15
#define SYS_sched_yield 	24
#define SYS_mincore		27
#define SYS_madvise		28
#define SYS_setittimer		38
#define SYS_getpid		39
#define SYS_socket		41
#define SYS_connect		42
#define SYS_clone		56
#define SYS_exit		60
#define SYS_kill		62
#define SYS_fcntl		72
#define SYS_getrlimit		97
#define SYS_sigaltstack 	131
#define SYS_arch_prctl		158
#define SYS_gettid		186
#define SYS_tkill		200
#define SYS_futex		202
#define SYS_sched_getaffinity	204
#define SYS_epoll_create	213
#define SYS_exit_group		231
#define SYS_epoll_ctl		233
#define SYS_openat		257
#define SYS_faccessat		269
#define SYS_pselect6		270
#define SYS_epoll_pwait		281
#define SYS_epoll_create1	291

TEXT runtime·exit(SB),NOSPLIT,$0-4
	MOVL	code+0(FP), DI
	MOVL	$SYS_exit_group, AX
	SYSCALL
	RET

// func exitThread(wait *uint32)
TEXT runtime·exitThread(SB),NOSPLIT,$0-8
	MOVQ	wait+0(FP), AX
	// We're done using the stack.
	MOVL	$0, (AX)
	MOVL	$0, DI	// exit code
	MOVL	$SYS_exit, AX
	SYSCALL
	// We may not even have a stack any more.
	INT	$3
	JMP	0(PC)

TEXT runtime·open(SB),NOSPLIT,$0-20
	// This uses openat instead of open, because Android O blocks open.
	MOVL	$AT_FDCWD, DI // AT_FDCWD, so this acts like open
	MOVQ	name+0(FP), SI
	MOVL	mode+8(FP), DX
	MOVL	perm+12(FP), R10
	MOVL	$SYS_openat, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$-1, AX
	MOVL	AX, ret+16(FP)
	RET

TEXT runtime·closefd(SB),NOSPLIT,$0-12
	MOVL	fd+0(FP), DI
	MOVL	$SYS_close, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$-1, AX
	MOVL	AX, ret+8(FP)
	RET

TEXT runtime·write(SB),NOSPLIT,$0-28
	MOVQ	fd+0(FP), DI
	MOVQ	p+8(FP), SI
	MOVL	n+16(FP), DX
	MOVL	$SYS_write, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$-1, AX
	MOVL	AX, ret+24(FP)
	RET

TEXT runtime·read(SB),NOSPLIT,$0-28
	MOVL	fd+0(FP), DI
	MOVQ	p+8(FP), SI
	MOVL	n+16(FP), DX
	MOVL	$SYS_read, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$-1, AX
	MOVL	AX, ret+24(FP)
	RET

TEXT runtime·getrlimit(SB),NOSPLIT,$0-20
	MOVL	kind+0(FP), DI
	MOVQ	limit+8(FP), SI
	MOVL	$SYS_getrlimit, AX
	SYSCALL
	MOVL	AX, ret+16(FP)
	RET

TEXT runtime·usleep(SB),NOSPLIT,$16
	MOVL	$0, DX
	MOVL	usec+0(FP), AX
	MOVL	$1000000, CX
	DIVL	CX
	MOVQ	AX, 0(SP)
	MOVL	$1000, AX	// usec to nsec
	MULL	DX
	MOVQ	AX, 8(SP)

	// pselect6(0, 0, 0, 0, &ts, 0)
	MOVL	$0, DI
	MOVL	$0, SI
	MOVL	$0, DX
	MOVL	$0, R10
	MOVQ	SP, R8
	MOVL	$0, R9
	MOVL	$SYS_pselect6, AX
	SYSCALL
	RET

TEXT runtime·gettid(SB),NOSPLIT,$0-4
	MOVL	$SYS_gettid, AX
	SYSCALL
	MOVL	AX, ret+0(FP)
	RET

TEXT runtime·raise(SB),NOSPLIT,$0
	MOVL	$SYS_gettid, AX
	SYSCALL
	MOVL	AX, DI	// arg 1 tid
	MOVL	sig+0(FP), SI	// arg 2
	MOVL	$SYS_tkill, AX
	SYSCALL
	RET

TEXT runtime·raiseproc(SB),NOSPLIT,$0
	MOVL	$SYS_getpid, AX
	SYSCALL
	MOVL	AX, DI	// arg 1 pid
	MOVL	sig+0(FP), SI	// arg 2
	MOVL	$SYS_kill, AX
	SYSCALL
	RET

TEXT runtime·setitimer(SB),NOSPLIT,$0-24
	MOVL	mode+0(FP), DI
	MOVQ	new+8(FP), SI
	MOVQ	old+16(FP), DX
	MOVL	$SYS_setittimer, AX
	SYSCALL
	RET

TEXT runtime·mincore(SB),NOSPLIT,$0-28
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVQ	dst+16(FP), DX
	MOVL	$SYS_mincore, AX
	SYSCALL
	MOVL	AX, ret+24(FP)
	RET

// func walltime() (sec int64, nsec int32)
TEXT runtime·walltime(SB),NOSPLIT,$0-12
	// We don't know how much stack space the VDSO code will need,
	// so switch to g0.
	// In particular, a kernel configured with CONFIG_OPTIMIZE_INLINING=n
	// and hardening can use a full page of stack space in gettime_sym
	// due to stack probes inserted to avoid stack/heap collisions.
	// See issue #20427.

	MOVQ	SP, BP	// Save old SP; BP unchanged by C code.

	get_tls(CX)
	MOVQ	g(CX), AX
	MOVQ	g_m(AX), CX
	MOVQ	m_curg(CX), DX

	CMPQ	AX, DX		// Only switch if on curg.
	JNE	noswitch

	MOVQ	m_g0(CX), DX
	MOVQ	(g_sched+gobuf_sp)(DX), SP	// Set SP to g0 stack

noswitch:
	SUBQ	$16, SP		// Space for results
	ANDQ	$~15, SP	// Align for C code

	MOVQ	runtime·__vdso_clock_gettime_sym(SB), AX
	CMPQ	AX, $0
	JEQ	fallback
	MOVL	$0, DI // CLOCK_REALTIME
	LEAQ	0(SP), SI
	CALL	AX
	MOVQ	0(SP), AX	// sec
	MOVQ	8(SP), DX	// nsec
	MOVQ	BP, SP		// Restore real SP
	MOVQ	AX, sec+0(FP)
	MOVL	DX, nsec+8(FP)
	RET
fallback:
	LEAQ	0(SP), DI
	MOVQ	$0, SI
	MOVQ	runtime·__vdso_gettimeofday_sym(SB), AX
	CALL	AX
	MOVQ	0(SP), AX	// sec
	MOVL	8(SP), DX	// usec
	IMULQ	$1000, DX
	MOVQ	BP, SP		// Restore real SP
	MOVQ	AX, sec+0(FP)
	MOVL	DX, nsec+8(FP)
	RET

TEXT runtime·nanotime(SB),NOSPLIT,$0-8
	// Switch to g0 stack. See comment above in runtime·walltime.

	MOVQ	SP, BP	// Save old SP; BX unchanged by C code.

	get_tls(CX)
	MOVQ	g(CX), AX
	MOVQ	g_m(AX), CX
	MOVQ	m_curg(CX), DX

	CMPQ	AX, DX		// Only switch if on curg.
	JNE	noswitch

	MOVQ	m_g0(CX), DX
	MOVQ	(g_sched+gobuf_sp)(DX), SP	// Set SP to g0 stack

noswitch:
	SUBQ	$16, SP		// Space for results
	ANDQ	$~15, SP	// Align for C code

	MOVQ	runtime·__vdso_clock_gettime_sym(SB), AX
	CMPQ	AX, $0
	JEQ	fallback
	MOVL	$1, DI // CLOCK_MONOTONIC
	LEAQ	0(SP), SI
	CALL	AX
	MOVQ	0(SP), AX	// sec
	MOVQ	8(SP), DX	// nsec
	MOVQ	BP, SP		// Restore real SP
	// sec is in AX, nsec in DX
	// return nsec in AX
	IMULQ	$1000000000, AX
	ADDQ	DX, AX
	MOVQ	AX, ret+0(FP)
	RET
fallback:
	LEAQ	0(SP), DI
	MOVQ	$0, SI
	MOVQ	runtime·__vdso_gettimeofday_sym(SB), AX
	CALL	AX
	MOVQ	0(SP), AX	// sec
	MOVL	8(SP), DX	// usec
	MOVQ	BP, SP		// Restore real SP
	IMULQ	$1000, DX
	// sec is in AX, nsec in DX
	// return nsec in AX
	IMULQ	$1000000000, AX
	ADDQ	DX, AX
	MOVQ	AX, ret+0(FP)
	RET

TEXT runtime·rtsigprocmask(SB),NOSPLIT,$0-28
	MOVL	how+0(FP), DI
	MOVQ	new+8(FP), SI
	MOVQ	old+16(FP), DX
	MOVL	size+24(FP), R10
	MOVL	$SYS_rt_sigprocmask, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$0xf1, 0xf1  // crash
	RET

TEXT runtime·sysSigaction(SB),NOSPLIT,$0-36
	MOVQ	sig+0(FP), DI
	MOVQ	new+8(FP), SI
	MOVQ	old+16(FP), DX
	MOVQ	size+24(FP), R10
	MOVL	$SYS_rt_sigaction, AX
	SYSCALL
	MOVL	AX, ret+32(FP)
	RET

// Call the function stored in _cgo_sigaction using the GCC calling convention.
TEXT runtime·callCgoSigaction(SB),NOSPLIT,$16
	MOVQ	sig+0(FP), DI
	MOVQ	new+8(FP), SI
	MOVQ	old+16(FP), DX
	MOVQ	_cgo_sigaction(SB), AX
	MOVQ	SP, BX	// callee-saved
	ANDQ	$~15, SP	// alignment as per amd64 psABI
	CALL	AX
	MOVQ	BX, SP
	MOVL	AX, ret+24(FP)
	RET

TEXT runtime·sigfwd(SB),NOSPLIT,$0-32
	MOVQ	fn+0(FP),    AX
	MOVL	sig+8(FP),   DI
	MOVQ	info+16(FP), SI
	MOVQ	ctx+24(FP),  DX
	PUSHQ	BP
	MOVQ	SP, BP
	ANDQ	$~15, SP     // alignment for x86_64 ABI
	CALL	AX
	MOVQ	BP, SP
	POPQ	BP
	RET

TEXT runtime·sigtramp(SB),NOSPLIT,$72
	// Save callee-saved C registers, since the caller may be a C signal handler.
	MOVQ	BX,  bx-8(SP)
	MOVQ	BP,  bp-16(SP)  // save in case GOEXPERIMENT=noframepointer is set
	MOVQ	R12, r12-24(SP)
	MOVQ	R13, r13-32(SP)
	MOVQ	R14, r14-40(SP)
	MOVQ	R15, r15-48(SP)
	// We don't save mxcsr or the x87 control word because sigtrampgo doesn't
	// modify them.

	MOVQ	DX, ctx-56(SP)
	MOVQ	SI, info-64(SP)
	MOVQ	DI, signum-72(SP)
	MOVQ	$runtime·sigtrampgo(SB), AX
	CALL AX

	MOVQ	r15-48(SP), R15
	MOVQ	r14-40(SP), R14
	MOVQ	r13-32(SP), R13
	MOVQ	r12-24(SP), R12
	MOVQ	bp-16(SP),  BP
	MOVQ	bx-8(SP),   BX
	RET

// Used instead of sigtramp in programs that use cgo.
// Arguments from kernel are in DI, SI, DX.
TEXT runtime·cgoSigtramp(SB),NOSPLIT,$0
	// If no traceback function, do usual sigtramp.
	MOVQ	runtime·cgoTraceback(SB), AX
	TESTQ	AX, AX
	JZ	sigtramp

	// If no traceback support function, which means that
	// runtime/cgo was not linked in, do usual sigtramp.
	MOVQ	_cgo_callers(SB), AX
	TESTQ	AX, AX
	JZ	sigtramp

	// Figure out if we are currently in a cgo call.
	// If not, just do usual sigtramp.
	get_tls(CX)
	MOVQ	g(CX),AX
	TESTQ	AX, AX
	JZ	sigtrampnog     // g == nil
	MOVQ	g_m(AX), AX
	TESTQ	AX, AX
	JZ	sigtramp        // g.m == nil
	MOVL	m_ncgo(AX), CX
	TESTL	CX, CX
	JZ	sigtramp        // g.m.ncgo == 0
	MOVQ	m_curg(AX), CX
	TESTQ	CX, CX
	JZ	sigtramp        // g.m.curg == nil
	MOVQ	g_syscallsp(CX), CX
	TESTQ	CX, CX
	JZ	sigtramp        // g.m.curg.syscallsp == 0
	MOVQ	m_cgoCallers(AX), R8
	TESTQ	R8, R8
	JZ	sigtramp        // g.m.cgoCallers == nil
	MOVL	m_cgoCallersUse(AX), CX
	TESTL	CX, CX
	JNZ	sigtramp	// g.m.cgoCallersUse != 0

	// Jump to a function in runtime/cgo.
	// That function, written in C, will call the user's traceback
	// function with proper unwind info, and will then call back here.
	// The first three arguments, and the fifth, are already in registers.
	// Set the two remaining arguments now.
	MOVQ	runtime·cgoTraceback(SB), CX
	MOVQ	$runtime·sigtramp(SB), R9
	MOVQ	_cgo_callers(SB), AX
	JMP	AX

sigtramp:
	JMP	runtime·sigtramp(SB)

sigtrampnog:
	// Signal arrived on a non-Go thread. If this is SIGPROF, get a
	// stack trace.
	CMPL	DI, $27 // 27 == SIGPROF
	JNZ	sigtramp

	// Lock sigprofCallersUse.
	MOVL	$0, AX
	MOVL	$1, CX
	MOVQ	$runtime·sigprofCallersUse(SB), R11
	LOCK
	CMPXCHGL	CX, 0(R11)
	JNZ	sigtramp  // Skip stack trace if already locked.

	// Jump to the traceback function in runtime/cgo.
	// It will call back to sigprofNonGo, which will ignore the
	// arguments passed in registers.
	// First three arguments to traceback function are in registers already.
	MOVQ	runtime·cgoTraceback(SB), CX
	MOVQ	$runtime·sigprofCallers(SB), R8
	MOVQ	$runtime·sigprofNonGo(SB), R9
	MOVQ	_cgo_callers(SB), AX
	JMP	AX

// For cgo unwinding to work, this function must look precisely like
// the one in glibc.  The glibc source code is:
// https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/x86_64/sigaction.c
// The code that cares about the precise instructions used is:
// https://gcc.gnu.org/viewcvs/gcc/trunk/libgcc/config/i386/linux-unwind.h?revision=219188&view=markup
TEXT runtime·sigreturn(SB),NOSPLIT,$0
	MOVQ	$SYS_rt_sigreturn, AX
	SYSCALL
	INT $3	// not reached

TEXT runtime·sysMmap(SB),NOSPLIT,$0
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVL	prot+16(FP), DX
	MOVL	flags+20(FP), R10
	MOVL	fd+24(FP), R8
	MOVL	off+28(FP), R9

	MOVL	$SYS_mmap, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	ok
	NOTQ	AX
	INCQ	AX
	MOVQ	$0, p+32(FP)
	MOVQ	AX, err+40(FP)
	RET
ok:
	MOVQ	AX, p+32(FP)
	MOVQ	$0, err+40(FP)
	RET

// Call the function stored in _cgo_mmap using the GCC calling convention.
// This must be called on the system stack.
TEXT runtime·callCgoMmap(SB),NOSPLIT,$16
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVL	prot+16(FP), DX
	MOVL	flags+20(FP), CX
	MOVL	fd+24(FP), R8
	MOVL	off+28(FP), R9
	MOVQ	_cgo_mmap(SB), AX
	MOVQ	SP, BX
	ANDQ	$~15, SP	// alignment as per amd64 psABI
	MOVQ	BX, 0(SP)
	CALL	AX
	MOVQ	0(SP), SP
	MOVQ	AX, ret+32(FP)
	RET

TEXT runtime·sysMunmap(SB),NOSPLIT,$0
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVQ	$SYS_munmap, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$0xf1, 0xf1  // crash
	RET

// Call the function stored in _cgo_munmap using the GCC calling convention.
// This must be called on the system stack.
TEXT runtime·callCgoMunmap(SB),NOSPLIT,$16-16
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVQ	_cgo_munmap(SB), AX
	MOVQ	SP, BX
	ANDQ	$~15, SP	// alignment as per amd64 psABI
	MOVQ	BX, 0(SP)
	CALL	AX
	MOVQ	0(SP), SP
	RET

TEXT runtime·madvise(SB),NOSPLIT,$0
	MOVQ	addr+0(FP), DI
	MOVQ	n+8(FP), SI
	MOVL	flags+16(FP), DX
	MOVQ	$SYS_madvise, AX
	SYSCALL
	// ignore failure - maybe pages are locked
	RET

// int64 futex(int32 *uaddr, int32 op, int32 val,
//	struct timespec *timeout, int32 *uaddr2, int32 val2);
//wangyang 这里使用了 futex 实现了wait signal等功能 , 调用了 sys_futex 系统调用
TEXT runtime·futex(SB),NOSPLIT,$0
	MOVQ	addr+0(FP), DI
	MOVL	op+8(FP), SI
	MOVL	val+12(FP), DX
	MOVQ	ts+16(FP), R10
	MOVQ	addr2+24(FP), R8
	MOVL	val3+32(FP), R9
	MOVL	$SYS_futex, AX
	SYSCALL
	MOVL	AX, ret+40(FP)
	RET

/**
    非常重要 clone 系统调用，创建一个系统线程，并执行mstart 函数
*/
// int32 clone(int32 flags, void *stk, M *mp, G *gp, void (*fn)(void));
TEXT runtime·clone(SB),NOSPLIT,$0
	MOVL	flags+0(FP), DI //di=flags
	MOVQ	stk+8(FP), SI //si=stk
	MOVQ	$0, DX
	MOVQ	$0, R10

	// Copy mp, gp, fn off parent stack for use by child.
	// Careful: Linux system call clobbers CX and R11.
	MOVQ	mp+16(FP), R8 //r8 = mp
	MOVQ	gp+24(FP), R9 //r9 = gp (g0)
	MOVQ	fn+32(FP), R12 //r12 = fn

	MOVL	$SYS_clone, AX // --> linux 系统调用号
	SYSCALL //-->系统调用

    /**
        这里会返回2遍，如果结果是0 那么是子线程当中执行pc + 3 也就是往下3条开始执行
        如果不是，那么执行ret ,说明是在父线程当中
    */
	// In parent, return.
	CMPQ	AX, $0
	JEQ	3(PC)
	MOVL	AX, ret+40(FP) //--> 将ax 放到返回值的位置
	RET

	// In child, on new stack.
	MOVQ	SI, SP

	// If g or m are nil, skip Go-related setup.
	CMPQ	R8, $0    // m
	JEQ	nog
	CMPQ	R9, $0    // g
	JEQ	nog

	// Initialize m->procid to Linux tid
	MOVL	$SYS_gettid, AX //--> 使用ax寄存器 进行系统调用，结果放到ax寄存器中
	SYSCALL
	MOVQ	AX, m_procid(R8) //m.procid = ax (sys_gettid) 这里就可以看到 结果是从ax寄存器到内存一个位置

	// Set FS to point at m->tls.
	LEAQ	m_tls(R8), DI //--> 将m 的 tls地址设置到 [tls]寄存器（也就是fs）段的基址位置，方便取出
	CALL	runtime·settls(SB) // --> 调用settls字段，设置tls

	// In child, set up new stack
	get_tls(CX)
	MOVQ	R8, g_m(R9)
	MOVQ	R9, g(CX) //--> 将g0的值移动到 当前线程对应的m的局部变量位置，表示当前对应的g 也就是getg函数对应的g
	CALL	runtime·stackcheck(SB)

//--> 这里都会顺序往下执行
nog:
	// Call fn
	CALL	R12

	// It shouldn't return. If it does, exit that thread.
	MOVL	$111, DI
	MOVL	$SYS_exit, AX
	SYSCALL
	JMP	-3(PC)	// keep exiting

TEXT runtime·sigaltstack(SB),NOSPLIT,$-8
	MOVQ	new+0(FP), DI
	MOVQ	old+8(FP), SI
	MOVQ	$SYS_sigaltstack, AX
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001
	JLS	2(PC)
	MOVL	$0xf1, 0xf1  // crash
	RET

/**
    重要，这里是 设置 tls寄存器的位置
    FS寄存器指向当前活动线程的TEB结构（线程结构）
    偏移  说明
    000  指向SEH链指针
    004  线程堆栈顶部
    008  线程堆栈底部
    00C  SubSystemTib
    010  FiberData
    014  ArbitraryUserPointer
    018  FS段寄存器在内存中的镜像地址
    020  进程PID
    024  线程ID
    02C  指向线程局部存储指针
    030  PEB结构地址（进程结构）
    034  上个错误号

    从代码可以看到，这里通过arch_prctl系统调用把m0.tls[1]的地址设置成了fs段的段基址。
    CPU中有个叫fs的段寄存器与之对应，而每个线程都有自己的一组CPU寄存器值，
    操作系统在把线程调离CPU运行时会帮我们把所有寄存器中的值保存在内存中，
    调度线程起来运行时又会从内存中把这些寄存器的值恢复到CPU，这样，在此之后，
    工作线程代码就可以通过fs寄存器来找到m.tls，读者可以参考上面初始化tls之后对tls功能验证的代码来理解这一过程。

*/

/*
    wangyang ***  非常重要，这里是设置tls tls是个固定的位置，是基于fs 开始偏移的，这里将m0->tls[1]的地址设置到这里
    所以 TLS 本身就是 -8 是一个偏移值
*/
// set tls base to DI
//ARCH_SET_FS	Set the 64bit base for the FS register toaddr.
TEXT runtime·settls(SB),NOSPLIT,$32
#ifdef GOOS_android
	// Same as in sys_darwin_386.s:/ugliness, different constant.
	// DI currently holds m->tls, which must be fs:0x1d0.
	// See cgo/gcc_android_amd64.c for the derivation of the constant.
	SUBQ	$0x1d0, DI  // In android, the tls base
#else
	ADDQ	$8, DI	// ELF wants to use -8(FS)
#endif
//DI寄存器中存放的是m.tls[0]的地址，m的tls成员是一个数组，读者如果忘记了可以回头看一下m结构体的定义
//下面这一句代码把DI寄存器中的地址加8，为什么要+8呢，主要跟ELF可执行文件格式中的TLS实现的机制有关
//执行下面这句指令之后DI寄存器中的存放的就是m.tls[1]的地址了
/**
    这里的目的是将tls[1]的地址保存到主线程的线程局部变量中, tls 对应的是线程的局部存储初始地址
    也就是fs_base寄存器所存储的位置，默认是0 ，第一个变量的存储就是 0xffffff8(-8)这样的一个位置

    将tls[1]这个地址存放到线程的 局部偏移中

    下面函数实际上是调用 相应的 相应的系统调用 (sys_arch_prctl) 系统调用 ，
    下面的0x1002指的是 arch_set_fs系统调用

    int arch_prctl(int code, unsigned long addr);
    arch_prctl()设置特定于体系结构的进程或线程状态。代码选择一个子函数并将参数addr传递给它；
    对于 set 操作，addr解释为无符号long，对于 get 操作，解释为unsigned long *。
    x86和x86-64的子功能是：
    ARCH_SET_FS
    将FS寄存器的64位基址设置为addr。
    ARCH_GET_FS
    返回addr指向的无符号长整数中当前线程FS寄存器的64位基值。
    ARCH_SET_GS
    将GS寄存器的64位基址设置为addr。
    ARCH_GET_GS
    返回addr指向的无符号长整数中当前线程的GS寄存器的64位基值。
*/

/*
    所以这里有 三个 参数 ，系统调用号 在ax寄存器中，
    系统调用第一个参数 在di寄存器中
    系统调用第二个参数 在si寄存器中

    所以传递过来的是 tls[0]的地址，但是将tls[1]的地址设置成为 fs段基地址

    首先这个 函数的名称是 settls 也就是设置 tls地址，会将 tls[1]的地址设置成为 fs段基地址
    所以也就是 TLS地址

    这里的作用就是将 tls[1] 的地址设置成为 当前线程 thread local 基地址
    我们这里可以理解为 将 tls[1] 的地址设置成为 fs 段寄存器 基地址
*/
	MOVQ	DI, SI //SI存放arch_prctl系统调用的第二个参数
	MOVQ	$0x1002, DI	// ARCH_SET_FS // ARCH_SET_FS //arch_prctl的第一个参数 --> 系统调用号
	MOVQ	$SYS_arch_prctl, AX // --> 这个常量是158
	SYSCALL
	CMPQ	AX, $0xfffffffffffff001 //--> 进行比较
	JLS	2(PC)
	MOVL	$0xf1, 0xf1  // crash
	RET

TEXT runtime·osyield(SB),NOSPLIT,$0
	MOVL	$SYS_sched_yield, AX
	SYSCALL
	RET

TEXT runtime·sched_getaffinity(SB),NOSPLIT,$0
	MOVQ	pid+0(FP), DI
	MOVQ	len+8(FP), SI
	MOVQ	buf+16(FP), DX
	MOVL	$SYS_sched_getaffinity, AX
	SYSCALL
	MOVL	AX, ret+24(FP)
	RET

// int32 runtime·epollcreate(int32 size);
TEXT runtime·epollcreate(SB),NOSPLIT,$0
	MOVL    size+0(FP), DI
	MOVL    $SYS_epoll_create, AX
	SYSCALL
	MOVL	AX, ret+8(FP)
	RET

// int32 runtime·epollcreate1(int32 flags);
TEXT runtime·epollcreate1(SB),NOSPLIT,$0
	MOVL	flags+0(FP), DI
	MOVL	$SYS_epoll_create1, AX
	SYSCALL
	MOVL	AX, ret+8(FP)
	RET

// func epollctl(epfd, op, fd int32, ev *epollEvent) int
TEXT runtime·epollctl(SB),NOSPLIT,$0
	MOVL	epfd+0(FP), DI
	MOVL	op+4(FP), SI
	MOVL	fd+8(FP), DX
	MOVQ	ev+16(FP), R10
	MOVL	$SYS_epoll_ctl, AX
	SYSCALL
	MOVL	AX, ret+24(FP)
	RET

// int32 runtime·epollwait(int32 epfd, EpollEvent *ev, int32 nev, int32 timeout);
TEXT runtime·epollwait(SB),NOSPLIT,$0
	// This uses pwait instead of wait, because Android O blocks wait.
	MOVL	epfd+0(FP), DI
	MOVQ	ev+8(FP), SI
	MOVL	nev+16(FP), DX
	MOVL	timeout+20(FP), R10
	MOVQ	$0, R8
	MOVL	$SYS_epoll_pwait, AX
	SYSCALL
	MOVL	AX, ret+24(FP)
	RET

// void runtime·closeonexec(int32 fd);
TEXT runtime·closeonexec(SB),NOSPLIT,$0
	MOVL    fd+0(FP), DI  // fd
	MOVQ    $2, SI  // F_SETFD
	MOVQ    $1, DX  // FD_CLOEXEC
	MOVL	$SYS_fcntl, AX
	SYSCALL
	RET


// int access(const char *name, int mode)
TEXT runtime·access(SB),NOSPLIT,$0
	// This uses faccessat instead of access, because Android O blocks access.
	MOVL	$AT_FDCWD, DI // AT_FDCWD, so this acts like access
	MOVQ	name+0(FP), SI
	MOVL	mode+8(FP), DX
	MOVL	$0, R10
	MOVL	$SYS_faccessat, AX
	SYSCALL
	MOVL	AX, ret+16(FP)
	RET

// int connect(int fd, const struct sockaddr *addr, socklen_t addrlen)
TEXT runtime·connect(SB),NOSPLIT,$0-28
	MOVL	fd+0(FP), DI
	MOVQ	addr+8(FP), SI
	MOVL	len+16(FP), DX
	MOVL	$SYS_connect, AX
	SYSCALL
	MOVL	AX, ret+24(FP)
	RET

// int socket(int domain, int type, int protocol)
TEXT runtime·socket(SB),NOSPLIT,$0-20
	MOVL	domain+0(FP), DI
	MOVL	typ+4(FP), SI
	MOVL	prot+8(FP), DX
	MOVL	$SYS_socket, AX
	SYSCALL
	MOVL	AX, ret+16(FP)
	RET

// func sbrk0() uintptr
TEXT runtime·sbrk0(SB),NOSPLIT,$0-8
	// Implemented as brk(NULL).
	MOVQ	$0, DI
	MOVL	$SYS_brk, AX
	SYSCALL
	MOVQ	AX, ret+0(FP)
	RET
