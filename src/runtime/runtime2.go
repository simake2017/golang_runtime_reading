// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"runtime/internal/atomic"
	"runtime/internal/sys"
	"unsafe"
)

// defined constants
const (
	// G status
	//
	// Beyond indicating the general state of a G, the G status
	// acts like a lock on the goroutine's stack (and hence its
	// ability to execute user code).
	//
	// If you add to this list, add to the list
	// of "okay during garbage collection" status
	// in mgcmark.go too.

	// _Gidle means this goroutine was just allocated and has not
	// yet been initialized.
	_Gidle = iota // 0

	// _Grunnable means this goroutine is on a run queue. It is
	// not currently executing user code. The stack is not owned.
	_Grunnable // 1

	// _Grunning means this goroutine may execute user code. The
	// stack is owned by this goroutine. It is not on a run queue.
	// It is assigned an M and a P.
	_Grunning // 2

	// _Gsyscall means this goroutine is executing a system call.
	// It is not executing user code. The stack is owned by this
	// goroutine. It is not on a run queue. It is assigned an M.
	_Gsyscall // 3

	// _Gwaiting means this goroutine is blocked in the runtime.
	// It is not executing user code. It is not on a run queue,
	// but should be recorded somewhere (e.g., a channel wait
	// queue) so it can be ready()d when necessary. The stack is
	// not owned *except* that a channel operation may read or
	// write parts of the stack under the appropriate channel
	// lock. Otherwise, it is not safe to access the stack after a
	// goroutine enters _Gwaiting (e.g., it may get moved).
	_Gwaiting // 4

	// _Gmoribund_unused is currently unused, but hardcoded in gdb
	// scripts.
	_Gmoribund_unused // 5

	// _Gdead means this goroutine is currently unused. It may be
	// just exited, on a free list, or just being initialized. It
	// is not executing user code. It may or may not have a stack
	// allocated. The G and its stack (if any) are owned by the M
	// that is exiting the G or that obtained the G from the free
	// list.
	_Gdead // 6

	// _Genqueue_unused is currently unused.
	_Genqueue_unused // 7

	// _Gcopystack means this goroutine's stack is being moved. It
	// is not executing user code and is not on a run queue. The
	// stack is owned by the goroutine that put it in _Gcopystack.
	_Gcopystack // 8

	// _Gscan combined with one of the above states other than
	// _Grunning indicates that GC is scanning the stack. The
	// goroutine is not executing user code and the stack is owned
	// by the goroutine that set the _Gscan bit.
	//
	// _Gscanrunning is different: it is used to briefly block
	// state transitions while GC signals the G to scan its own
	// stack. This is otherwise like _Grunning.
	//
	// atomicstatus&~Gscan gives the state the goroutine will
	// return to when the scan completes.
	_Gscan         = 0x1000
	_Gscanrunnable = _Gscan + _Grunnable // 0x1001
	_Gscanrunning  = _Gscan + _Grunning  // 0x1002
	_Gscansyscall  = _Gscan + _Gsyscall  // 0x1003
	_Gscanwaiting  = _Gscan + _Gwaiting  // 0x1004
)

const (
	// P status
	_Pidle    = iota
	_Prunning // Only this P is allowed to change from _Prunning.
	_Psyscall
	_Pgcstop
	_Pdead
)

// Mutual exclusion locks.  In the uncontended case,
// as fast as spin locks (just a few user-level instructions),
// but on the contention path they sleep in the kernel.
// A zeroed Mutex is unlocked (no need to initialize each lock).
type mutex struct {
	// Futex-based impl treats it as uint32 key,
	// while sema-based impl as M* waitm.
	// Used to be a union, but unions break precise GC.
	key uintptr
}

// sleep and wakeup on one-time events.
// before any calls to notesleep or notewakeup,
// must call noteclear to initialize the Note.
// then, exactly one thread can call notesleep
// and exactly one thread can call notewakeup (once).
// once notewakeup has been called, the notesleep
// will return.  future notesleep will return immediately.
// subsequent noteclear must be called only after
// previous notesleep has returned, e.g. it's disallowed
// to call noteclear straight after notewakeup.
//
// notetsleep is like notesleep but wakes up after
// a given number of nanoseconds even if the event
// has not yet happened.  if a goroutine uses notetsleep to
// wake up early, it must wait to call noteclear until it
// can be sure that no other goroutine is calling
// notewakeup.
//
// notesleep/notetsleep are generally called on g0,
// notetsleepg is similar to notetsleep but is called on user g.
// 一次性事件的睡眠和唤醒。在任何对notesleep或notewakeup的调用之前，必须调用noteclear来初始化Note。
// 那么，正好一个线程可以调用notesleep，而一个线程可以调用notewakeup（一次）。一旦调用了notewakeup，noteleep将返回。
// 未来的noteleep将立即返回。必须在之前的noteleep返回之后调用后续noteclear，例如注意到唤醒之后不允许直接调用noteclear。
//
// notetsleep就像是 notesleep ，但是在给定的纳秒数后唤醒，即使事件尚未发生。如果goroutine使用notetsleep提前唤醒，
// 它必须等待调用noteclear，直到可以确定没有其他goroutine正在调用notewakeup。 notesleep/notetsleep 通常在g0上调用，
// notetsleepg类似于notetsleep但在用户g上调用。
type note struct {
	// Futex-based impl treats it as uint32 key,
	// while sema-based impl as M* waitm.
	// Used to be a union, but unions break precise GC.
	key uintptr
}

/**
	wangyang 函数结构体内容 ，每个函数由该结构体来实现
	*******
	golang 的类型是依赖于 类型推断，Java中的类型是包含在具体对象中的，在header 里面通过 metadata 指向对应的类型对象Class 实例
	golang 也比较类似，比如所有的函数 的kind 是 func ，都是由funcval结构体来描述的，funcval 里面的fn指向可以用来获取一个
	具体的函数信息 包含在 funcInfo 中 , 函数的类型 会由funcType来描述，

	举例来说，比如一个 函数或者一个 结构体 type student struct{} ，student 可能有很多，而且内存布局都一样，
	但是他们都有一个共同的 structtype 也就是 对象实例 来描述这个类型 ,
	slice func 包括 interface 等等都是一样的，但是他们跟Java区别的地方在于 他的实例对象中不包含 metadata指针
	而是在编译期确认的，所以不能像Java这种 将一个子类型 struct 转为父类型 struct


 */
type funcval struct {
	fn uintptr
	// variable-size, fn-specific data here
}

/**
	wangyang iface
 */
type iface struct {
	tab  *itab
	data unsafe.Pointer
}

type eface struct {
	_type *_type
	data  unsafe.Pointer
}

func efaceOf(ep *interface{}) *eface {
	return (*eface)(unsafe.Pointer(ep))
}

// The guintptr, muintptr, and puintptr are all used to bypass write barriers.
// It is particularly important to avoid write barriers when the current P has
// been released, because the GC thinks the world is stopped, and an
// unexpected write barrier would not be synchronized with the GC,
// which can lead to a half-executed write barrier that has marked the object
// but not queued it. If the GC skips the object and completes before the
// queuing can occur, it will incorrectly free the object.
//
// We tried using special assignment functions invoked only when not
// holding a running P, but then some updates to a particular memory
// word went through write barriers and some did not. This breaks the
// write barrier shadow checking mode, and it is also scary: better to have
// a word that is completely ignored by the GC than to have one for which
// only a few updates are ignored.
//
// Gs and Ps are always reachable via true pointers in the
// allgs and allp lists or (during allocation before they reach those lists)
// from stack variables.
//
// Ms are always reachable via true pointers either from allm or
// freem. Unlike Gs and Ps we do free Ms, so it's important that
// nothing ever hold an muintptr across a safe point.

// A guintptr holds a goroutine pointer, but typed as a uintptr
// to bypass write barriers. It is used in the Gobuf goroutine state
// and in scheduling lists that are manipulated without a P.
//
// The Gobuf.g goroutine pointer is almost always updated by assembly code.
// In one of the few places it is updated by Go code - func save - it must be
// treated as a uintptr to avoid a write barrier being emitted at a bad time.
// Instead of figuring out how to emit the write barriers missing in the
// assembly manipulation, we change the type of the field to uintptr,
// so that it does not require write barriers at all.
//
// Goroutine structs are published in the allg list and never freed.
// That will keep the goroutine structs from being collected.
// There is never a time that Gobuf.g's contain the only references
// to a goroutine: the publishing of the goroutine in allg comes first.
// Goroutine pointers are also kept in non-GC-visible places like TLS,
// so I can't see them ever moving. If we did want to start moving data
// in the GC, we'd need to allocate the goroutine structs from an
// alternate arena. Using guintptr doesn't make that problem any worse.
type guintptr uintptr

//go:nosplit
func (gp guintptr) ptr() *g { return (*g)(unsafe.Pointer(gp)) }

//go:nosplit
func (gp *guintptr) set(g *g) { *gp = guintptr(unsafe.Pointer(g)) }

//go:nosplit
func (gp *guintptr) cas(old, new guintptr) bool {
	return atomic.Casuintptr((*uintptr)(unsafe.Pointer(gp)), uintptr(old), uintptr(new))
}

// setGNoWB performs *gp = new without a write barrier.
// For times when it's impractical to use a guintptr.
//go:nosplit
//go:nowritebarrier
func setGNoWB(gp **g, new *g) {
	(*guintptr)(unsafe.Pointer(gp)).set(new)
}

// 用二级指针操作单向链表
//
/*
        p1                 p2
	+-------+           +-------+
	|   id  |<-----+    |   id  |<------ pidleList
	+-------+      |    +-------+
	| link  |      +----| link  |
	+-------+           +-------+
*/
type puintptr uintptr

//go:nosplit
func (pp puintptr) ptr() *p { return (*p)(unsafe.Pointer(pp)) }

//go:nosplit
func (pp *puintptr) set(p *p) { *pp = puintptr(unsafe.Pointer(p)) }

// muintptr is a *m that is not tracked by the garbage collector.
//
// Because we do free Ms, there are some additional constrains on
// muintptrs:
//
// 1. Never hold an muintptr locally across a safe point.
//
// 2. Any muintptr in the heap must be owned by the M itself so it can
//    ensure it is not in use when the last true *m is released.
type muintptr uintptr

//go:nosplit
func (mp muintptr) ptr() *m { return (*m)(unsafe.Pointer(mp)) }

//go:nosplit
func (mp *muintptr) set(m *m) { *mp = muintptr(unsafe.Pointer(m)) }

// setMNoWB performs *mp = new without a write barrier.
// For times when it's impractical to use an muintptr.
//go:nosplit
//go:nowritebarrier
func setMNoWB(mp **m, new *m) {
	(*muintptr)(unsafe.Pointer(mp)).set(new)
}

// g 的运行现场
type gobuf struct {
	// The offsets of sp, pc, and g are known to (hard-coded in) libmach.
	//
	// ctxt is unusual with respect to GC: it may be a
	// heap-allocated funcval, so GC needs to track it, but it
	// needs to be set and cleared from assembly, where it's
	// difficult to have write barriers. However, ctxt is really a
	// saved, live register, and we only ever exchange it between
	// the real register and the gobuf. Hence, we treat it as a
	// root during stack scanning, which means assembly that saves
	// and restores it doesn't need write barriers. It's still
	// typed as a pointer so that any other writes from Go get
	// write barriers.
	sp   uintptr //记录cpu rsp寄存器的值
	pc   uintptr //记录cpu rip寄存器的值
	g    guintptr //记录对应 goroutine结构体的指针  对应着  之前调度的g 结构体指针
	ctxt unsafe.Pointer
	ret  sys.Uintreg //记录对应系统调用的返回值
	lr   uintptr
	bp   uintptr // for GOEXPERIMENT=framepointer //-->记录对应的bp 栈帧栈底指针
}

// sudog represents a g in a wait list, such as for sending/receiving
// on a channel.
//
// sudog is necessary because the g ↔ synchronization object relation
// is many-to-many. A g can be on many wait lists, so there may be
// many sudogs for one g; and many gs may be waiting on the same
// synchronization object, so there may be many sudogs for one object.
//
// sudogs are allocated from a special pool. Use acquireSudog and
// releaseSudog to allocate and free them.
//
// sudog 代表在等待列表里的 g，比如向 channel 发送/接收内容时
// 之所以需要 sudog 是因为 g 和同步对象之间的关系是多对多的
// 一个 g 可能会在多个等待队列中，所以一个 g 可能被打包为多个 sudog
// 多个 g 也可以等待在同一个同步对象上
// 因此对于一个同步对象就会有很多 sudog 了
// sudog 是从一个特殊的池中进行分配的。用 acquireSudog 和 releaseSudog 来分配和释放 sudog
type sudog struct {
	// The following fields are protected by the hchan.lock of the
	// channel this sudog is blocking on. shrinkstack depends on
	// this for sudogs involved in channel ops.

	g *g

	// isSelect indicates g is participating in a select, so
	// g.selectDone must be CAS'd to win the wake-up race.
	isSelect bool
	next     *sudog
	prev     *sudog
	elem     unsafe.Pointer // data element (may point to stack)

	// The following fields are never accessed concurrently.
	// For channels, waitlink is only accessed by g.
	// For semaphores, all fields (including the ones above)
	// are only accessed when holding a semaRoot lock.

	acquiretime int64
	releasetime int64
	ticket      uint32
	parent      *sudog // semaRoot binary tree
	waitlink    *sudog // g.waiting list or semaRoot
	waittail    *sudog // semaRoot
	c           *hchan // channel
}

type libcall struct {
	fn   uintptr
	n    uintptr // number of parameters
	args uintptr // parameters
	r1   uintptr // return values
	r2   uintptr
	err  uintptr // error number
}

// describes how to handle callback
type wincallbackcontext struct {
	gobody       unsafe.Pointer // go function to call
	argsize      uintptr        // callback arguments size (in bytes)
	restorestack uintptr        // adjust stack on return by (in bytes) (386 only)
	cleanstack   bool
}

// Stack describes a Go execution stack.
// The bounds of the stack are exactly [lo, hi),
// with no implicit data structures on either side.
// stack 描述了Go执行堆栈。堆栈的边界正好是 [lo,hi)，两边都没有隐式数据结构。
/**
	用于描述 栈的数据结构
 */
type stack struct {
	lo uintptr 	//-->low 低地址指向栈顶
	hi uintptr 	//--> high 高地址 指向栈底
}

type g struct {
	// Stack parameters.
	// stack describes the actual stack memory: [stack.lo, stack.hi).
	// stackguard0 is the stack pointer compared in the Go stack growth prologue.
	// It is stack.lo+StackGuard normally, but can be StackPreempt to trigger a preemption.
	// stackguard1 is the stack pointer compared in the C stack growth prologue.
	// It is stack.lo+StackGuard on g0 and gsignal stacks.
	// It is ~0 on other goroutine stacks, to trigger a call to morestackc (and crash).
	// 简单数据结构，lo 和 hi 成员描述了栈的下界和上界内存地址
	stack       stack   // offset known to runtime/cgo -->记录该goroutine使用的栈
	// 下面两个成员用于栈溢出检查，实现栈的自动伸缩，抢占调度也会用到stackguard0
	stackguard0 uintptr // offset known to liblink
	stackguard1 uintptr // offset known to liblink

	_panic *_panic // innermost panic - offset known to liblink
	_defer *_defer // innermost defer
	// 当前的m --> 当前对应的m 被哪个工作线程执行
	m *m // current m; offset known to arm liblink
	// goroutine切换时，用于保存g的上下文 -->这里对应的类型是gobuf，保存的是
	//相应的寄存器 也就是 上下文的值
	/**
		wangyang @@@@ 也就是说，当前的这个g是从哪个 g的什么位置切换到当前
		的g 的
	 */
	sched     gobuf
	syscallsp uintptr // if status==Gsyscall, syscallsp = sched.sp to use during gc
	syscallpc uintptr // if status==Gsyscall, syscallpc = sched.pc to use during gc
	stktopsp  uintptr // expected sp at top of stack, to check in traceback
	// 用于传递参数，睡眠时其他goroutine可以设置param，唤醒时该goroutine可以获取
	param        unsafe.Pointer // passed parameter on wakeup
	atomicstatus uint32
	stackLock    uint32 // sigprof/scang lock; TODO: fold in to atomicstatus
	// 唯一的goroutine的ID
	goid int64
	// g被阻塞的大体时间
	waitsince  int64  // approx time when the g become blocked
	waitreason string // if status==Gwaiting
	// schedlink字段指向全局运行队列中的下一个g，
	//所有位于全局运行队列中的g形成一个链表
	/**
		如果当前goroutine位于全局队列，则该字段指向队列中下一个goroutine

		因为有的goroutine是从全局队列中获取的，这种情况下，如果这个goroutine被p给捕获了
		那么这个goroutine就会加入到p的gfree中去，但是这个字段仍然需要去指向全局队列中的
		goroutine, 方便窃取
	 */
	schedlink  guintptr
	// 标记是否可抢占
	// 抢占调度标志，如果需要抢占调度，设置preempt为true
	preempt        bool     // preemption signal, duplicates stackguard0 = stackpreempt
	paniconfault   bool     // panic (instead of crash) on unexpected fault address
	preemptscan    bool     // preempted g does scan for gc
	gcscandone     bool     // g has scanned stack; protected by _Gscan bit in status
	gcscanvalid    bool     // false at start of gc cycle, true if G has not run since last scan; TODO: remove?
	throwsplit     bool     // must not split stack
	raceignore     int8     // ignore race detection events
	sysblocktraced bool     // StartTrace has emitted EvGoInSyscall about this goroutine
	sysexitticks   int64    // cputicks when syscall has returned (for tracing)
	traceseq       uint64   // trace event sequencer
	tracelastp     puintptr // last P emitted an event for this goroutine
	// G被锁定只在这个m上运行
	lockedm  muintptr
	sig      uint32
	writebuf []byte
	sigcode0 uintptr
	sigcode1 uintptr
	sigpc    uintptr
	// 调用者的 PC/IP
	gopc uintptr // pc of go statement that created this goroutine
	// 任务函数
	startpc    uintptr // pc of goroutine function
	racectx    uintptr
	waiting    *sudog         // sudog structures this g is waiting on (that have a valid elem ptr); in lock order
	cgoCtxt    []uintptr      // cgo traceback context
	labels     unsafe.Pointer // profiler labels
	timer      *timer         // cached timer for time.Sleep
	selectDone uint32         // are we participating in a select and did someone win the race?

	// Per-G GC state

	// gcAssistBytes is this G's GC assist credit in terms of
	// bytes allocated. If this is positive, then the G has credit
	// to allocate gcAssistBytes bytes without assisting. If this
	// is negative, then the G must correct this by performing
	// scan work. We track this in bytes to make it fast to update
	// and check for debt in the malloc hot path. The assist ratio
	// determines how this corresponds to scan work debt.
	gcAssistBytes int64
}

/**

	这里猜测是这样的 每个 goroutine 的创建也是在栈区，创建的时候每个栈的初始大小是2k
	在进行切换的时候 会不断进行切换，每条goroutine 会关联一条thread ，可以获取相应的
	thread的tls，相应的g0栈的位置保存在 tls里面，在切换的时候通过g0栈执行筛选切换逻辑
	然后跳转到另一个栈，然后进行切换，当栈溢出的时候，使用的技术是整栈复制

 */
type m struct {
	// 用来执行调度指令的 goroutine ,m中自己的代码，用来调度属于m的goroutine，该部分代码通常在sysstack 执行
	// g0主要用来记录工作线程使用的栈信息，在执行调度代码时需要使用这个栈
	// 执行用户goroutine代码时，使用用户goroutine自己的栈，调度时会发生栈的切换
	g0      *g     // goroutine with scheduling stack
	morebuf gobuf  // gobuf arg to morestack
	divmod  uint32 // div/mod denominator for arm - known to liblink

	// Fields not known to debuggers.
	procid uint64 // for debuggers, but offset not hard-coded
	// 处理信号的goroutine
	gsignal    *g           // signal-handling g
	goSigStack gsignalStack // Go-allocated signal handling stack
	sigmask    sigset       // storage for saved signal mask
	// thread-local storage
	//--> 这里是 线程局部存储 对应到当前线程
	tls      [6]uintptr // thread-local storage (for x86 extern register)
	mstartfn func()
	// 当前运行的goroutine --> 当前正在运行中的user goroutine
	curg      *g       // current running goroutine
	caughtsig guintptr // goroutine running during fatal signal
	// 关联p和执行的go代码
	p     puintptr // attached p for executing go code (nil if not executing go code)
	nextp puintptr
	id    int64
	// 状态
	mallocing  int32
	throwing   int32
	preemptoff string // if != "", keep curg running on this m
	// locks表示该M是否被锁的状态，M被锁的状态下该M无法执行gc
	locks     int32
	softfloat int32
	dying     int32
	profilehz int32
	helpgc    int32
	// 是否自旋，自旋就表示M正在找G来运行
	/**
		从其他地方寻找 goroutine , 这就是类似fork join的原理
	 */
	// spinning状态：表示当前工作线程正在试图从其它工作线程的本地运行队列偷取goroutine
	spinning bool // m is out of work and is actively looking for work
	// m是否被阻塞 --> 比如阻塞在系统调用
	blocked bool // m is blocked on a note
	// m是否在执行写屏蔽
	inwb        bool // m is executing a write barrier
	newSigstack bool // minit on C thread called sigaltstack
	printlock   int8
	// m在执行cgo吗
	incgo      bool   // m is executing a cgo call
	freeWait   uint32 // if == 0, safe to free g0 and delete m (atomic)
	fastrand   [2]uint32
	needextram bool
	traceback  uint8
	// 当前cgo调用的数目
	ncgocall uint64 // number of cgo calls in total
	// cgo调用的总数
	ncgo          int32       // number of cgo calls currently in progress
	cgoCallersUse uint32      // if non-zero, cgoCallers in use temporarily
	cgoCallers    *cgoCallers // cgo traceback if crashing in cgo call
	// 没有goroutine需要运行时，工作线程睡眠在这个park成员上，
	// 其它线程通过这个park唤醒该工作线程
	/**
		线程 休眠技术，类似Java线程的 unsafe.park与unsafe.unpark 等等
	 */
	park          note
	// 用于链接allm
	// 记录所有工作线程的一个链表
	/**
		所有的工作线程 形成一个链表  通过这个来记录
	 */
	alllink   *m // on allm
	schedlink muintptr
	// 当前m的内存缓存
	mcache *mcache
	// 锁定g在当前m上执行，而不会切换到其他m，一般cgo调用或者手动调用LockOSThread()才会有值
	lockedg guintptr
	// thread创建的栈
	createstack [32]uintptr // stack that created this thread.
	freglo      [16]uint32  // d[i] lsb and f[i]
	freghi      [16]uint32  // d[i] msb and f[i+16]
	fflag       uint32      // floating point compare flags
	// 用户锁定M的标记
	lockedExt uint32 // tracking for external LockOSThread
	// runtime 内部锁定M的标记
	lockedInt     uint32         // tracking for internal lockOSThread
	nextwaitm     muintptr       // next m waiting for lock
	waitunlockf   unsafe.Pointer // todo go func(*g, unsafe.pointer) bool
	waitlock      unsafe.Pointer
	waittraceev   byte
	waittraceskip int
	startingtrace bool
	syscalltick   uint32
	/**
		linux 系统下，该值对应线程的id
	 */
	thread        uintptr // thread handle
	freelink      *m      // on sched.freem

	// these are here because they are too large to be on the stack
	// of low-level NOSPLIT functions.
	libcall   libcall
	libcallpc uintptr // for cpu profiler
	libcallsp uintptr
	libcallg  guintptr
	syscall   libcall // stores syscall parameters on windows

	mOS
}

/**
	p结构体用于保存工作线程执行go代码时所必需的资源，比如goroutine的运行队列，内存分配用到的缓存等等

	每个m 会关联一个p ，g队列都是跟m绑定的， 最早的版本是没有p的，g队列挂在m上面，但是当执行阻塞系统调用时
	所有的g都会等待，所以才有了p，m在调度的时候，从p上寻找相应的g

 */
type p struct {
	lock mutex

	// id也是allp的数组下标
	id     int32
	status uint32 // one of pidle/prunning/...
	// 单向链表，指向下一个P的地址
	link puintptr
	// 每调度一次加1
	schedtick uint32 // incremented on every scheduler call
	// 每一次系统调用加1
	syscalltick uint32     // incremented on every system call
	sysmontick  sysmontick // last tick observed by sysmon
	// 回链到关联的m
	m       muintptr // back-link to associated m (nil if idle)
	mcache  *mcache
	racectx uintptr

	deferpool    [5][]*_defer // pool of available defer structs of different sizes (see panic.go)
	deferpoolbuf [5][32]*_defer

	// Cache of goroutine ids, amortizes accesses to runtime·sched.goidgen.
	// goroutine的ID的缓存
	goidcache    uint64
	goidcacheend uint64

	// Queue of runnable goroutines. Accessed without lock.
	// 可运行的goroutine的队列
	//本地goroutine运行队列 头部
	runqhead uint32
	runqtail uint32
	runq     [256]guintptr
	// runnext, if non-nil, is a runnable G that was ready'd by
	// the current G and should be run next instead of what's in
	// runq if there's time remaining in the running G's time
	// slice. It will inherit the time left in the current time
	// slice. If a set of goroutines is locked in a
	// communicate-and-wait pattern, this schedules that set as a
	// unit and eliminates the (potentially large) scheduling
	// latency that otherwise arises from adding the ready'd
	// goroutines to the end of the run queue.
	// 下一个运行的g，优先级最高
	runnext guintptr

	// Available G's (status == Gdead)
	/**
		用于记录已经退出的空闲的 g（状态为status ==Gdead）
	 */
	gfree    *g
	gfreecnt int32

	sudogcache []*sudog
	sudogbuf   [128]*sudog

	tracebuf traceBufPtr

	// traceSweep indicates the sweep events should be traced.
	// This is used to defer the sweep start event until a span
	// has actually been swept.
	traceSweep bool
	// traceSwept and traceReclaimed track the number of bytes
	// swept and reclaimed by sweeping in the current sweep loop.
	traceSwept, traceReclaimed uintptr

	palloc persistentAlloc // per-P to avoid mutex

	// Per-P GC state
	gcAssistTime         int64 // Nanoseconds in assistAlloc
	gcFractionalMarkTime int64 // Nanoseconds in fractional mark worker
	gcBgMarkWorker       guintptr
	gcMarkWorkerMode     gcMarkWorkerMode

	// gcMarkWorkerStartTime is the nanotime() at which this mark
	// worker started.
	gcMarkWorkerStartTime int64

	// gcw is this P's GC work buffer cache. The work buffer is
	// filled by write barriers, drained by mutator assists, and
	// disposed on certain GC state transitions.
	gcw gcWork

	// wbBuf is this P's GC write barrier buffer.
	//
	// TODO: Consider caching this in the running G.
	wbBuf wbBuf

	runSafePointFn uint32 // if 1, run sched.safePointFn at next safe point

	pad [sys.CacheLineSize]byte
}

/**
重要全局变量
allgs     []*g     // 保存所有的g
allm       *m    // 所有的m构成的一个链表，包括下面的m0
allp       []*p    // 保存所有的p，len(allp) == gomaxprocs

ncpu             int32   // 系统中cpu核的数量，程序启动时由runtime代码初始化
gomaxprocs int32   // p的最大值，默认等于ncpu，但可以通过GOMAXPROCS修改

sched      schedt     // 调度器结构体对象，记录了调度器的工作状态

m0  m       // 代表进程的主线程
g0   g        // m0的g0，也就是m0.g0 = &g0
 */

/**
	schedt结构体用来保存调度器的状态信息和goroutine的全局运行队列：

	用于保存全局运行队列

 */
type schedt struct {
	// accessed atomically. keep at top to ensure alignment on 32-bit systems.
	goidgen  uint64
	lastpoll uint64

	lock mutex

	// When increasing nmidle, nmidlelocked, nmsys, or nmfreed, be
	// sure to call checkdead().
	// idle状态的m
	// 由空闲的工作线程组成链表 都是链表形式的 结构
	midle muintptr // idle m's waiting for work
	// idle状态的m个数 --> 空闲工作线程的数量
	nmidle int32 // number of idle m's waiting for work
	// lockde状态的m个数
	nmidlelocked int32 // number of locked m's waiting for work
	mnext        int64 // number of m's that have been created and next M ID
	// m允许的最大个数
	maxmcount int32 // maximum number of m's allowed (or die)
	nmsys     int32 // number of system m's not counted for deadlock
	nmfreed   int64 // cumulative number of freed m's

	// 系统中goroutine的数目，会自动更新
	ngsys uint32 // number of system goroutines; updated atomically

	// idle的p列表
	// 由空闲的p结构体对象组成的链表 --> 同样是链表结构
	pidle puintptr // idle p's
	// 有多少个状态为idle的p
	npidle uint32
	// 有多少个m自旋
	nmspinning uint32 // See "Worker thread parking/unparking" comment in proc.go.

	// Global runnable queue.
	// 全局的可运行的g队列
	runqhead guintptr // --> 头
	runqtail guintptr //--> 尾部
	// 全局队列的大小
	runqsize int32

	// Global cache of dead G's.
	// dead的G的全局缓存
	gflock       mutex
	gfreeStack   *g
	gfreeNoStack *g
	ngfree       int32

	// Central cache of sudog structs.
	// sudog的缓存中心
	sudoglock  mutex
	sudogcache *sudog

	// Central pool of available defer structs of different sizes.
	deferlock mutex
	deferpool [5]*_defer

	// freem is the list of m's waiting to be freed when their
	// m.exited is set. Linked through m.freelink.
	freem *m

	gcwaiting  uint32 // gc is waiting to run
	stopwait   int32
	stopnote   note
	sysmonwait uint32
	sysmonnote note

	// safepointFn should be called on each P at the next GC
	// safepoint if p.runSafePointFn is set.
	safePointFn   func(*p)
	safePointWait int32
	safePointNote note

	profilehz int32 // cpu profiling rate

	procresizetime int64 // nanotime() of last change to gomaxprocs
	totaltime      int64 // ∫gomaxprocs dt up to procresizetime
}

// Values for the flags field of a sigTabT.
const (
	_SigNotify   = 1 << iota // let signal.Notify have signal, even if from kernel
	_SigKill                 // if signal.Notify doesn't take it, exit quietly
	_SigThrow                // if signal.Notify doesn't take it, exit loudly
	_SigPanic                // if the signal is from the kernel, panic
	_SigDefault              // if the signal isn't explicitly requested, don't monitor it
	_SigGoExit               // cause all runtime procs to exit (only used on Plan 9).
	_SigSetStack             // add SA_ONSTACK to libc handler
	_SigUnblock              // always unblock; see blockableSig
	_SigIgn                  // _SIG_DFL action is to ignore the signal
)

// Layout of in-memory per-function information prepared by linker
// See https://golang.org/s/go12symtab.
// Keep in sync with linker (../cmd/link/internal/ld/pcln.go:/pclntab)
// and with package debug/gosym and with symtab.go in package runtime.
/**
wangyang 重要 函数内存布局，在 funcInfo 中使用
 */
type _func struct {
	entry   uintptr // start pc
	nameoff int32   // function name

	args   int32  // in/out args size
	funcID funcID // set for certain special runtime functions

	pcsp      int32
	pcfile    int32
	pcln      int32
	npcdata   int32
	nfuncdata int32
}

// layout of Itab known to compilers
// allocated in non-garbage-collected memory
// Needs to be in sync with
// ../cmd/compile/internal/gc/reflect.go:/^func.dumptypestructs.
type itab struct {
	inter *interfacetype
	_type *_type
	hash  uint32 // copy of _type.hash. Used for type switches.
	_     [4]byte
	fun   [1]uintptr // variable sized. fun[0]==0 means _type does not implement inter.
}

// Lock-free stack node.
// // Also known to export_test.go.
type lfnode struct {
	next    uint64
	pushcnt uintptr
}

type forcegcstate struct {
	lock mutex
	g    *g
	idle uint32
}

// startup_random_data holds random bytes initialized at startup. These come from
// the ELF AT_RANDOM auxiliary vector (vdso_linux_amd64.go or os_linux_386.go).
var startupRandomData []byte

// extendRandom extends the random numbers in r[:n] to the whole slice r.
// Treats n<0 as n==0.
func extendRandom(r []byte, n int) {
	if n < 0 {
		n = 0
	}
	for n < len(r) {
		// Extend random bits using hash function & time seed
		w := n
		if w > 16 {
			w = 16
		}
		h := memhash(unsafe.Pointer(&r[n-w]), uintptr(nanotime()), uintptr(w))
		for i := 0; i < sys.PtrSize && n < len(r); i++ {
			r[n] = byte(h)
			n++
			h >>= 8
		}
	}
}

// A _defer holds an entry on the list of deferred calls.
// If you add a field here, add code to clear it in freedefer.
type _defer struct {
	siz     int32
	started bool
	sp      uintptr // sp at time of defer
	pc      uintptr
	fn      *funcval
	_panic  *_panic // panic that is running defer
	link    *_defer
}

// panics
type _panic struct {
	argp      unsafe.Pointer // pointer to arguments of deferred call run during panic; cannot move - known to liblink
	arg       interface{}    // argument to panic
	link      *_panic        // link to earlier panic
	recovered bool           // whether this panic is over
	aborted   bool           // the panic was aborted
}

// stack traces
type stkframe struct {
	fn       funcInfo   // function being run
	pc       uintptr    // program counter within fn
	continpc uintptr    // program counter where execution can continue, or 0 if not
	lr       uintptr    // program counter at caller aka link register
	sp       uintptr    // stack pointer at pc
	fp       uintptr    // stack pointer at caller aka frame pointer
	varp     uintptr    // top of local variables
	argp     uintptr    // pointer to function arguments
	arglen   uintptr    // number of bytes at argp
	argmap   *bitvector // force use of this argmap
}

const (
	_TraceRuntimeFrames = 1 << iota // include frames for internal runtime functions.
	_TraceTrap                      // the initial PC, SP are from a trap, not a return PC from a call
	_TraceJumpStack                 // if traceback is on a systemstack, resume trace at g that called into it
)

// The maximum number of frames we print for a traceback
const _TracebackMaxFrames = 100

var (
	allglen    uintptr
	allm       *m
	allp       []*p  // len(allp) == gomaxprocs; may change at safe points, otherwise immutable
	allpLock   mutex // Protects P-less reads of allp and all writes
	gomaxprocs int32
	ncpu       int32
	forcegc    forcegcstate
	sched      schedt
	newprocs   int32

	// Information about what cpu features are available.
	// Set on startup in asm_{386,amd64,amd64p32}.s.
	// Packages outside the runtime should not use these
	// as they are not an external api.
	processorVersionInfo uint32
	isIntel              bool
	lfenceBeforeRdtsc    bool
	support_aes          bool
	support_avx          bool
	support_avx2         bool
	support_bmi1         bool
	support_bmi2         bool
	support_erms         bool
	support_osxsave      bool
	support_popcnt       bool
	support_sse2         bool
	support_sse41        bool
	support_sse42        bool
	support_ssse3        bool

	goarm                uint8 // set by cmd/link on arm systems
	framepointer_enabled bool  // set by cmd/link
)

// Set by the linker so the runtime can determine the buildmode.
var (
	islibrary bool // -buildmode=c-shared
	isarchive bool // -buildmode=c-archive
)
