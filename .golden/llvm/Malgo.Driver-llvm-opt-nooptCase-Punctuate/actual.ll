; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Punctuate.ll'
source_filename = "test/testcases/malgo/Punctuate.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str360 = unnamed_addr constant [2 x i8] c"x\00"
@str361 = unnamed_addr constant [2 x i8] c"y\00"
@str362 = unnamed_addr constant [2 x i8] c"z\00"
@str363 = unnamed_addr constant [1 x i8] zeroinitializer
@str427 = unnamed_addr constant [10 x i8] c"no branch\00"
@str430 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str431 = unnamed_addr constant [6 x i8] c"SInt \00"
@str432 = unnamed_addr constant [8 x i8] c"SList [\00"
@str433 = unnamed_addr constant [3 x i8] c", \00"
@str434 = unnamed_addr constant [2 x i8] c"]\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_237"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i": ; preds = %2
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

common.ret1:                                      ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i"
  %common.ret1.op = phi ptr [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i" ], [ %18, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i" ]
  ret ptr %common.ret1.op

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i": ; preds = %2
  %__0 = load ptr, ptr %0, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load ptr, ptr %__0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %__0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %6)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %14, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_237", ptr %let_func_0.i2.i, align 8
  %15 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_237"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i1.i, ptr nocapture nofree readonly %8)
  %16 = load ptr, ptr %13, align 8
  %17 = load ptr, ptr %let_func_0.i.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr %15)
  br label %common.ret1
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_356"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_357", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = tail call ptr @malgo_string_append(ptr %p_0.i.i, ptr %1)
  ret ptr %4
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_357"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#_0", i64 8
  %"string#_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val", ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_356", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_357", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %7 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i, ptr %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_422"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %__0 = load ptr, ptr %0, align 8
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %common.ret1

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i": ; preds = %2
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load i8, ptr %8, align 1
  %switch1.i = icmp eq i8 %9, 0
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i.i, align 8
  br i1 %switch1.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = load ptr, ptr %10, align 8
  %13 = load ptr, ptr %let_func_0.i.i, align 8
  %14 = tail call ptr %13(ptr %12, ptr nonnull %11)
  br label %common.ret1

common.ret1:                                      ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i", %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i"
  %common.ret1.op = phi ptr [ %23, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i" ], [ %4, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i" ], [ %14, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1.i" ]
  ret ptr %common.ret1.op

"switch_branch_runtime/malgo/Prelude.mlg.Cons_1.i": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i"
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i4.i, align 8
  store ptr %let_capture_0.i4.i, ptr %15, align 8
  %let_func_0.i5.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i5.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i6.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %__0, ptr %let_capture_0.i6.i, align 8
  store ptr %let_capture_0.i6.i, ptr %16, align 8
  %let_func_0.i7.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_422", ptr %let_func_0.i7.i, align 8
  %17 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_422"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i6.i, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %8)
  %18 = load ptr, ptr %15, align 8
  %19 = load ptr, ptr %let_func_0.i5.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr %17)
  %21 = load ptr, ptr %10, align 8
  %22 = load ptr, ptr %let_func_0.i.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr %20)
  br label %common.ret1
}

define internal fastcc ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$nil_974_0") unnamed_addr {
  %1 = load i8, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %2, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %12, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str363, ptr %3, align 8
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %0
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374", ptr %let_func_0.i, align 8
  %9 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree readonly %7)
  %10 = load ptr, ptr %8, align 8
  %11 = load ptr, ptr %let_func_0.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr %9)
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", align 1
  switch i8 %2, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0"
  ]

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
  %common.ret.op = phi ptr [ %13, %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ], [ %26, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ %50, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0": ; preds = %1
  %3 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str430, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374", ptr %let_func_0.i.i, align 8
  %"string#_0.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %8 = getelementptr i8, ptr %"string#_0.i", i64 8
  %"string#_0.val.i" = load ptr, ptr %8, align 8
  %9 = getelementptr i8, ptr %4, i64 8
  %.val.i = load ptr, ptr %9, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val.i", ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %10, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_356", ptr %let_func_0.i.i.i, align 8
  %x_0.i.i.i = load ptr, ptr %let_capture_0.i.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i.i, ptr %let_capture_0.i.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i, ptr %11, align 8
  %let_func_0.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_357", ptr %let_func_0.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i.i, align 8
  %12 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i.i, ptr %.val.i)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %12, ptr %14, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0": ; preds = %1
  %15 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr @str431, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i1 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i.i1, align 8
  store ptr %let_capture_0.i.i1, ptr %19, align 8
  %let_func_0.i.i2 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374", ptr %let_func_0.i.i2, align 8
  %20 = getelementptr i8, ptr %16, i64 4
  %.val = load i32, ptr %20, align 4
  %21 = tail call ptr @malgo_int32_t_to_string(i32 %.val)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = load ptr, ptr %19, align 8
  %25 = load ptr, ptr %let_func_0.i.i2, align 8
  %26 = tail call ptr %25(ptr %24, ptr nonnull %22)
  br label %common.ret

"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0": ; preds = %1
  %27 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %29, i64 0, i32 1, i32 0
  store ptr @str432, ptr %30, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %29, ptr %let_capture_0.i.i3, align 8
  store ptr %let_capture_0.i.i3, ptr %31, align 8
  %let_func_0.i.i4 = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374", ptr %let_func_0.i.i4, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr @str433, ptr %33, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %32, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %34, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_422", ptr %let_func_0.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.show", ptr %show_func_0, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %let_capture_0.i5, align 8
  store ptr %let_capture_0.i5, ptr %36, align 8
  %let_func_0.i6 = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_237", ptr %let_func_0.i6, align 8
  %37 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_237"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i5, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %28)
  %38 = load ptr, ptr %34, align 8
  %39 = load ptr, ptr %let_func_0.i, align 8
  %40 = tail call ptr %39(ptr %38, ptr %37)
  %41 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %40)
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %41, ptr %let_capture_0.i.i7, align 8
  store ptr %let_capture_0.i.i7, ptr %42, align 8
  %let_func_0.i.i8 = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_374", ptr %let_func_0.i.i8, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %43, i64 0, i32 1, i32 0
  store ptr @str434, ptr %44, align 8
  %45 = load ptr, ptr %42, align 8
  %46 = load ptr, ptr %let_func_0.i.i8, align 8
  %47 = tail call ptr %46(ptr %45, ptr nonnull %43)
  %48 = load ptr, ptr %31, align 8
  %49 = load ptr, ptr %let_func_0.i.i4, align 8
  %50 = tail call ptr %49(ptr %48, ptr %47)
  br label %common.ret

switch_default_0:                                 ; preds = %1
  unreachable
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str360, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %9, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %10, i64 0, i32 1, i32 0
  store ptr @str361, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %10, ptr %13, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %14, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i2.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr @str362, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %15, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %19, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i4.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %20, align 1
  %21 = load ptr, ptr %19, align 8
  %22 = load ptr, ptr %let_func_0.i4.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr nonnull %20)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %24, align 1
  %25 = getelementptr { i8, { ptr } }, ptr %24, i64 0, i32 1, i32 0
  store ptr %23, ptr %25, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %24, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %26, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i6.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %27, align 1
  %28 = load ptr, ptr %26, align 8
  %29 = load ptr, ptr %let_func_0.i6.i, align 8
  %30 = tail call ptr %29(ptr %28, ptr nonnull %27)
  %31 = load ptr, ptr %14, align 8
  %32 = load ptr, ptr %let_func_0.i2.i, align 8
  %33 = tail call ptr %32(ptr %31, ptr %30)
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %34, align 1
  %35 = getelementptr { i8, { ptr } }, ptr %34, i64 0, i32 1, i32 0
  store ptr %33, ptr %35, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %34, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %36, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_369", ptr %let_func_0.i8.i, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %37, align 1
  %38 = load ptr, ptr %36, align 8
  %39 = load ptr, ptr %let_func_0.i8.i, align 8
  %40 = tail call ptr %39(ptr %38, ptr nonnull %37)
  %41 = load ptr, ptr %9, align 8
  %42 = load ptr, ptr %let_func_0.i.i, align 8
  %43 = tail call ptr %42(ptr %41, ptr %40)
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %44, align 8
  %45 = getelementptr { i8, { ptr } }, ptr %44, i64 0, i32 1, i32 0
  store ptr %43, ptr %45, align 8
  %46 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr poison, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %44)
  %47 = getelementptr i8, ptr %46, i64 8
  %.val.i = load ptr, ptr %47, align 8
  %48 = tail call ptr @malgo_print_string(ptr %.val.i)
  %49 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %49, align 1
  %50 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %50, align 1
  %51 = tail call ptr @malgo_newline(ptr noundef nonnull %50)
  ret i32 0
}
