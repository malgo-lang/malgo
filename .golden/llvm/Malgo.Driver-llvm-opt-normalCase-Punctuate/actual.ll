; ModuleID = '.malgo-work/test/testcases/malgo/Punctuate.ll'
source_filename = "test/testcases/malgo/Punctuate.mlg"

@str3171 = unnamed_addr constant [2 x i8] c"x\00"
@str3172 = unnamed_addr constant [2 x i8] c"y\00"
@str3173 = unnamed_addr constant [2 x i8] c"z\00"
@str3175 = unnamed_addr constant [1 x i8] zeroinitializer
@str3177 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str3179 = unnamed_addr constant [6 x i8] c"SInt \00"
@str3181 = unnamed_addr constant [8 x i8] c"SList [\00"
@str3182 = unnamed_addr constant [3 x i8] c", \00"
@str3183 = unnamed_addr constant [2 x i8] c"]\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal fastcc noundef ptr @"runtime/malgo/Prelude.mlg.$mapList_curry_790"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %"runtime/malgo/Prelude.mlg.$__791_0", ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$nil_792_0") unnamed_addr {
  %1 = load i8, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %2, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %12, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %0
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", i64 0, i32 1, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = load ptr, ptr %"runtime/malgo/Prelude.mlg.$__791_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__791_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %4)
  %11 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.$mapList_curry_790"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %"runtime/malgo/Prelude.mlg.$__791_0", ptr nocapture nofree readonly %6)
  %12 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %12, align 1
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %10, ptr %13, align 8
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i64 0, i32 1, i32 1
  store ptr %11, ptr %14, align 8
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3170"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3176"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %p_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3170", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = tail call ptr @malgo_string_append(ptr %p_0.i.i, ptr %1)
  ret ptr %4
}

define internal fastcc noundef ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$nil_974_0") unnamed_addr {
  %1 = load i8, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %common.ret1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret1:                                      ; preds = %0, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %.sink = phi ptr [ %14, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ], [ @str3175, %0 ]
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %.sink, ptr %3, align 8
  ret ptr %2

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %0
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree readonly %7)
  %9 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %13, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3176", ptr %let_func_0, align 8
  %14 = tail call ptr @malgo_string_append(ptr %10, ptr %12)
  br label %common.ret1
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3178"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3180"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3184"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3185"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0") {
  %2 = load i8, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", align 1
  switch i8 %2, label %switch_default_9 [
    i8 0, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0"
  ]

"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0": ; preds = %1
  %3 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str3177, ptr %6, align 8
  %7 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr @str3177, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %9, align 8
  br label %common.ret

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0", %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0"
  %.sink = phi ptr [ %43, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ], [ %23, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ %9, %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ]
  %"test/testcases/malgo/Punctuate.mlg.#let_closure_3185.sink" = phi ptr [ @"test/testcases/malgo/Punctuate.mlg.#let_closure_3185", %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ], [ @"test/testcases/malgo/Punctuate.mlg.#let_closure_3180", %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ @"test/testcases/malgo/Punctuate.mlg.#let_closure_3178", %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ]
  %let_capture_6.sink = phi ptr [ %let_capture_6, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ], [ %let_capture_2, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ %let_capture_0, %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ]
  %.sink7 = phi ptr [ %39, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0" ], [ %19, %"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0" ], [ %8, %"switch_branch_test/testcases/malgo/Punctuate.mlg.Symbol_0" ]
  %let_func_3 = getelementptr { ptr, ptr }, ptr %.sink, i64 0, i32 1
  store ptr %"test/testcases/malgo/Punctuate.mlg.#let_closure_3185.sink", ptr %let_func_3, align 8
  %p_0.i3 = load ptr, ptr %let_capture_6.sink, align 8
  %10 = tail call ptr @malgo_string_append(ptr %p_0.i3, ptr %.sink7)
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  ret ptr %11

"switch_branch_test/testcases/malgo/Punctuate.mlg.SInt_0": ; preds = %1
  %13 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr @str3179, ptr %16, align 8
  %17 = getelementptr { i8, { i32 } }, ptr %14, i64 0, i32 1
  %18 = load i32, ptr %17, align 4
  %19 = tail call ptr @malgo_int32_t_to_string(i32 %18)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr %19, ptr %21, align 8
  %22 = load ptr, ptr %16, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %22, ptr %let_capture_2, align 8
  store ptr %let_capture_2, ptr %23, align 8
  br label %common.ret

"switch_branch_test/testcases/malgo/Punctuate.mlg.SList_0": ; preds = %1
  %24 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Punctuate.mlg.$symbol_120_0", i64 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr @str3181, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %28, align 8
  %29 = getelementptr { i8, { ptr } }, ptr %28, i64 0, i32 1, i32 0
  store ptr @str3182, ptr %29, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.show", ptr %show_func_0, align 8
  %31 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.$mapList_curry_790"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %30, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %25)
  %32 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.$punctuate_curry_746"(ptr nofree noundef nonnull align 8 dereferenceable(1) %28, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %31)
  %33 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %32)
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %34, align 1
  %35 = getelementptr { i8, { ptr } }, ptr %34, i64 0, i32 1, i32 0
  store ptr @str3183, ptr %35, align 8
  %36 = getelementptr { i8, { ptr } }, ptr %33, i64 0, i32 1
  %37 = load ptr, ptr %36, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %37, ptr %let_capture_4, align 8
  store ptr %let_capture_4, ptr %38, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"test/testcases/malgo/Punctuate.mlg.#let_closure_3184", ptr %let_func_2, align 8
  %p_0.i2 = load ptr, ptr %let_capture_4, align 8
  %39 = tail call ptr @malgo_string_append(ptr %p_0.i2, ptr noundef nonnull @str3183)
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %40, i64 0, i32 1, i32 0
  store ptr %39, ptr %41, align 8
  %42 = load ptr, ptr %27, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %42, ptr %let_capture_6, align 8
  store ptr %let_capture_6, ptr %43, align 8
  br label %common.ret

switch_default_9:                                 ; preds = %1
  unreachable
}

define internal fastcc noundef ptr @"runtime/malgo/Prelude.mlg.$punctuate_curry_746"(ptr nofree noundef nonnull align 8 dereferenceable(1) %"runtime/malgo/Prelude.mlg.$__747_0", ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$nil_748_0") unnamed_addr {
  %1 = load i8, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1"
  %common.ret.op = phi ptr [ %16, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1" ], [ %2, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %9, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %0
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", i64 0, i32 1, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = load i8, ptr %6, align 1
  %switch1 = icmp eq i8 %7, 0
  br i1 %switch1, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_1":  ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %4, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %8, ptr %11, align 8
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_1": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %12 = tail call fastcc ptr @"runtime/malgo/Prelude.mlg.$punctuate_curry_746"(ptr nofree noundef nonnull align 8 dereferenceable(1) %"runtime/malgo/Prelude.mlg.$__747_0", ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %6)
  %13 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$__747_0", ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 1
  store ptr %12, ptr %15, align 8
  %16 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %4, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 1
  store ptr %13, ptr %18, align 8
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str3171, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str3172, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr @str3173, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %11, ptr %14, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %13, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 1
  store ptr %15, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %16, ptr %20, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %22, align 1
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %19, ptr %23, align 8
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 1
  store ptr %21, ptr %24, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %25, align 1
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %25, i64 0, i32 1, i32 0
  store ptr %9, ptr %26, align 8
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %25, i64 0, i32 1, i32 1
  store ptr %22, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %28, i64 0, i32 1, i32 0
  store ptr %25, ptr %29, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  %31 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %31, align 1
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %31, i64 0, i32 1, i32 0
  store ptr %28, ptr %32, align 8
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %31, i64 0, i32 1, i32 1
  store ptr %30, ptr %33, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %34, align 1
  %35 = getelementptr { i8, { ptr, ptr } }, ptr %34, i64 0, i32 1, i32 0
  store ptr %5, ptr %35, align 8
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %34, i64 0, i32 1, i32 1
  store ptr %31, ptr %36, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 2, ptr %37, align 8
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %34, ptr %38, align 8
  %39 = tail call ptr @"test/testcases/malgo/Punctuate.mlg.show"(ptr poison, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %37)
  %40 = getelementptr { i8, { ptr } }, ptr %39, i64 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = tail call ptr @malgo_print_string(ptr %41)
  %43 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %43, align 1
  %44 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %44, align 1
  %45 = tail call ptr @malgo_newline(ptr noundef nonnull %44)
  ret i32 0
}
