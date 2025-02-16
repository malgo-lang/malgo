; ModuleID = '.malgo-work/test/testcases/malgo/TuplePattern.ll'
source_filename = "test/testcases/malgo/TuplePattern.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str206 = unnamed_addr constant [1 x i8] zeroinitializer
@str207 = unnamed_addr constant [2 x i8] c"A\00"
@str271 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_205"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %.val.val = load i8, ptr %.val, align 1
  switch i8 %.val.val, label %switch_default_0.i [
    i8 0, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0.i"
    i8 1, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0.i"
    i8 2, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0.i"
  ]

"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0.i": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str207, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str207)
  br label %"test/testcases/malgo/TuplePattern.mlg.$raw_fun_76.exit"

"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0.i": ; preds = %2
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  br label %"test/testcases/malgo/TuplePattern.mlg.$raw_fun_76.exit"

"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0.i": ; preds = %2
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  br label %"test/testcases/malgo/TuplePattern.mlg.$raw_fun_76.exit"

switch_default_0.i:                               ; preds = %2
  unreachable

"test/testcases/malgo/TuplePattern.mlg.$raw_fun_76.exit": ; preds = %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0.i", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0.i", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0.i"
  %common.ret.op.i = phi ptr [ %6, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0.i" ], [ %7, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0.i" ], [ %8, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_210"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 1
  store ptr %6, ptr %9, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_210", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %11, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_205", ptr %fun_func_0.i, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = load ptr, ptr %let_func_0.i.i, align 8
  %14 = tail call ptr %13(ptr %12, ptr nonnull %11)
  ret i32 0
}
