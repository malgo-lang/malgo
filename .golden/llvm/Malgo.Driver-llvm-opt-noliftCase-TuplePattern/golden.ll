; ModuleID = '.malgo-work/test/testcases/malgo/TuplePattern.ll'
source_filename = "test/testcases/malgo/TuplePattern.mlg"

@str2835 = unnamed_addr constant [2 x i8] c"A\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_2833"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %cast_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_2834"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_Tuple#_0":
  %2 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0"
    i8 2, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0"
  ]

"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0": ; preds = %"switch_branch_Tuple#_0"
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str2835, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr noundef nonnull @str2835)
  br label %common.ret

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0"
  %common.ret.op = phi ptr [ %7, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0" ], [ %8, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0" ], [ %9, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0": ; preds = %"switch_branch_Tuple#_0"
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  br label %common.ret

"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0": ; preds = %"switch_branch_Tuple#_0"
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  br label %common.ret

switch_default_1:                                 ; preds = %"switch_branch_Tuple#_0"
  unreachable
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %5, i64 0, i32 1, i32 1
  store ptr %4, ptr %7, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %8, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_2833", ptr %let_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %9, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_2834", ptr %fun_func_0.i, align 8
  %10 = load ptr, ptr %8, align 8
  %11 = load ptr, ptr %let_func_0.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %9)
  ret i32 0
}
