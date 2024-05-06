; ModuleID = '.malgo-work/test/testcases/malgo/Null.ll'
source_filename = "test/testcases/malgo/Null.mlg"

@str235 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Null.mlg.#let_closure_240"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/Null.mlg.#fun_closure_241"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/Null.mlg.False_0", label %"switch_branch_test/testcases/malgo/Null.mlg.True_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/Null.mlg.True_0", %"switch_branch_test/testcases/malgo/Null.mlg.False_0"
  %common.ret.op = phi ptr [ %4, %"switch_branch_test/testcases/malgo/Null.mlg.False_0" ], [ %6, %"switch_branch_test/testcases/malgo/Null.mlg.True_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/Null.mlg.False_0": ; preds = %2
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str235)
  br label %common.ret

"switch_branch_test/testcases/malgo/Null.mlg.True_0": ; preds = %2
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %5)
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %4, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 1
  store ptr %5, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %3, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %6, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %3, ptr %13, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_6.i, align 8
  store ptr %let_capture_6.i, ptr %15, align 8
  %let_func_3.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_240", ptr %let_func_3.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_6.i, ptr %16, align 8
  %fun_func_3.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#fun_closure_241", ptr %fun_func_3.i, align 8
  %17 = load ptr, ptr %15, align 8
  %18 = load ptr, ptr %let_func_3.i, align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %16)
  ret i32 0
}
