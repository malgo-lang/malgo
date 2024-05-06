; ModuleID = '.malgo-work/test/testcases/malgo/NestedMatch.ll'
source_filename = "test/testcases/malgo/NestedMatch.mlg"

@str62 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/NestedMatch.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/NestedMatch.mlg.$p_36_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"test/testcases/malgo/NestedMatch.mlg.$p_36_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/NestedMatch.mlg.malgo_exit_failure"(ptr nocapture nofree readnone %0, ptr %"test/testcases/malgo/NestedMatch.mlg.$p_37_0") {
  %2 = tail call ptr @malgo_exit_failure(ptr %"test/testcases/malgo/NestedMatch.mlg.$p_37_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_61"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/NestedMatch.mlg.False_0", label %"switch_branch_test/testcases/malgo/NestedMatch.mlg.True_0"

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/NestedMatch.mlg.True_0", %"switch_branch_test/testcases/malgo/NestedMatch.mlg.False_0"
  %malgo_print_string_0.sink3 = phi ptr [ %malgo_print_string_0, %"switch_branch_test/testcases/malgo/NestedMatch.mlg.True_0" ], [ %malgo_exit_failure_0, %"switch_branch_test/testcases/malgo/NestedMatch.mlg.False_0" ]
  %str62.sink = phi ptr [ @str62, %"switch_branch_test/testcases/malgo/NestedMatch.mlg.True_0" ], [ %8, %"switch_branch_test/testcases/malgo/NestedMatch.mlg.False_0" ]
  %4 = load ptr, ptr %malgo_print_string_0.sink3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0.sink3, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %str62.sink)
  ret ptr %7

"switch_branch_test/testcases/malgo/NestedMatch.mlg.False_0": ; preds = %2
  %malgo_exit_failure_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %malgo_exit_failure_0 = load ptr, ptr %malgo_exit_failure_addr_0, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  br label %common.ret

"switch_branch_test/testcases/malgo/NestedMatch.mlg.True_0": ; preds = %2
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  br label %common.ret
}

define internal noundef ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_63"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %2 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %2, 0
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %switch to i8
  store i8 %spec.select, ptr %3, align 1
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_64"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
common.ret:
  %2 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %2, 0
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %switch to i8
  store i8 %spec.select, ptr %3, align 1
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/NestedMatch.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_exit_failure_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/NestedMatch.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0.i, align 8
  %malgo_exit_failure_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %5, ptr %malgo_exit_failure_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_61", ptr %fun_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %6, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_63", ptr %fun_func_1.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_4.i, ptr %7, align 8
  %fun_func_2.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/NestedMatch.mlg.#fun_closure_64", ptr %fun_func_2.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %8, align 1
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %fun_func_2.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  %12 = load ptr, ptr %6, align 8
  %13 = load ptr, ptr %fun_func_1.i, align 8
  %14 = tail call ptr %13(ptr %12, ptr %11)
  %15 = load ptr, ptr %3, align 8
  %16 = load ptr, ptr %fun_func_0.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr %14)
  ret i32 0
}
