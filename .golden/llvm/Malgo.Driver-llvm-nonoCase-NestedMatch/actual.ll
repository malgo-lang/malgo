; ModuleID = './test/testcases/malgo/NestedMatch.mlg'
source_filename = "./test/testcases/malgo/NestedMatch.mlg"

@str62 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @NestedMatch.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.False1(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.True1(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.False2(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.True2(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @NestedMatch.malgo_print_string(ptr %0, ptr %"NestedMatch.$p_36_0") {
  %2 = call ptr @malgo_print_string(ptr %"NestedMatch.$p_36_0")
  ret ptr %2
}

define internal ptr @NestedMatch.malgo_exit_failure(ptr %0, ptr %"NestedMatch.$p_37_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"NestedMatch.$p_37_0")
  ret ptr %2
}

define internal ptr @"NestedMatch.#fun_closure_61"(ptr %0, ptr %1) {
  %malgo_exit_failure_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %malgo_exit_failure_0 = load ptr, ptr %malgo_exit_failure_addr_0, align 8
  %malgo_print_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False_0
    i8 1, label %switch_branch_NestedMatch.True_0
  ]

switch_branch_NestedMatch.False_0:                ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { ptr, ptr }, ptr %malgo_exit_failure_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %malgo_exit_failure_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %6)
  ret ptr %12

switch_branch_NestedMatch.True_0:                 ; preds = %2
  %13 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %14 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr @str62)
  ret ptr %18

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"NestedMatch.#fun_closure_63"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False1_0
    i8 1, label %switch_branch_NestedMatch.True1_0
  ]

switch_branch_NestedMatch.False1_0:               ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @NestedMatch.True(ptr null)
  ret ptr %6

switch_branch_NestedMatch.True1_0:                ; preds = %2
  %7 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %8 = call ptr @NestedMatch.False(ptr null)
  ret ptr %8

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"NestedMatch.#fun_closure_64"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_NestedMatch.False2_0
    i8 1, label %switch_branch_NestedMatch.True2_0
  ]

switch_branch_NestedMatch.False2_0:               ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @NestedMatch.True1(ptr null)
  ret ptr %6

switch_branch_NestedMatch.True2_0:                ; preds = %2
  %7 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %8 = call ptr @NestedMatch.False1(ptr null)
  ret ptr %8

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @NestedMatch.main(ptr %0, ptr %"NestedMatch.$$__38_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_exit_failure_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_exit_failure_capture_0, align 8
  %malgo_exit_failure_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @NestedMatch.malgo_exit_failure, ptr %malgo_exit_failure_func_0, align 8
  %malgo_exit_failure_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %malgo_exit_failure_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @NestedMatch.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %malgo_print_string_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %4, ptr %malgo_print_string_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"NestedMatch.#fun_closure_61", ptr %fun_func_0, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"NestedMatch.#fun_closure_63", ptr %fun_func_1, align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_5 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %fun_capture_4, ptr %fun_capture_5, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"NestedMatch.#fun_closure_64", ptr %fun_func_2, align 8
  %7 = call ptr @NestedMatch.True2(ptr null)
  %8 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  %18 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_NestedMatch()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @NestedMatch.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_NestedMatch() {
  ret void
}
