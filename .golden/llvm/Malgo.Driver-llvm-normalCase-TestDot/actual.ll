; ModuleID = 'test/testcases/malgo/TestDot.mlg'
source_filename = "test/testcases/malgo/TestDot.mlg"

declare void @GC_init()

declare ptr @malgo_panic(ptr)

declare ptr @malgo_unsafe_cast(ptr)

declare i32 @malgo_add_int32_t(i32, i32)

declare i32 @malgo_sub_int32_t(i32, i32)

declare i32 @malgo_mul_int32_t(i32, i32)

declare i32 @malgo_div_int32_t(i32, i32)

declare i64 @malgo_add_int64_t(i64, i64)

declare i64 @malgo_sub_int64_t(i64, i64)

declare i64 @malgo_mul_int64_t(i64, i64)

declare i64 @malgo_div_int64_t(i64, i64)

declare float @malgo_add_float(float, float)

declare float @malgo_sub_float(float, float)

declare float @malgo_mul_float(float, float)

declare float @malgo_div_float(float, float)

declare double @malgo_add_double(double, double)

declare double @malgo_sub_double(double, double)

declare double @malgo_mul_double(double, double)

declare double @malgo_div_double(double, double)

declare float @sqrtf(float)

declare double @sqrt(double)

declare i32 @malgo_eq_int32_t(i32, i32)

declare i32 @malgo_ne_int32_t(i32, i32)

declare i32 @malgo_lt_int32_t(i32, i32)

declare i32 @malgo_gt_int32_t(i32, i32)

declare i32 @malgo_le_int32_t(i32, i32)

declare i32 @malgo_ge_int32_t(i32, i32)

declare i32 @malgo_eq_int64_t(i64, i64)

declare i32 @malgo_ne_int64_t(i64, i64)

declare i32 @malgo_lt_int64_t(i64, i64)

declare i32 @malgo_gt_int64_t(i64, i64)

declare i32 @malgo_le_int64_t(i64, i64)

declare i32 @malgo_ge_int64_t(i64, i64)

declare i32 @malgo_eq_float(float, float)

declare i32 @malgo_ne_float(float, float)

declare i32 @malgo_lt_float(float, float)

declare i32 @malgo_gt_float(float, float)

declare i32 @malgo_le_float(float, float)

declare i32 @malgo_ge_float(float, float)

declare i32 @malgo_eq_double(double, double)

declare i32 @malgo_ne_double(double, double)

declare i32 @malgo_lt_double(double, double)

declare i32 @malgo_gt_double(double, double)

declare i32 @malgo_le_double(double, double)

declare i32 @malgo_ge_double(double, double)

declare i32 @malgo_eq_char(i8, i8)

declare i32 @malgo_ne_char(i8, i8)

declare i32 @malgo_lt_char(i8, i8)

declare i32 @malgo_gt_char(i8, i8)

declare i32 @malgo_le_char(i8, i8)

declare i32 @malgo_ge_char(i8, i8)

declare i32 @malgo_eq_string(ptr, ptr)

declare i32 @malgo_ne_string(ptr, ptr)

declare i32 @malgo_lt_string(ptr, ptr)

declare i32 @malgo_gt_string(ptr, ptr)

declare i32 @malgo_le_string(ptr, ptr)

declare i32 @malgo_ge_string(ptr, ptr)

declare i32 @malgo_char_ord(i8)

declare i32 @malgo_is_digit(i8)

declare i32 @malgo_is_lower(i8)

declare i32 @malgo_is_upper(i8)

declare i32 @malgo_is_alphanum(i8)

declare i64 @malgo_string_length(ptr)

declare i8 @malgo_string_at(i64, ptr)

declare ptr @malgo_string_cons(i8, ptr)

declare ptr @malgo_string_append(ptr, ptr)

declare ptr @malgo_substring(ptr, i64, i64)

declare ptr @malgo_int32_t_to_string(i32)

declare ptr @malgo_int64_t_to_string(i64)

declare ptr @malgo_float_to_string(float)

declare ptr @malgo_double_to_string(double)

declare ptr @malgo_char_to_string(i8)

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_newline(ptr)

declare ptr @malgo_print_char(i8)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_flush(ptr)

declare i8 @malgo_get_char(ptr)

declare ptr @malgo_get_contents(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1794_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1794_0", ptr %4, align 4
  ret ptr %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_add_int64_t_curry_1834"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1835_0", i64 %"runtime/malgo/Builtin.mlg.$p_1836_0") {
  %2 = call i64 @malgo_add_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1835_0", i64 %"runtime/malgo/Builtin.mlg.$p_1836_0")
  ret i64 %2
}

define internal i64 @"test/testcases/malgo/TestDot.mlg.#let_closure_2889"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @malgo_add_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3995_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3995_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_2889", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestDot.mlg.main"(ptr %0, ptr %"test/testcases/malgo/TestDot.mlg.$$__37_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %7 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i64 }, ptr %7, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  %10 = call ptr @"runtime/malgo/Builtin.mlg.addInt64#"(ptr null, i64 %9)
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call i64 %14(ptr %12, i64 1)
  %16 = call ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr null, i64 %15)
  %17 = getelementptr { i8, <8 x i8> }, ptr %16, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %19 = getelementptr { i8, { i64 } }, ptr %16, i32 0, i32 1
  %20 = getelementptr { i64 }, ptr %19, i32 0, i32 0
  %21 = load i64, ptr %20, align 4
  %22 = call ptr @"runtime/malgo/Builtin.mlg.addInt64#"(ptr null, i64 %21)
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call i64 %26(ptr %24, i64 1)
  %28 = call ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr null, i64 %27)
  %29 = getelementptr { i8, <8 x i8> }, ptr %28, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  %31 = getelementptr { i8, { i64 } }, ptr %28, i32 0, i32 1
  %32 = getelementptr { i64 }, ptr %31, i32 0, i32 0
  %33 = load i64, ptr %32, align 4
  %34 = call ptr @malgo_int64_t_to_string(i64 %33)
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %36 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 0
  store i8 0, ptr %36, align 1
  %37 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 1, i32 0
  store ptr %34, ptr %37, align 8
  %38 = getelementptr { i8, <8 x i8> }, ptr %35, i32 0, i32 0
  %39 = load i8, ptr %38, align 1
  switch i8 %39, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_2"
  %40 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 1
  %41 = getelementptr { ptr }, ptr %40, i32 0, i32 0
  %42 = load ptr, ptr %41, align 8
  %43 = call ptr @malgo_print_string(ptr %42)
  ret ptr %43

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_2"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal i64 @"test/testcases/malgo/TestDot.mlg.#let_closure_2890"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"test/testcases/malgo/TestDot.mlg.succ"(ptr %0, ptr %"test/testcases/malgo/TestDot.mlg.$int64#_30_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestDot.mlg.$int64#_30_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"test/testcases/malgo/TestDot.mlg.$int64#_30_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestDot.mlg.#let_closure_2890", ptr %let_func_0, align 8
  %8 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_add_int64_t_curry_1834"(ptr null, i64 %6, i64 1)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i64 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i64 } }, ptr %9, i32 0, i32 1, i32 0
  store i64 %8, ptr %11, align 4
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestDot.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TestDot.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestDot.mlg"() {
  ret void
}
