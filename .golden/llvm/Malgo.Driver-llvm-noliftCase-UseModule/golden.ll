; ModuleID = 'test/testcases/malgo/UseModule.mlg'
source_filename = "test/testcases/malgo/UseModule.mlg"

@str2938 = unnamed_addr constant [14 x i8] c"Hello, world!\00"

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

define internal i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0")
  ret i32 %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_2934"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1808_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1808_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2934", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_2936"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %"int32#_0", i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %"int32#_0", i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %10 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2936", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i32 %17(ptr %15, i32 %12)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 1, i32 0
  store i32 %18, ptr %21, align 4
  ret ptr %19

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$i_773_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_2937"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %malgo_add_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 1
  %malgo_add_int32_t_0 = load ptr, ptr %malgo_add_int32_t_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %1)
  ret i32 %12
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.succ"(ptr %0, ptr %"test/testcases/malgo/UseModule.mlg.$x_36_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/UseModule.mlg.$x_36_0", i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %7 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/UseModule.mlg.$x_36_0", i32 0, i32 1
  %8 = getelementptr { i32 }, ptr %7, i32 0, i32 0
  %9 = load i32, ptr %8, align 4
  %10 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %12 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %13 = getelementptr { i32 }, ptr %12, i32 0, i32 0
  %14 = load i32, ptr %13, align 4
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 0
  store i32 %9, ptr %p_0, align 4
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %malgo_add_int32_t_capture_0, align 8
  %malgo_add_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0, align 8
  %malgo_add_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %16, ptr %malgo_add_int32_t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2937", ptr %let_func_0, align 8
  %17 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %9, i32 %14)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2939"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %cast_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %cast_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %1)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.main"(ptr %0, ptr %"test/testcases/malgo/UseModule.mlg.$$__42_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str2938, ptr %4, align 8
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %7 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_print_string(ptr %9)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_newline(ptr %13)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printInt32_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %printInt32_capture_0, align 8
  %printInt32_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %16, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2939", ptr %let_func_0, align 8
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 1, ptr %20, align 4
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addInt32_capture_0 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  store ptr null, ptr %addInt32_capture_0, align 8
  %addInt32_func_0 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0, align 8
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %18)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, i32 1)
  %33 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 0
  %34 = load ptr, ptr %33, align 8
  %35 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = call ptr %36(ptr %34, ptr %32)
  %38 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addInt32_capture_1 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 0
  store ptr null, ptr %addInt32_capture_1, align 8
  %addInt32_func_1 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_1, align 8
  %39 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 0
  %40 = load ptr, ptr %39, align 8
  %41 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 1
  %42 = load ptr, ptr %41, align 8
  %43 = call ptr %42(ptr %40, ptr %37)
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_1" = getelementptr { ptr, ptr }, ptr %44, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_1", align 8
  %"Int32#_func_1" = getelementptr { ptr, ptr }, ptr %44, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1", align 8
  %45 = getelementptr { ptr, ptr }, ptr %44, i32 0, i32 0
  %46 = load ptr, ptr %45, align 8
  %47 = getelementptr { ptr, ptr }, ptr %44, i32 0, i32 1
  %48 = load ptr, ptr %47, align 8
  %49 = call ptr %48(ptr %46, i32 1)
  %50 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 0
  %51 = load ptr, ptr %50, align 8
  %52 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 1
  %53 = load ptr, ptr %52, align 8
  %54 = call ptr %53(ptr %51, ptr %49)
  %55 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = call ptr %58(ptr %56, ptr %54)
  ret ptr %59

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/UseModule.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/UseModule.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/UseModule.mlg"() {
  ret void
}
