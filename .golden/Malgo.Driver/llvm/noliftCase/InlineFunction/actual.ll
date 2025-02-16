; ModuleID = 'test/testcases/malgo/InlineFunction.mlg'
source_filename = "test/testcases/malgo/InlineFunction.mlg"

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

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3412"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$addInt32_curry_4048"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3412", ptr %let_func_0, align 8
  %13 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr null, i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0")
  ret i32 %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3413"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leInt32_curry_2853"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3413", ptr %let_func_0, align 8
  %13 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int32_t_curry_1932"(ptr null, i32 %6, i32 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leInt32#_curry_2732"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2733_0", i32 %"runtime/malgo/Builtin.mlg.$y_2734_0") {
  %2 = call i32 @malgo_le_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_2733_0", i32 %"runtime/malgo/Builtin.mlg.$y_2734_0")
  ret i32 %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3414"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3414", ptr %let_func_0, align 8
  %13 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr null, i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2293_0", i32 %"runtime/malgo/Builtin.mlg.$y_2294_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_2293_0", i32 %"runtime/malgo/Builtin.mlg.$y_2294_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int32_t_curry_1932"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1933_0", i32 %"runtime/malgo/Builtin.mlg.$p_1934_0") {
  %2 = call i32 @malgo_le_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1933_0", i32 %"runtime/malgo/Builtin.mlg.$p_1934_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0")
  ret i32 %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.False"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.True"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3415"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2287_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2287_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3415", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3416"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_le_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2727_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2727_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3416", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.isTrue#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0") {
  switch i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0", label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %1
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2

switch-unboxed_default_0:                         ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3417"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4027_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_4027_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3417", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = call ptr @malgo_newline(ptr %10)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3420"(ptr %0, ptr %1) {
  %fix_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %fix_0 = load ptr, ptr %fix_addr_0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %fix_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %fix_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %f_0)
  %8 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %1)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3419"(ptr %0, ptr %1) {
  %fix_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %fix_0 = load ptr, ptr %fix_addr_0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fix_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %fix_0, ptr %fix_1, align 8
  %f_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %f_0, ptr %f_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3420", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %3)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %1)
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3418"(ptr %0, ptr %1) {
  %fix_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %fix_0 = load ptr, ptr %fix_addr_0, align 8
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fix_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %fix_0, ptr %fix_1, align 8
  %f_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %f_0, ptr %f_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3419", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %3)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %1)
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.fix"(ptr %0, ptr %"test/testcases/malgo/InlineFunction.mlg.$f_112_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fix_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %fix_capture_0, align 8
  %fix_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0, align 8
  %fix_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %3, ptr %fix_0, align 8
  %f_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$f_112_0", ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3418", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3422"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leInt32#_curry_2732"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3421"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %x_0, i32 0, i32 1
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3422", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i32 %17(ptr %15, i32 %12)
  switch i32 %18, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %19 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %19

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %20 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %20

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.<="(ptr %0, ptr %"test/testcases/malgo/InlineFunction.mlg.$x_128_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_128_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3421", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3424"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3423"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %x_0, i32 0, i32 1
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3424", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.-"(ptr %0, ptr %"test/testcases/malgo/InlineFunction.mlg.$x_140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_140_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3423", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3426"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3425"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %x_0, i32 0, i32 1
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
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3426", ptr %let_func_0, align 8
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

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.+"(ptr %0, ptr %"test/testcases/malgo/InlineFunction.mlg.$x_152_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/InlineFunction.mlg.$x_152_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3425", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3429"(ptr %0, ptr %1) {
  %"leInt32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"leInt32#_0" = load ptr, ptr %"leInt32#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %n_0, i32 0, i32 1
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
  %13 = getelementptr { ptr, ptr }, ptr %"leInt32#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"leInt32#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i32 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3430"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3432"(ptr %0, ptr %1) {
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %n_0, i32 0, i32 1
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
  %13 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i32 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3433"(ptr %0, ptr %1) {
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %n_0, i32 0, i32 1
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
  %13 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"subInt32#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i32 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3434"(ptr %0, ptr %1) {
  %"addInt32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"addInt32#_0" = load ptr, ptr %"addInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %d_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %d_0, i32 0, i32 1
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
  %13 = getelementptr { ptr, ptr }, ptr %"addInt32#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"addInt32#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i32 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3431"(ptr %0, ptr %1) {
  %"addInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"addInt32#_0" = load ptr, ptr %"addInt32#_addr_0", align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %f_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt32#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"subInt32#_0", ptr %"subInt32#_1", align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %n_1 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %n_0, ptr %n_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3432", ptr %let_func_0, align 8
  %7 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %n_0, ptr %3)
  %8 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, { i32 } }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { i32 } }, ptr %13, i32 0, i32 1, i32 0
  store i32 2, ptr %15, align 4
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt32#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %"subInt32#_0", ptr %"subInt32#_2", align 8
  %"Int32#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_2", align 8
  %n_2 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 2
  store ptr %n_0, ptr %n_2, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3433", ptr %let_func_1, align 8
  %17 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %n_0, ptr %13)
  %18 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %"addInt32#_0", ptr %"addInt32#_1", align 8
  %"Int32#_3" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 1
  store ptr %"Int32#_0", ptr %"Int32#_3", align 8
  %d_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 2
  store ptr %12, ptr %d_0, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3434", ptr %let_func_2, align 8
  %24 = call ptr @"runtime/malgo/Builtin.mlg.$addInt32_curry_4048"(ptr null, ptr %12, ptr %22)
  ret ptr %24
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3428"(ptr %0, ptr %1) {
  %"addInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"addInt32#_0" = load ptr, ptr %"addInt32#_addr_0", align 8
  %"leInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"leInt32#_0" = load ptr, ptr %"leInt32#_addr_0", align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %f_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 5
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"leInt32#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"leInt32#_0", ptr %"leInt32#_1", align 8
  %"isTrue#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"isTrue#_0", ptr %"isTrue#_1", align 8
  %n_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %1, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3429", ptr %let_func_0, align 8
  %7 = call ptr @"runtime/malgo/Builtin.mlg.$leInt32_curry_2853"(ptr null, ptr %1, ptr %3)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3430", ptr %fun_func_0, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %"addInt32#_0", ptr %"addInt32#_1", align 8
  %"subInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %"subInt32#_0", ptr %"subInt32#_1", align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 2
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %f_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 3
  store ptr %f_0, ptr %f_1, align 8
  %n_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 4
  store ptr %1, ptr %n_1, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3431", ptr %fun_func_1, align 8
  %10 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %2
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, {} }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %2
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, {} }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %22 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 0
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1, i32 0
  store i32 1, ptr %23, align 4
  ret ptr %21

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3427"(ptr %0, ptr %1) {
  %"addInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"addInt32#_0" = load ptr, ptr %"addInt32#_addr_0", align 8
  %"leInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"leInt32#_0" = load ptr, ptr %"leInt32#_addr_0", align 8
  %"subInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"subInt32#_0" = load ptr, ptr %"subInt32#_addr_0", align 8
  %"Int32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %"Int32#_0" = load ptr, ptr %"Int32#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"addInt32#_0", ptr %"addInt32#_1", align 8
  %"leInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"leInt32#_0", ptr %"leInt32#_1", align 8
  %"subInt32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %"subInt32#_0", ptr %"subInt32#_1", align 8
  %"Int32#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 3
  store ptr %"Int32#_0", ptr %"Int32#_1", align 8
  %"isTrue#_1" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 4
  store ptr %"isTrue#_0", ptr %"isTrue#_1", align 8
  %f_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 5
  store ptr %1, ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3428", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3435"(ptr %0, ptr %1) {
  %fix_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %fix_0 = load ptr, ptr %fix_addr_0, align 8
  %fun_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %fun_0 = load ptr, ptr %fun_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %fix_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %fix_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %fun_0)
  %8 = getelementptr { ptr, ptr }, ptr %fun_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %fun_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %1)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3436"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/InlineFunction.mlg.main"(ptr %0, ptr %"test/testcases/malgo/InlineFunction.mlg.$$__164_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_0", align 8
  %"addInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32#", ptr %"addInt32#_func_0", align 8
  %"addInt32#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %"addInt32#_0", align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %"leInt32#_capture_0", align 8
  %"leInt32#_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt32#", ptr %"leInt32#_func_0", align 8
  %"leInt32#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %4, ptr %"leInt32#_0", align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %"subInt32#_capture_0", align 8
  %"subInt32#_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32#", ptr %"subInt32#_func_0", align 8
  %"subInt32#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %5, ptr %"subInt32#_0", align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %"Int32#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %6, ptr %"Int32#_0", align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %"isTrue#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 4
  store ptr %7, ptr %"isTrue#_0", align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#fun_closure_3427", ptr %fun_func_0, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { i32 } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %8, i32 0, i32 1, i32 0
  store i32 5, ptr %10, align 4
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fix_capture_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %fix_capture_0, align 8
  %fix_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.fix", ptr %fix_func_0, align 8
  %fix_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %12, ptr %fix_0, align 8
  %fun_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %2, ptr %fun_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3435", ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %8)
  %23 = getelementptr { i8, <4 x i8> }, ptr %22, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %25 = getelementptr { i8, { i32 } }, ptr %22, i32 0, i32 1
  %26 = getelementptr { i32 }, ptr %25, i32 0, i32 0
  %27 = load i32, ptr %26, align 4
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_0, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0, align 8
  %29 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, i32 %27)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1, i32 0
  store ptr %33, ptr %36, align 8
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %34, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  store ptr @"test/testcases/malgo/InlineFunction.mlg.#let_closure_3436", ptr %let_func_1, align 8
  %38 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  %39 = load ptr, ptr %38, align 8
  %40 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %43 = call ptr %41(ptr %39, ptr %42)
  ret ptr %43

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/InlineFunction.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/InlineFunction.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/InlineFunction.mlg"() {
  ret void
}
