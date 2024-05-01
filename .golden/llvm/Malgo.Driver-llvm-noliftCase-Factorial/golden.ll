; ModuleID = 'test/testcases/malgo/Factorial.mlg'
source_filename = "test/testcases/malgo/Factorial.mlg"

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

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3263"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqInt64_curry_3628"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3263", ptr %let_func_0, align 8
  %13 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int64_t_curry_1944"(ptr null, i64 %6, i64 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqInt64#_curry_3612"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3613_0", i64 %"runtime/malgo/Builtin.mlg.$y_3614_0") {
  %2 = call i32 @malgo_eq_int64_t(i64 %"runtime/malgo/Builtin.mlg.$x_3613_0", i64 %"runtime/malgo/Builtin.mlg.$y_3614_0")
  ret i32 %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3264"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$mulInt64_curry_2517"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3264", ptr %let_func_0, align 8
  %13 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_mul_int64_t_curry_1846"(ptr null, i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$mulInt64#_curry_2501"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2502_0", i64 %"runtime/malgo/Builtin.mlg.$y_2503_0") {
  %2 = call i64 @malgo_mul_int64_t(i64 %"runtime/malgo/Builtin.mlg.$x_2502_0", i64 %"runtime/malgo/Builtin.mlg.$y_2503_0")
  ret i64 %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3265"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subInt64_curry_2276"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3265", ptr %let_func_0, align 8
  %13 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_sub_int64_t_curry_1840"(ptr null, i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$subInt64#_curry_2260"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2261_0", i64 %"runtime/malgo/Builtin.mlg.$y_2262_0") {
  %2 = call i64 @malgo_sub_int64_t(i64 %"runtime/malgo/Builtin.mlg.$x_2261_0", i64 %"runtime/malgo/Builtin.mlg.$y_2262_0")
  ret i64 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int64_t_curry_1944"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1945_0", i64 %"runtime/malgo/Builtin.mlg.$p_1946_0") {
  %2 = call i32 @malgo_eq_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1945_0", i64 %"runtime/malgo/Builtin.mlg.$p_1946_0")
  ret i32 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_mul_int64_t_curry_1846"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1847_0", i64 %"runtime/malgo/Builtin.mlg.$p_1848_0") {
  %2 = call i64 @malgo_mul_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1847_0", i64 %"runtime/malgo/Builtin.mlg.$p_1848_0")
  ret i64 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_sub_int64_t_curry_1840"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1841_0", i64 %"runtime/malgo/Builtin.mlg.$p_1842_0") {
  %2 = call i64 @malgo_sub_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1841_0", i64 %"runtime/malgo/Builtin.mlg.$p_1842_0")
  ret i64 %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1794_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1794_0", ptr %4, align 4
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

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_2156_0") {
  %2 = call ptr @malgo_int64_t_to_string(i64 %"runtime/malgo/Builtin.mlg.$p_2156_0")
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3266"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2255_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2255_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3266", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3267"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2496_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2496_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3267", ptr %let_func_0, align 8
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

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3268"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3607_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3607_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3268", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3269"(ptr %0, ptr %1) {
  %"eqInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"eqInt64#_0" = load ptr, ptr %"eqInt64#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %n_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = getelementptr { ptr, ptr }, ptr %"eqInt64#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"eqInt64#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i32 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3270"(ptr %0, ptr %1) {
  %acc_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  ret ptr %acc_0
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3272"(ptr %0, ptr %1) {
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"subInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"subInt64#_0" = load ptr, ptr %"subInt64#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %n_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = getelementptr { ptr, ptr }, ptr %"subInt64#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"subInt64#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i64 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3273"(ptr %0, ptr %1) {
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"mulInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"mulInt64#_0" = load ptr, ptr %"mulInt64#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %n_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %n_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = getelementptr { ptr, ptr }, ptr %"mulInt64#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"mulInt64#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %12)
  %23 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i64 %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3271"(ptr %0, ptr %1) {
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"subInt64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"subInt64#_0" = load ptr, ptr %"subInt64#_addr_0", align 8
  %acc_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %"mulInt64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %"mulInt64#_0" = load ptr, ptr %"mulInt64#_addr_0", align 8
  %n_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i64 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i64 } }, ptr %3, i32 0, i32 1, i32 0
  store i64 1, ptr %5, align 4
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Int64#_0", ptr %"Int64#_1", align 8
  %"subInt64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"subInt64#_0", ptr %"subInt64#_1", align 8
  %n_1 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %n_0, ptr %n_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3272", ptr %let_func_0, align 8
  %7 = call ptr @"runtime/malgo/Builtin.mlg.$subInt64_curry_2276"(ptr null, ptr %n_0, ptr %3)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %"Int64#_0", ptr %"Int64#_2", align 8
  %"mulInt64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 1
  store ptr %"mulInt64#_0", ptr %"mulInt64#_1", align 8
  %n_2 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 2
  store ptr %n_0, ptr %n_2, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3273", ptr %let_func_1, align 8
  %9 = call ptr @"runtime/malgo/Builtin.mlg.$mulInt64_curry_2517"(ptr null, ptr %n_0, ptr %acc_0)
  %10 = call ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr null, ptr %7, ptr %9)
  ret ptr %10
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3274"(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt64_curry_2276"(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3275"(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$mulInt64_curry_2517"(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr null, ptr %"eqInt64#_capture_0", align 8
  %"eqInt64#_func_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqInt64#", ptr %"eqInt64#_func_0", align 8
  %"eqInt64#_0" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %"eqInt64#_0", align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %"isTrue#_0" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %7, ptr %"isTrue#_0", align 8
  %n_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3269", ptr %let_func_0, align 8
  %8 = call ptr @"runtime/malgo/Builtin.mlg.$eqInt64_curry_3628"(ptr null, ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %2)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %acc_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0", ptr %acc_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3270", ptr %fun_func_0, align 8
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %"Int64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %11, ptr %"Int64#_0", align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"subInt64#_capture_0", align 8
  %"subInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt64#", ptr %"subInt64#_func_0", align 8
  %"subInt64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %12, ptr %"subInt64#_0", align 8
  %acc_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 2
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0", ptr %acc_1, align 8
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"mulInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"mulInt64#_capture_0", align 8
  %"mulInt64#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulInt64#", ptr %"mulInt64#_func_0", align 8
  %"mulInt64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 3
  store ptr %13, ptr %"mulInt64#_0", align 8
  %n_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 4
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %n_1, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3271", ptr %fun_func_1, align 8
  %14 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  %15 = load i8, ptr %14, align 1
  switch i8 %15, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 1, i32 0
  store i64 1, ptr %20, align 4
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_2 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %n_2, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3274", ptr %let_func_1, align 8
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %18)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_3 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr %n_3, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3275", ptr %let_func_2, align 8
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0")
  %33 = call ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr null, ptr %26, ptr %32)
  ret ptr %33

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %1
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, {} }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  ret ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3277"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqInt64#_curry_3612"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3276"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %x_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3277", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i32 %17(ptr %15, i64 %12)
  switch i32 %18, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  %19 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %19

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  %20 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %20

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.=="(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$x_98_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$x_98_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3276", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3279"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$subInt64#_curry_2260"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3278"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %x_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3279", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i64 %17(ptr %15, i64 %12)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { i64 } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i64 } }, ptr %19, i32 0, i32 1, i32 0
  store i64 %18, ptr %21, align 4
  ret ptr %19

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.-"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$x_110_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$x_110_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3278", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3281"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$mulInt64#_curry_2501"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3280"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %x_0, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %10 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { i64 }, ptr %10, i32 0, i32 0
  %12 = load i64, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3281", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call i64 %17(ptr %15, i64 %12)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { i64 } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i64 } }, ptr %19, i32 0, i32 1, i32 0
  store i64 %18, ptr %21, align 4
  ret ptr %19

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.*"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$x_122_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$x_122_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3280", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3282"(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.factAcc"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$n_134_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_134_0", ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3282", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.fact"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$n_200_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr null, ptr %"test/testcases/malgo/Factorial.mlg.$n_200_0", ptr %2)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3283"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Factorial.mlg.$$__206_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 5, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 1, i32 0
  store i64 1, ptr %7, align 4
  %8 = call ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr null, ptr %2, ptr %5)
  %9 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %11 = getelementptr { i8, { i64 } }, ptr %8, i32 0, i32 1
  %12 = getelementptr { i64 }, ptr %11, i32 0, i32 0
  %13 = load i64, ptr %12, align 4
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int64_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %malgo_int64_t_to_string_capture_0, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0, align 8
  %15 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr %18(ptr %16, i64 %13)
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, { ptr } }, ptr %20, i32 0, i32 0
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { ptr } }, ptr %20, i32 0, i32 1, i32 0
  store ptr %19, ptr %22, align 8
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %20, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3283", ptr %let_func_0, align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %29 = call ptr %27(ptr %25, ptr %28)
  ret ptr %29

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Factorial.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Factorial.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Factorial.mlg"() {
  ret void
}
