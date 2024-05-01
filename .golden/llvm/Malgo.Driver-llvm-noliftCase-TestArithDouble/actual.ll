; ModuleID = 'test/testcases/malgo/TestArithDouble.mlg'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

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

define internal double @"runtime/malgo/Builtin.mlg.$addDouble#_curry_4096"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_4097_0", double %"runtime/malgo/Builtin.mlg.$y_4098_0") {
  %2 = call double @malgo_add_double(double %"runtime/malgo/Builtin.mlg.$x_4097_0", double %"runtime/malgo/Builtin.mlg.$y_4098_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$mulDouble#_curry_2597"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2598_0", double %"runtime/malgo/Builtin.mlg.$y_2599_0") {
  %2 = call double @malgo_mul_double(double %"runtime/malgo/Builtin.mlg.$x_2598_0", double %"runtime/malgo/Builtin.mlg.$y_2599_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_mul_double_curry_1894"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1895_0", double %"runtime/malgo/Builtin.mlg.$p_1896_0") {
  %2 = call double @malgo_mul_double(double %"runtime/malgo/Builtin.mlg.$p_1895_0", double %"runtime/malgo/Builtin.mlg.$p_1896_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_sub_double_curry_1888"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1889_0", double %"runtime/malgo/Builtin.mlg.$p_1890_0") {
  %2 = call double @malgo_sub_double(double %"runtime/malgo/Builtin.mlg.$p_1889_0", double %"runtime/malgo/Builtin.mlg.$p_1890_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_add_double_curry_1882"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1883_0", double %"runtime/malgo/Builtin.mlg.$p_1884_0") {
  %2 = call double @malgo_add_double(double %"runtime/malgo/Builtin.mlg.$p_1883_0", double %"runtime/malgo/Builtin.mlg.$p_1884_0")
  ret double %2
}

declare ptr @malgo_malloc(i64)

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3128"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1880_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1880_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3128", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3129"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_mul_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1892_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1892_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3129", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.show"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_91_0") {
  %2 = call ptr @malgo_double_to_string(double %"test/testcases/malgo/TestArithDouble.mlg.$x_91_0")
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  ret ptr %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.traceShowId"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_95_0") {
  %2 = call ptr @malgo_double_to_string(double %"test/testcases/malgo/TestArithDouble.mlg.$x_95_0")
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  %6 = getelementptr { i8, <8 x i8> }, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %8 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1
  %9 = getelementptr { ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @malgo_print_string(ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, {} }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_newline(ptr %14)
  ret double %"test/testcases/malgo/TestArithDouble.mlg.$x_95_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3130"(ptr %0, double %1) {
  %3 = call double @malgo_sub_double(double 0.000000e+00, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.neg"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_100_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3130", ptr %let_func_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_sub_double_curry_1888"(ptr null, double 0.000000e+00, double %"test/testcases/malgo/TestArithDouble.mlg.$x_100_0")
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3132"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_mul_double_curry_1894"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3131"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_1 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %x_0, ptr %x_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3132", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call double %7(ptr %5, double %1)
  ret double %8
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.mul"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"test/testcases/malgo/TestArithDouble.mlg.$x_104_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3131", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3134"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_add_double_curry_1882"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3133"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_1 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %x_0, ptr %x_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3134", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call double %7(ptr %5, double %1)
  ret double %8
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.add"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_116_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"test/testcases/malgo/TestArithDouble.mlg.$x_116_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3133", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3136"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_add_double_curry_1882"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3135"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_1 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %x_0, ptr %x_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3136", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call double %7(ptr %5, double %1)
  ret double %8
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.+"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_128_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"test/testcases/malgo/TestArithDouble.mlg.$x_128_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3135", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3138"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_mul_double_curry_1894"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3137"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_1 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %x_0, ptr %x_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3138", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call double %7(ptr %5, double %1)
  ret double %8
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.*"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"test/testcases/malgo/TestArithDouble.mlg.$x_140_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3137", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3139"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double, ptr }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %malgo_add_double_addr_0 = getelementptr { double, ptr }, ptr %0, i32 0, i32 1
  %malgo_add_double_0 = load ptr, ptr %malgo_add_double_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_add_double_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_add_double_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %x_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %1)
  ret double %12
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3140"(ptr %0, double %1) {
  %malgo_mul_double_addr_0 = getelementptr { ptr, double }, ptr %0, i32 0, i32 0
  %malgo_mul_double_0 = load ptr, ptr %malgo_mul_double_addr_0, align 8
  %d_addr_0 = getelementptr { ptr, double }, ptr %0, i32 0, i32 1
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_mul_double_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_mul_double_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %d_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %1)
  ret double %12
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.f"(ptr %0, double %"test/testcases/malgo/TestArithDouble.mlg.$x_152_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double, ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double, ptr }, ptr %let_capture_0, i32 0, i32 0
  store double %"test/testcases/malgo/TestArithDouble.mlg.$x_152_0", ptr %x_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_double_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_add_double_capture_0, align 8
  %malgo_add_double_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_double", ptr %malgo_add_double_func_0, align 8
  %malgo_add_double_0 = getelementptr { double, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %3, ptr %malgo_add_double_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3139", ptr %let_func_0, align 8
  %4 = call double @"runtime/malgo/Builtin.mlg.$addDouble#_curry_4096"(ptr null, double %"test/testcases/malgo/TestArithDouble.mlg.$x_152_0", double 0.000000e+00)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, double }, ptr null, i32 1) to i64))
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_mul_double_capture_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr null, ptr %malgo_mul_double_capture_0, align 8
  %malgo_mul_double_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_mul_double", ptr %malgo_mul_double_func_0, align 8
  %malgo_mul_double_0 = getelementptr { ptr, double }, ptr %let_capture_2, i32 0, i32 0
  store ptr %6, ptr %malgo_mul_double_0, align 8
  %d_0 = getelementptr { ptr, double }, ptr %let_capture_2, i32 0, i32 1
  store double %4, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3140", ptr %let_func_1, align 8
  %7 = call double @"runtime/malgo/Builtin.mlg.$mulDouble#_curry_2597"(ptr null, double %4, double %"test/testcases/malgo/TestArithDouble.mlg.$x_152_0")
  ret double %7
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3141"(ptr %0, double %1) {
  %3 = call double @"runtime/malgo/Builtin.mlg.$addDouble#_curry_4096"(ptr null, double 5.000000e-01, double %1)
  ret double %3
}

define internal double @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3142"(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$mulDouble#_curry_2597"(ptr null, double %d_0, double %1)
  ret double %3
}

define internal ptr @"test/testcases/malgo/TestArithDouble.mlg.main"(ptr %0, ptr %"test/testcases/malgo/TestArithDouble.mlg.$$__160_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3141", ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double 0.000000e+00)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { double }, ptr %let_capture_2, i32 0, i32 0
  store double %7, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestArithDouble.mlg.#let_closure_3142", ptr %let_func_1, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call double %12(ptr %10, double 5.000000e-01)
  %14 = call ptr @malgo_double_to_string(double %13)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 1, i32 0
  store ptr %14, ptr %17, align 8
  %18 = getelementptr { i8, <8 x i8> }, ptr %15, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %20 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 1
  %21 = getelementptr { ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr @malgo_print_string(ptr %22)
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %25 = getelementptr { i8, {} }, ptr %24, i32 0, i32 0
  store i8 0, ptr %25, align 1
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, {} }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = call ptr @malgo_newline(ptr %26)
  ret ptr %28

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestArithDouble.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TestArithDouble.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestArithDouble.mlg"() {
  ret void
}
