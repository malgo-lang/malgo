; ModuleID = './test/testcases/malgo/TestArithDouble.mlg'
source_filename = "./test/testcases/malgo/TestArithDouble.mlg"

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

define internal double @"TestArithDouble.#let_closure_3764"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_mul_double(ptr %0, double %"Builtin.$p_1892_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1892_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3764", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.$raw_let_3762"(ptr %0, double %"TestArithDouble.$x_152_0", double %"TestArithDouble.$y_3705_0") {
  %2 = call ptr @Builtin.malgo_add_double(ptr null, double %"TestArithDouble.$x_152_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_3705_0")
  ret double %7
}

define internal double @"TestArithDouble.$raw_let_3763"(ptr %0, double %"TestArithDouble.$d_3700_0", double %"TestArithDouble.$y_3707_0") {
  %2 = call ptr @Builtin.malgo_mul_double(ptr null, double %"TestArithDouble.$d_3700_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_3707_0")
  ret double %7
}

define internal double @"TestArithDouble.#let_closure_3765"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$malgo_mul_double_curry_1894"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.$raw_let_3760"(ptr %0, double %"TestArithDouble.$x_140_0", double %"TestArithDouble.$y_141_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_140_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3765", ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_141_0")
  ret double %7
}

define internal double @TestArithDouble.traceShowId(ptr %0, double %"TestArithDouble.$x_95_0") {
  %2 = call ptr @malgo_double_to_string(double %"TestArithDouble.$x_95_0")
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  %6 = getelementptr { i8, <8 x i8> }, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
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
  ret double %"TestArithDouble.$x_95_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal double @"Builtin.$mulDouble#_curry_2597"(ptr %0, double %"Builtin.$x_2598_0", double %"Builtin.$y_2599_0") {
  %2 = call double @malgo_mul_double(double %"Builtin.$x_2598_0", double %"Builtin.$y_2599_0")
  ret double %2
}

define internal double @"TestArithDouble.#let_closure_3766"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3762"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.#let_closure_3767"(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3763"(ptr null, double %d_0, double %1)
  ret double %3
}

define internal double @TestArithDouble.f(ptr %0, double %"TestArithDouble.$x_152_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_152_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3766", ptr %let_func_0, align 8
  %3 = call double @"Builtin.$addDouble#_curry_4096"(ptr null, double %"TestArithDouble.$x_152_0", double 0.000000e+00)
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { double }, ptr %let_capture_2, i32 0, i32 0
  store double %3, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3767", ptr %let_func_1, align 8
  %5 = call double @"Builtin.$mulDouble#_curry_2597"(ptr null, double %3, double %"TestArithDouble.$x_152_0")
  ret double %5
}

define internal double @"TestArithDouble.#let_closure_3768"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3757"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @TestArithDouble.mul(ptr %0, double %"TestArithDouble.$x_104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_104_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3768", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_3769"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3760"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"TestArithDouble.*"(ptr %0, double %"TestArithDouble.$x_140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_140_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3769", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$malgo_mul_double_curry_1894"(ptr %0, double %"Builtin.$p_1895_0", double %"Builtin.$p_1896_0") {
  %2 = call double @malgo_mul_double(double %"Builtin.$p_1895_0", double %"Builtin.$p_1896_0")
  ret double %2
}

define internal ptr @TestArithDouble.show(ptr %0, double %"TestArithDouble.$x_91_0") {
  %2 = call ptr @malgo_double_to_string(double %"TestArithDouble.$x_91_0")
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  ret ptr %3
}

define internal double @"TestArithDouble.#let_closure_3770"(ptr %0, double %1) {
  %3 = call double @malgo_sub_double(double 0.000000e+00, double %1)
  ret double %3
}

define internal double @TestArithDouble.neg(ptr %0, double %"TestArithDouble.$x_100_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3770", ptr %let_func_0, align 8
  %3 = call double @"Builtin.$malgo_sub_double_curry_1888"(ptr null, double 0.000000e+00, double %"TestArithDouble.$x_100_0")
  ret double %3
}

define internal double @"TestArithDouble.#let_closure_3771"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3759"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"TestArithDouble.+"(ptr %0, double %"TestArithDouble.$x_128_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_128_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3771", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$malgo_add_double_curry_1882"(ptr %0, double %"Builtin.$p_1883_0", double %"Builtin.$p_1884_0") {
  %2 = call double @malgo_add_double(double %"Builtin.$p_1883_0", double %"Builtin.$p_1884_0")
  ret double %2
}

define internal double @"Builtin.$addDouble#_curry_4096"(ptr %0, double %"Builtin.$x_4097_0", double %"Builtin.$y_4098_0") {
  %2 = call double @malgo_add_double(double %"Builtin.$x_4097_0", double %"Builtin.$y_4098_0")
  ret double %2
}

define internal double @"TestArithDouble.#let_closure_3772"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"TestArithDouble.$raw_let_3758"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @TestArithDouble.add(ptr %0, double %"TestArithDouble.$x_116_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_116_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3772", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_3773"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_add_double(ptr %0, double %"Builtin.$p_1880_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1880_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3773", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_3774"(ptr %0, double %1) {
  %3 = call double @"Builtin.$addDouble#_curry_4096"(ptr null, double 5.000000e-01, double %1)
  ret double %3
}

define internal double @"TestArithDouble.#let_closure_3775"(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @"Builtin.$mulDouble#_curry_2597"(ptr null, double %d_0, double %1)
  ret double %3
}

define internal ptr @TestArithDouble.main(ptr %0, ptr %"TestArithDouble.$$__160_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3774", ptr %let_func_0, align 8
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
  store ptr @"TestArithDouble.#let_closure_3775", ptr %let_func_1, align 8
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
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
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

define internal double @"TestArithDouble.#let_closure_3776"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$malgo_add_double_curry_1882"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.$raw_let_3758"(ptr %0, double %"TestArithDouble.$x_116_0", double %"TestArithDouble.$y_117_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_116_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3776", ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_117_0")
  ret double %7
}

define internal double @"TestArithDouble.#let_closure_3777"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$malgo_add_double_curry_1882"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.$raw_let_3759"(ptr %0, double %"TestArithDouble.$x_128_0", double %"TestArithDouble.$y_129_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_128_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3777", ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_129_0")
  ret double %7
}

define internal double @"Builtin.$malgo_sub_double_curry_1888"(ptr %0, double %"Builtin.$p_1889_0", double %"Builtin.$p_1890_0") {
  %2 = call double @malgo_sub_double(double %"Builtin.$p_1889_0", double %"Builtin.$p_1890_0")
  ret double %2
}

define internal double @"TestArithDouble.#let_closure_3778"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$malgo_mul_double_curry_1894"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.$raw_let_3757"(ptr %0, double %"TestArithDouble.$x_104_0", double %"TestArithDouble.$y_105_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"TestArithDouble.$x_104_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3778", ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"TestArithDouble.$y_105_0")
  ret double %7
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_TestArithDouble()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @TestArithDouble.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_TestArithDouble() {
  ret void
}
