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

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3899(ptr %0, ptr %_M1t34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal818_0, ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0, ptr %_M4_x5F34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal819_0) {
  %2 = getelementptr { i8, {} }, ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %_M4_x5F34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal819_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %_M4_x5F34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal819_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10

switch_branch_True_0:                             ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %_M1t34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal818_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %_M1t34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal818_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3911(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal206_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 5, ptr %4, align 4
  %5 = call ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %2)
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { i64 } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %6, i32 0, i32 1, i32 0
  store i64 1, ptr %8, align 4
  %9 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %6)
  %14 = getelementptr { i8, <8 x i8> }, ptr %13, i32 0, i32 0
  %15 = load i8, ptr %14, align 1
  switch i8 %15, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %16 = getelementptr { i8, { i64 } }, ptr %13, i32 0, i32 1
  %17 = getelementptr { i64 }, ptr %16, i32 0, i32 0
  %18 = load i64, ptr %17, align 4
  %19 = call ptr @malgo_int64_t_to_string(i64 %18)
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
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3911, ptr %let_func_0, align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %29 = call ptr %27(ptr %25, ptr %28)
  ret ptr %29

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3912(ptr %0, ptr %1) {
  %true_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3900(ptr null, ptr %true_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M2if34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %true_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0, ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3912, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3913(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3903(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2A46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3913, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3914(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3902(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2D46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3914, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3915(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3901(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3D_x3D46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3915, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3916(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3897(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3619_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3619_0, ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3916, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3917(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3910(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3917, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3918(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3896(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2508_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2508_0, ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3918, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3919(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3895(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2267_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2267_0, ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3919, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i32 0, i32 1
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

define internal ptr @_M4fact46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal200_0) {
  %2 = call ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal200_0)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i64 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i64 } }, ptr %3, i32 0, i32 1, i32 0
  store i64 1, ptr %5, align 4
  %6 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %3)
  ret ptr %10
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3920(ptr %0, ptr %1) {
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3899(ptr null, ptr %t_0, ptr %true_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3900(ptr %0, ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0, ptr %_M1t34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal818_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1t34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal818_0, ptr %t_0, align 8
  %true_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %_M4true34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal817_0, ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3920, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3908(ptr %0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0, ptr %_M8_x24_x5F46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3833_0) {
  ret ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3921(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3895(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2267_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2268_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2267_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2267_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2268_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2268_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3921, ptr %let_func_0, align 8
  %13 = call i64 @malgo_sub_int64_t(i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3922(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3896(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2508_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2509_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2508_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2508_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2509_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2509_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3922, ptr %let_func_0, align 8
  %13 = call i64 @malgo_mul_int64_t(i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3923(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3897(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3619_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3620_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3619_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3619_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3620_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3620_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3923, ptr %let_func_0, align 8
  %13 = call i32 @malgo_eq_int64_t(i64 %6, i64 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_Int64#_1"
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %"switch_branch_Int64#_1"
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3901(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal99_0) {
  %2 = call ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal99_0)
  ret ptr %7
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3902(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal111_0) {
  %2 = call ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal111_0)
  ret ptr %7
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3903(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal123_0) {
  %2 = call ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal123_0)
  ret ptr %7
}

define internal ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3924(ptr %0, ptr %1) {
  %acc_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3908(ptr null, ptr %acc_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3925(ptr %0, ptr %1) {
  %acc_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3909(ptr null, ptr %acc_0, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3910(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = call ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = call ptr @_M2if34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %acc_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0, ptr %acc_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3924, ptr %fun_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %acc_1 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0, ptr %acc_1, align 8
  %n_0 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0, ptr %n_0, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3925, ptr %fun_func_1, align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %18)
  ret ptr %23
}

define internal ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3909(ptr %0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0, ptr %_M8_x24_x5F46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3837_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = call ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %10)
  %12 = call ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal134_0)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal135_0)
  %18 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Factorial.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Factorial.mlg"() {
  ret void
}
