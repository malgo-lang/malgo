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

declare ptr @malgo_malloc(i64)

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4104(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4102(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @_M4_x2A52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal140_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal140_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4104, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4105(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4101(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @_M4_x2B52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal128_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal128_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4105, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4106(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4100(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @_M3add52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal116_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal116_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4106, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4107(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @_M22malgo_x5Fadd_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, double %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1880_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1880_0, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4107, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4108(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @_M22malgo_x5Fmul_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, double %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1892_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1892_0, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4108, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4109(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4099(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @_M3mul52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal104_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal104_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4109, ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4110(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @malgo_add_double(double %x_0, double %1)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4111(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @malgo_mul_double(double %d_0, double %1)
  ret double %3
}

define internal double @_M1f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal152_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal152_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4110, ptr %let_func_0, align 8
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
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4111, ptr %let_func_1, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call double %12(ptr %10, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal152_0)
  ret double %13
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4112(ptr %0, double %1) {
  %3 = call double @malgo_add_double(double 5.000000e-01, double %1)
  ret double %3
}

define internal double @_M3let52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4065(ptr %0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4066_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4112, ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4066_0)
  ret double %7
}

define internal double @_M3neg52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal100_0) {
  %2 = call double @malgo_sub_double(double 0.000000e+00, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal100_0)
  ret double %2
}

define internal double @_M11traceShowId52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg10Temporal95_0) {
  %2 = call ptr @malgo_double_to_string(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg10Temporal95_0)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  %6 = getelementptr { i8, <8 x i8> }, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
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
  ret double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg10Temporal95_0

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4show52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg10Temporal91_0) {
  %2 = call ptr @malgo_double_to_string(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg10Temporal91_0)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  ret ptr %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4113(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @malgo_mul_double(double %x_0, double %1)
  ret double %3
}

define internal double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4099(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal104_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal105_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal104_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4113, ptr %let_func_0, align 8
  %3 = call double @malgo_mul_double(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal104_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal105_0)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4114(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @malgo_add_double(double %x_0, double %1)
  ret double %3
}

define internal double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4100(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal116_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal117_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal116_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4114, ptr %let_func_0, align 8
  %3 = call double @malgo_add_double(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal116_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal117_0)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4115(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @malgo_add_double(double %x_0, double %1)
  ret double %3
}

define internal double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4101(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal128_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal129_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal128_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4115, ptr %let_func_0, align 8
  %3 = call double @malgo_add_double(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal128_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal129_0)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4116(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @malgo_mul_double(double %x_0, double %1)
  ret double %3
}

define internal double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4102(ptr %0, double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal140_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal141_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal140_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4116, ptr %let_func_0, align 8
  %3 = call double @malgo_mul_double(double %_M1x52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal140_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal141_0)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4117(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @malgo_mul_double(double %d_0, double %1)
  ret double %3
}

define internal double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4103(ptr %0, double %_M1d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal3998_0, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4074_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %_M1d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal3998_0, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4117, ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %_M1y52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4074_0)
  ret double %7
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4118(ptr %0, double %1) {
  %d_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %d_0 = load double, ptr %d_addr_0, align 8
  %3 = call double @_M10raw_x5Flet52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Temporal4103(ptr null, double %d_0, double %1)
  ret double %3
}

define internal ptr @_M4main52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg11Temporal160_0) {
  %2 = call ptr @_M22malgo_x5Fadd_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, double 5.000000e-01)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double 0.000000e+00)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %7, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4118, ptr %let_func_0, align 8
  %9 = call ptr @_M22malgo_x5Fmul_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, double %7)
  %10 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call double %13(ptr %11, double 5.000000e-01)
  %15 = call ptr @malgo_double_to_string(double %14)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, { ptr } }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %16, i32 0, i32 1, i32 0
  store ptr %15, ptr %18, align 8
  %19 = getelementptr { i8, <8 x i8> }, ptr %16, i32 0, i32 0
  %20 = load i8, ptr %19, align 1
  switch i8 %20, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %21 = getelementptr { i8, { ptr } }, ptr %16, i32 0, i32 1
  %22 = getelementptr { ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr @malgo_print_string(ptr %23)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, {} }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, {} }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = call ptr @malgo_newline(ptr %27)
  ret ptr %29

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestArithDouble.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestArithDouble.mlg"() {
  ret void
}
