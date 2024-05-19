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

define internal double @_M28let_x2497_x5Fclosure_x24100852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$1$8c_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$1$8c_0" = load double, ptr %"x$1$8c_addr_0", align 8
  %3 = call double @_M24raw_x5Flet_x2497_x24100652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr null, double %"x$1$8c_0", double %1)
  ret double %3
}

define internal ptr @_M4_x2A52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x241_x248c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$97_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$1$8c_0" = getelementptr { double }, ptr %"let$97_capture_0", i32 0, i32 0
  store double %_M12x_x241_x248c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$1$8c_0", align 8
  %"let$97_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$97_capture_0", ptr %"let$97_capture_1", align 8
  %"let$97_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x2497_x5Fclosure_x24100852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$97_func_0", align 8
  ret ptr %2
}

define internal double @_M28let_x248b_x5Fclosure_x24100952test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$5$80_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$5$80_0" = load double, ptr %"x$5$80_addr_0", align 8
  %3 = call double @_M24raw_x5Flet_x248b_x24100552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr null, double %"x$5$80_0", double %1)
  ret double %3
}

define internal ptr @_M4_x2B52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x245_x248052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8b_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$5$80_0" = getelementptr { double }, ptr %"let$8b_capture_0", i32 0, i32 0
  store double %_M12x_x245_x248052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$5$80_0", align 8
  %"let$8b_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$8b_capture_0", ptr %"let$8b_capture_1", align 8
  %"let$8b_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x248b_x5Fclosure_x24100952test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$8b_func_0", align 8
  ret ptr %2
}

define internal double @_M28let_x247f_x5Fclosure_x24100a52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$7$74_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$7$74_0" = load double, ptr %"x$7$74_addr_0", align 8
  %3 = call double @_M24raw_x5Flet_x247f_x24100452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr null, double %"x$7$74_0", double %1)
  ret double %3
}

define internal ptr @_M3add52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x247_x247452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$7f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$7$74_0" = getelementptr { double }, ptr %"let$7f_capture_0", i32 0, i32 0
  store double %_M12x_x247_x247452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$7$74_0", align 8
  %"let$7f_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$7f_capture_0", ptr %"let$7f_capture_1", align 8
  %"let$7f_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x247f_x5Fclosure_x24100a52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$7f_func_0", align 8
  ret ptr %2
}

define internal double @_M29let_x2475d_x5Fclosure_x24100b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"p$758_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"p$758_0" = load double, ptr %"p$758_addr_0", align 8
  %3 = call double @malgo_add_double(double %"p$758_0", double %1)
  ret double %3
}

define internal ptr @_M22malgo_x5Fadd_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, double %_M8p_x2475834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$75d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"p$758_0" = getelementptr { double }, ptr %"let$75d_capture_0", i32 0, i32 0
  store double %_M8p_x2475834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"p$758_0", align 8
  %"let$75d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$75d_capture_0", ptr %"let$75d_capture_1", align 8
  %"let$75d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M29let_x2475d_x5Fclosure_x24100b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d_func_0", align 8
  ret ptr %2
}

define internal double @_M29let_x24769_x5Fclosure_x24100c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"p$764_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"p$764_0" = load double, ptr %"p$764_addr_0", align 8
  %3 = call double @malgo_mul_double(double %"p$764_0", double %1)
  ret double %3
}

define internal ptr @_M22malgo_x5Fmul_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, double %_M8p_x2476434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$769_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"p$764_0" = getelementptr { double }, ptr %"let$769_capture_0", i32 0, i32 0
  store double %_M8p_x2476434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"p$764_0", align 8
  %"let$769_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$769_capture_0", ptr %"let$769_capture_1", align 8
  %"let$769_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M29let_x24769_x5Fclosure_x24100c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769_func_0", align 8
  ret ptr %2
}

define internal double @_M28let_x2473_x5Fclosure_x24100d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$3$68_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$3$68_0" = load double, ptr %"x$3$68_addr_0", align 8
  %3 = call double @_M24raw_x5Flet_x2473_x24100352test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr null, double %"x$3$68_0", double %1)
  ret double %3
}

define internal ptr @_M3mul52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x243_x246852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$73_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$3$68_0" = getelementptr { double }, ptr %"let$73_capture_0", i32 0, i32 0
  store double %_M12x_x243_x246852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$3$68_0", align 8
  %"let$73_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$73_capture_0", ptr %"let$73_capture_1", align 8
  %"let$73_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x2473_x5Fclosure_x24100d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$73_func_0", align 8
  ret ptr %2
}

define internal double @_M36let_x2475d_x24f92_x5Fclosure_x24100e52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$b$98_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$b$98_0" = load double, ptr %"x$b$98_addr_0", align 8
  %3 = call double @malgo_add_double(double %"x$b$98_0", double %1)
  ret double %3
}

define internal double @_M36let_x24769_x24f97_x5Fclosure_x24100f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"d$9b_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"d$9b_0" = load double, ptr %"d$9b_addr_0", align 8
  %3 = call double @malgo_mul_double(double %"d$9b_0", double %1)
  ret double %3
}

define internal double @_M1f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x24b_x249852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$75d$f92_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$b$98_0" = getelementptr { double }, ptr %"let$75d$f92_capture_0", i32 0, i32 0
  store double %_M12x_x24b_x249852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$b$98_0", align 8
  %"let$75d$f92_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$75d$f92_capture_0", ptr %"let$75d$f92_capture_1", align 8
  %"let$75d$f92_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x2475d_x24f92_x5Fclosure_x24100e52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d$f92_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double 0.000000e+00)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$769$f97_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"d$9b_0" = getelementptr { double }, ptr %"let$769$f97_capture_0", i32 0, i32 0
  store double %7, ptr %"d$9b_0", align 8
  %"let$769$f97_capture_1" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %"let$769$f97_capture_0", ptr %"let$769$f97_capture_1", align 8
  %"let$769$f97_func_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @_M36let_x24769_x24f97_x5Fclosure_x24100f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769$f97_func_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call double %12(ptr %10, double %_M12x_x24b_x249852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %13
}

define internal double @_M36let_x2475d_x24ff7_x5Fclosure_x24101052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %3 = call double @malgo_add_double(double 5.000000e-01, double %1)
  ret double %3
}

define internal double @_M18let_x2410f5_x24fe152test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M22y_x243d_x2410eb_x24fe252test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$75d$ff7_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"let$75d$ff7_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$75d$ff7_capture_0", ptr %"let$75d$ff7_capture_1", align 8
  %"let$75d$ff7_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x2475d_x24ff7_x5Fclosure_x24101052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d$ff7_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %_M22y_x243d_x2410eb_x24fe252test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %7
}

define internal double @_M3neg52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x240_x246452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call double @malgo_sub_double(double 0.000000e+00, double %_M12x_x240_x246452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %2
}

define internal double @_M11traceShowId52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x24a_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_double_to_string(double %_M12x_x24a_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
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
  ret double %_M12x_x24a_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4show52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, double %_M12x_x249_x245b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_double_to_string(double %_M12x_x249_x245b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  ret ptr %3
}

define internal double @_M36let_x24769_x24ed1_x5Fclosure_x24101152test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$3$68_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$3$68_0" = load double, ptr %"x$3$68_addr_0", align 8
  %3 = call double @malgo_mul_double(double %"x$3$68_0", double %1)
  ret double %3
}

define internal double @_M24raw_x5Flet_x2473_x24100352test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M12x_x243_x246852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x244_x246952test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$769$ed1_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$3$68_0" = getelementptr { double }, ptr %"let$769$ed1_capture_0", i32 0, i32 0
  store double %_M12x_x243_x246852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$3$68_0", align 8
  %"let$769$ed1_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$769$ed1_capture_0", ptr %"let$769$ed1_capture_1", align 8
  %"let$769$ed1_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x24769_x24ed1_x5Fclosure_x24101152test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769$ed1_func_0", align 8
  %3 = call double @malgo_mul_double(double %_M12x_x243_x246852test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x244_x246952test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %3
}

define internal double @_M36let_x2475d_x24ef5_x5Fclosure_x24101252test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$7$74_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$7$74_0" = load double, ptr %"x$7$74_addr_0", align 8
  %3 = call double @malgo_add_double(double %"x$7$74_0", double %1)
  ret double %3
}

define internal double @_M24raw_x5Flet_x247f_x24100452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M12x_x247_x247452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x248_x247552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$75d$ef5_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$7$74_0" = getelementptr { double }, ptr %"let$75d$ef5_capture_0", i32 0, i32 0
  store double %_M12x_x247_x247452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$7$74_0", align 8
  %"let$75d$ef5_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$75d$ef5_capture_0", ptr %"let$75d$ef5_capture_1", align 8
  %"let$75d$ef5_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x2475d_x24ef5_x5Fclosure_x24101252test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d$ef5_func_0", align 8
  %3 = call double @malgo_add_double(double %_M12x_x247_x247452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x248_x247552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %3
}

define internal double @_M36let_x2475d_x24f19_x5Fclosure_x24101352test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$5$80_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$5$80_0" = load double, ptr %"x$5$80_addr_0", align 8
  %3 = call double @malgo_add_double(double %"x$5$80_0", double %1)
  ret double %3
}

define internal double @_M24raw_x5Flet_x248b_x24100552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M12x_x245_x248052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x246_x248152test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$75d$f19_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$5$80_0" = getelementptr { double }, ptr %"let$75d$f19_capture_0", i32 0, i32 0
  store double %_M12x_x245_x248052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$5$80_0", align 8
  %"let$75d$f19_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$75d$f19_capture_0", ptr %"let$75d$f19_capture_1", align 8
  %"let$75d$f19_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x2475d_x24f19_x5Fclosure_x24101352test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d$f19_func_0", align 8
  %3 = call double @malgo_add_double(double %_M12x_x245_x248052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x246_x248152test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %3
}

define internal double @_M36let_x24769_x24f3d_x5Fclosure_x24101452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"x$1$8c_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"x$1$8c_0" = load double, ptr %"x$1$8c_addr_0", align 8
  %3 = call double @malgo_mul_double(double %"x$1$8c_0", double %1)
  ret double %3
}

define internal double @_M24raw_x5Flet_x2497_x24100652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M12x_x241_x248c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x242_x248d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$769$f3d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"x$1$8c_0" = getelementptr { double }, ptr %"let$769$f3d_capture_0", i32 0, i32 0
  store double %_M12x_x241_x248c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"x$1$8c_0", align 8
  %"let$769$f3d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$769$f3d_capture_0", ptr %"let$769$f3d_capture_1", align 8
  %"let$769$f3d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x24769_x24f3d_x5Fclosure_x24101452test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769$f3d_func_0", align 8
  %3 = call double @malgo_mul_double(double %_M12x_x241_x248c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M12y_x242_x248d52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %3
}

define internal double @_M36let_x24769_x24ffd_x5Fclosure_x24101552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"d$9b$f9e_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"d$9b$f9e_0" = load double, ptr %"d$9b$f9e_addr_0", align 8
  %3 = call double @malgo_mul_double(double %"d$9b$f9e_0", double %1)
  ret double %3
}

define internal double @_M32raw_x5Flet_x24a59_x24fe9_x24100752test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr %0, double %_M14d_x249b_x24f9e52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, double %_M21y_x2445_x24a4f_x24fea52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$769$ffd_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"d$9b$f9e_0" = getelementptr { double }, ptr %"let$769$ffd_capture_0", i32 0, i32 0
  store double %_M14d_x249b_x24f9e52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0, ptr %"d$9b$f9e_0", align 8
  %"let$769$ffd_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$769$ffd_capture_0", ptr %"let$769$ffd_capture_1", align 8
  %"let$769$ffd_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x24769_x24ffd_x5Fclosure_x24101552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769$ffd_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %_M21y_x2445_x24a4f_x24fea52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0)
  ret double %7
}

define internal double @_M36let_x24a59_x24fe9_x5Fclosure_x24101652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr %0, double %1) {
  %"d$9b$f9e_addr_0" = getelementptr { double }, ptr %0, i32 0, i32 0
  %"d$9b$f9e_0" = load double, ptr %"d$9b$f9e_addr_0", align 8
  %3 = call double @_M32raw_x5Flet_x24a59_x24fe9_x24100752test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal(ptr null, double %"d$9b$f9e_0", double %1)
  ret double %3
}

define internal ptr @_M4main52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x245a_x24a052test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Temporal_0) {
  %2 = call ptr @_M22malgo_x5Fadd_x5Fdouble34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, double 5.000000e-01)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double 0.000000e+00)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$a59$fe9_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %"d$9b$f9e_0" = getelementptr { double }, ptr %"let$a59$fe9_capture_0", i32 0, i32 0
  store double %7, ptr %"d$9b$f9e_0", align 8
  %"let$a59$fe9_capture_1" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %"let$a59$fe9_capture_0", ptr %"let$a59$fe9_capture_1", align 8
  %"let$a59$fe9_func_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @_M36let_x24a59_x24fe9_x5Fclosure_x24101652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$a59$fe9_func_0", align 8
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
