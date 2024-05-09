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

define internal ptr @_M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M10isTrue_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M7unboxed34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2777_0) {
  switch i32 %_M7unboxed34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2777_0, label %switch-unboxed_default_0 [
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

define internal i32 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3297(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @_M11eqInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3607_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3607_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3297, ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3298(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M12mulInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2496_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2496_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3298, ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3299(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M12subInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2255_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2255_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3299, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M35malgo_x5Fint64_x5Ft_x5Fto_x5Fstring34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2156_0) {
  %2 = call ptr @malgo_int64_t_to_string(i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2156_0)
  ret ptr %2
}

define internal ptr @_M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1794_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1794_0, ptr %4, align 4
  ret ptr %2
}

define internal i32 @_M20eqInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3612(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3613_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3614_0) {
  %2 = call i32 @malgo_eq_int64_t(i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3613_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3614_0)
  ret i32 %2
}

define internal i32 @_M34malgo_x5Feq_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1944(ptr %0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1945_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1946_0) {
  %2 = call i32 @malgo_eq_int64_t(i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1945_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1946_0)
  ret i32 %2
}

define internal i64 @_M35malgo_x5Fmul_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1846(ptr %0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1847_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1848_0) {
  %2 = call i64 @malgo_mul_int64_t(i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1847_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1848_0)
  ret i64 %2
}

define internal i64 @_M35malgo_x5Fsub_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1840(ptr %0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1841_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1842_0) {
  %2 = call i64 @malgo_sub_int64_t(i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1841_0, i64 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1842_0)
  ret i64 %2
}

define internal i64 @_M21mulInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2501(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2502_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2503_0) {
  %2 = call i64 @malgo_mul_int64_t(i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2502_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2503_0)
  ret i64 %2
}

define internal i64 @_M21subInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2260(ptr %0, i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2261_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2262_0) {
  %2 = call i64 @malgo_sub_int64_t(i64 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2261_0, i64 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2262_0)
  ret i64 %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3300(ptr %0, ptr %1) {
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
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 1, i32 0
  store i64 1, ptr %7, align 4
  %8 = call ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr null, ptr %2, ptr %5)
  %9 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %11 = getelementptr { i8, { i64 } }, ptr %8, i32 0, i32 1
  %12 = getelementptr { i64 }, ptr %11, i32 0, i32 0
  %13 = load i64, ptr %12, align 4
  %14 = call ptr @_M35malgo_x5Fint64_x5Ft_x5Fto_x5Fstring34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %13)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 1, i32 0
  store ptr %14, ptr %17, align 8
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %15, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3300, ptr %let_func_0, align 8
  %19 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %24 = call ptr %22(ptr %20, ptr %23)
  ret ptr %24

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3301(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3296(ptr null, ptr %x_0, ptr %1)
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
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3301, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3302(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3295(ptr null, ptr %x_0, ptr %1)
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
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3302, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3303(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3294(ptr null, ptr %x_0, ptr %1)
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
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3303, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3304(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr null, ptr %n_0, ptr %1)
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
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3304, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
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
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal200_0, ptr %2)
  ret ptr %5
}

define internal ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3286(ptr %0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0, ptr %_M8_x24_x5F46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal178_0) {
  ret ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0
}

define internal i32 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3305(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @_M16eqInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3628(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3629_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3630_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3629_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3629_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3630_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3630_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3305, ptr %let_func_0, align 8
  %13 = call i32 @_M34malgo_x5Feq_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1944(ptr null, i64 %6, i64 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3306(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3278(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3307(ptr %0, ptr %1) {
  %acc_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3286(ptr null, ptr %acc_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3308(ptr %0, ptr %1) {
  %acc_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3293(ptr null, ptr %acc_0, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3309(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M17subInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2276(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3310(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M17mulInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2517(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3306, ptr %let_func_0, align 8
  %6 = call ptr @_M16eqInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3628(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %2)
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %acc_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0, ptr %acc_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3307, ptr %fun_func_0, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %acc_1 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0, ptr %acc_1, align 8
  %n_1 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_1, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3308, ptr %fun_func_1, align 8
  %9 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %switch_branch__M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
    i8 1, label %switch_branch__M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, { i64 } }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { i64 } }, ptr %13, i32 0, i32 1, i32 0
  store i64 1, ptr %15, align 4
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_2 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_2, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3309, ptr %let_func_1, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %13)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_3 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_3, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3310, ptr %let_func_2, align 8
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0)
  %28 = call ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr null, ptr %21, ptr %27)
  ret ptr %28

switch_branch__M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  ret ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3311(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M17mulInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2517(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2518_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2519_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2518_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2518_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2519_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2519_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3311, ptr %let_func_0, align 8
  %13 = call i64 @_M35malgo_x5Fmul_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1846(ptr null, i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3278(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3036_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3036_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3036_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @_M11eqInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @_M10isTrue_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3291(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3038_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3038_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3038_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @_M12subInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @_M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3292(ptr %0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3040_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3040_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3040_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @_M12mulInt64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @_M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3312(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @_M20eqInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3612(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3294(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal99_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal98_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal99_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg10Temporal99_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3312, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  switch i32 %17, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %18 = call ptr @_M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null)
  ret ptr %18

switch-unboxed_default_0:                         ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %19 = call ptr @_M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null)
  ret ptr %19

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3313(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @_M21subInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2260(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3295(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal111_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal110_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal111_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal111_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3313, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 1, i32 0
  store i64 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3314(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @_M21mulInt64_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2501(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3296(ptr %0, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal123_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M1x46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal122_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal123_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M1y46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal123_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3314, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i64 } }, ptr %18, i32 0, i32 1, i32 0
  store i64 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3315(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @_M17subInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2276(ptr %0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2277_0, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2278_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2277_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2277_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2278_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i64 } }, ptr %_M9int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2278_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3315, ptr %let_func_0, align 8
  %13 = call i64 @_M35malgo_x5Fsub_x5Fint64_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1840(ptr null, i64 %6, i64 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i64 } }, ptr %14, i32 0, i32 1, i32 0
  store i64 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %switch_branch__M9Int64_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3316(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3291(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3317(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3292(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Ffun46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Temporal3293(ptr %0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M8_x24_x5F46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal183_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3316, ptr %let_func_0, align 8
  %6 = call ptr @_M17subInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2276(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %2)
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %n_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @_M14let_x5Fclosure46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg12Internal3317, ptr %let_func_1, align 8
  %8 = call ptr @_M17mulInt64_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2517(ptr null, ptr %_M1n46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal167_0, ptr %_M3acc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal168_0)
  %9 = call ptr @_M16factAcc_x5Fcurry46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg11Temporal166(ptr null, ptr %6, ptr %8)
  ret ptr %9
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
