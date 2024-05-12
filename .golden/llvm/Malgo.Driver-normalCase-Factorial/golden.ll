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

define internal ptr @_M24raw_x5Flet_x24344_x24f3b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M17_x5F_x244e_x2433334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %_M17_x5F_x244e_x2433334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %_M17_x5F_x244e_x2433334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10

switch_branch_True_0:                             ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M35let_x242c2_x24f27_x5Fclosure_x24f4746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$882$f34_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$882$f34_0" = load ptr, ptr %"d$882$f34_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$882$f34_0")
  ret ptr %7
}

define internal ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2461_x24ce46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
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
  %"let$2c2$f27_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$882$f34_0" = getelementptr { ptr }, ptr %"let$2c2$f27_capture_0", i32 0, i32 0
  store ptr %20, ptr %"d$882$f34_0", align 8
  %"let$2c2$f27_capture_1" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %"let$2c2$f27_capture_0", ptr %"let$2c2$f27_capture_1", align 8
  %"let$2c2$f27_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @_M35let_x242c2_x24f27_x5Fclosure_x24f4746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$2c2$f27_func_0", align 8
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

define internal ptr @_M28let_x24345_x5Fclosure_x24f4846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"true$331_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"true$331_0" = load ptr, ptr %"true$331_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24345_x24f3c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"true$331_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M2if34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$345_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"true$331_0" = getelementptr { ptr }, ptr %"let$345_capture_0", i32 0, i32 0
  store ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"true$331_0", align 8
  %"let$345_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$345_capture_0", ptr %"let$345_capture_1", align 8
  %"let$345_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24345_x5Fclosure_x24f4846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$345_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x2485_x5Fclosure_x24f4946test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$4$7a_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$4$7a_0" = load ptr, ptr %"x$4$7a_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2485_x24f3f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"x$4$7a_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2A46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M12x_x244_x247a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$85_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$4$7a_0" = getelementptr { ptr }, ptr %"let$85_capture_0", i32 0, i32 0
  store ptr %_M12x_x244_x247a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"x$4$7a_0", align 8
  %"let$85_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$85_capture_0", ptr %"let$85_capture_1", align 8
  %"let$85_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2485_x5Fclosure_x24f4946test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$85_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x2479_x5Fclosure_x24f4a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$2$6e_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$2$6e_0" = load ptr, ptr %"x$2$6e_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2479_x24f3e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"x$2$6e_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2D46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M12x_x242_x246e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$79_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$2$6e_0" = getelementptr { ptr }, ptr %"let$79_capture_0", i32 0, i32 0
  store ptr %_M12x_x242_x246e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"x$2$6e_0", align 8
  %"let$79_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$79_capture_0", ptr %"let$79_capture_1", align 8
  %"let$79_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2479_x5Fclosure_x24f4a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$79_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x246d_x5Fclosure_x24f4b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$0$62_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$0$62_0" = load ptr, ptr %"x$0$62_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x246d_x24f3d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"x$0$62_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3D_x3D46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M12x_x240_x246246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$6d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$0$62_0" = getelementptr { ptr }, ptr %"let$6d_capture_0", i32 0, i32 0
  store ptr %_M12x_x240_x246246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"x$0$62_0", align 8
  %"let$6d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$6d_capture_0", ptr %"let$6d_capture_1", align 8
  %"let$6d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x246d_x5Fclosure_x24f4b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$6d_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x24e36_x5Fclosure_x24f4c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int64#$e23_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#$e23_0" = load ptr, ptr %"int64#$e23_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24e36_x24f3946test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"int64#$e23_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int64_x23_x24e2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$e36_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#$e23_0" = getelementptr { ptr }, ptr %"let$e36_capture_0", i32 0, i32 0
  store ptr %_M16int64_x23_x24e2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int64#$e23_0", align 8
  %"let$e36_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$e36_capture_0", ptr %"let$e36_capture_1", align 8
  %"let$e36_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24e36_x5Fclosure_x24f4c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$e36_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x24c7_x5Fclosure_x24f4d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"n$7$86_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"n$7$86_0" = load ptr, ptr %"n$7$86_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24c7_x24f4646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"n$7$86_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$c7_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"n$7$86_0" = getelementptr { ptr }, ptr %"let$c7_capture_0", i32 0, i32 0
  store ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"n$7$86_0", align 8
  %"let$c7_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$c7_capture_0", ptr %"let$c7_capture_1", align 8
  %"let$c7_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24c7_x5Fclosure_x24f4d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$c7_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x249df_x5Fclosure_x24f4e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int64#$9cc_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#$9cc_0" = load ptr, ptr %"int64#$9cc_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x249df_x24f3846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"int64#$9cc_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int64_x23_x249cc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$9df_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#$9cc_0" = getelementptr { ptr }, ptr %"let$9df_capture_0", i32 0, i32 0
  store ptr %_M16int64_x23_x249cc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int64#$9cc_0", align 8
  %"let$9df_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$9df_capture_0", ptr %"let$9df_capture_1", align 8
  %"let$9df_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x249df_x5Fclosure_x24f4e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$9df_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x248ee_x5Fclosure_x24f4f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int64#$8db_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#$8db_0" = load ptr, ptr %"int64#$8db_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x248ee_x24f3746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"int64#$8db_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int64_x23_x248db34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8ee_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#$8db_0" = getelementptr { ptr }, ptr %"let$8ee_capture_0", i32 0, i32 0
  store ptr %_M16int64_x23_x248db34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int64#$8db_0", align 8
  %"let$8ee_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$8ee_capture_0", ptr %"let$8ee_capture_1", align 8
  %"let$8ee_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x248ee_x5Fclosure_x24f4f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$8ee_func_0", align 8
  ret ptr %2
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M16str_x245b_x242cc34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16str_x245b_x242cc34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M16str_x245b_x242cc34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
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

define internal ptr @_M4fact46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr %0, ptr %_M12n_x246_x24c846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %_M12n_x246_x24c846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
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

define internal ptr @_M28let_x24344_x5Fclosure_x24f5046test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"t$4d$332_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"t$4d$332_0" = load ptr, ptr %"t$4d$332_addr_0", align 8
  %"true$331_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"true$331_0" = load ptr, ptr %"true$331_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24344_x24f3b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"t$4d$332_0", ptr %"true$331_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M24raw_x5Flet_x24345_x24f3c46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$344_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"t$4d$332_0" = getelementptr { ptr, ptr }, ptr %"let$344_capture_0", i32 0, i32 0
  store ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"t$4d$332_0", align 8
  %"true$331_0" = getelementptr { ptr, ptr }, ptr %"let$344_capture_0", i32 0, i32 1
  store ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"true$331_0", align 8
  %"let$344_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$344_capture_0", ptr %"let$344_capture_1", align 8
  %"let$344_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24344_x5Fclosure_x24f5046test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$344_func_0", align 8
  ret ptr %2
}

define internal ptr @_M36raw_x5Ffun_x2491_x24b1_x24ef8_x24f4446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M33_x24_x5F_x2439_x2490_x24b2_x24ef946test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  ret ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0
}

define internal ptr @_M23raw_x5Flet_x246d_x24f3d46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M12x_x240_x246246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M12y_x241_x246346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x240_x246246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x241_x246346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal ptr @_M23raw_x5Flet_x2479_x24f3e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M12x_x242_x246e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M12y_x243_x246f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x242_x246e46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x243_x246f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal ptr @_M23raw_x5Flet_x2485_x24f3f46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M12x_x244_x247a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M12y_x245_x247b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x244_x247a46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x245_x247b46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal i64 @_M35let_x24733_x24179_x5Fclosure_x24f5146test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$8dd$8e7$15c_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$8dd$8e7$15c_0" = load i64, ptr %"p$8dd$8e7$15c_addr_0", align 4
  %3 = call i64 @malgo_sub_int64_t(i64 %"p$8dd$8e7$15c_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M24raw_x5Flet_x248ee_x24f3746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M16int64_x23_x248db34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int64_x23_x248dc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x248db34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x248db34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x248dc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x248dc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$733$179_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$8dd$8e7$15c_0" = getelementptr { i64 }, ptr %"let$733$179_capture_0", i32 0, i32 0
  store i64 %6, ptr %"p$8dd$8e7$15c_0", align 4
  %"let$733$179_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$733$179_capture_0", ptr %"let$733$179_capture_1", align 8
  %"let$733$179_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24733_x24179_x5Fclosure_x24f5146test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$733$179_func_0", align 8
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

define internal i64 @_M35let_x24739_x242b6_x5Fclosure_x24f5246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$9ce$9d8$299_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$9ce$9d8$299_0" = load i64, ptr %"p$9ce$9d8$299_addr_0", align 4
  %3 = call i64 @malgo_mul_int64_t(i64 %"p$9ce$9d8$299_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M24raw_x5Flet_x249df_x24f3846test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M16int64_x23_x249cc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int64_x23_x249cd34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x249cc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x249cc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x249cd34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x249cd34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$739$2b6_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$9ce$9d8$299_0" = getelementptr { i64 }, ptr %"let$739$2b6_capture_0", i32 0, i32 0
  store i64 %6, ptr %"p$9ce$9d8$299_0", align 4
  %"let$739$2b6_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$739$2b6_capture_0", ptr %"let$739$2b6_capture_1", align 8
  %"let$739$2b6_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24739_x242b6_x5Fclosure_x24f5246test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$739$2b6_func_0", align 8
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

define internal ptr @_M40fun_x2491_x24b1_x24ef8_x5Fclosure_x24f5346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"acc$8$87_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"acc$8$87_0" = load ptr, ptr %"acc$8$87_addr_0", align 8
  %3 = call ptr @_M36raw_x5Ffun_x2491_x24b1_x24ef8_x24f4446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"acc$8$87_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M40fun_x24a3_x24b6_x24efc_x5Fclosure_x24f5446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, ptr %1) {
  %"acc$8$87_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"acc$8$87_0" = load ptr, ptr %"acc$8$87_addr_0", align 8
  %"n$7$86_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"n$7$86_0" = load ptr, ptr %"n$7$86_addr_0", align 8
  %3 = call ptr @_M36raw_x5Ffun_x24a3_x24b6_x24efc_x24f4546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr null, ptr %"acc$8$87_0", ptr %"n$7$86_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M23raw_x5Flet_x24c7_x24f4646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = call ptr @_M7eqInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = call ptr @_M2if34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$91$b1$ef8_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"acc$8$87_0" = getelementptr { ptr }, ptr %"fun$91$b1$ef8_capture_0", i32 0, i32 0
  store ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"acc$8$87_0", align 8
  %"fun$91$b1$ef8_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"fun$91$b1$ef8_capture_0", ptr %"fun$91$b1$ef8_capture_1", align 8
  %"fun$91$b1$ef8_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M40fun_x2491_x24b1_x24ef8_x5Fclosure_x24f5346test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"fun$91$b1$ef8_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$a3$b6$efc_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"acc$8$87_1" = getelementptr { ptr, ptr }, ptr %"fun$a3$b6$efc_capture_0", i32 0, i32 0
  store ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"acc$8$87_1", align 8
  %"n$7$86_0" = getelementptr { ptr, ptr }, ptr %"fun$a3$b6$efc_capture_0", i32 0, i32 1
  store ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %"n$7$86_0", align 8
  %"fun$a3$b6$efc_capture_1" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %"fun$a3$b6$efc_capture_0", ptr %"fun$a3$b6$efc_capture_1", align 8
  %"fun$a3$b6$efc_func_0" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M40fun_x24a3_x24b6_x24efc_x5Fclosure_x24f5446test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"fun$a3$b6$efc_func_0", align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %18)
  ret ptr %23
}

define internal i32 @_M35let_x2479b_x24a48_x5Fclosure_x24f5546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$e25$e2f$a2a_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$e25$e2f$a2a_0" = load i64, ptr %"p$e25$e2f$a2a_addr_0", align 4
  %3 = call i32 @malgo_eq_int64_t(i64 %"p$e25$e2f$a2a_0", i64 %1)
  ret i32 %3
}

define internal ptr @_M24raw_x5Flet_x24e36_x24f3946test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M16int64_x23_x24e2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int64_x23_x24e2434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x24e2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x24e2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M16int64_x23_x24e2434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %_M16int64_x23_x24e2434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$79b$a48_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$e25$e2f$a2a_0" = getelementptr { i64 }, ptr %"let$79b$a48_capture_0", i32 0, i32 0
  store i64 %6, ptr %"p$e25$e2f$a2a_0", align 4
  %"let$79b$a48_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$79b$a48_capture_0", ptr %"let$79b$a48_capture_1", align 8
  %"let$79b$a48_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x2479b_x24a48_x5Fclosure_x24f5546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Internal, ptr %"let$79b$a48_func_0", align 8
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

define internal ptr @_M36raw_x5Ffun_x24a3_x24b6_x24efc_x24f4546test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal(ptr %0, ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0, ptr %_M33_x24_x5F_x2446_x2495_x24b7_x24efd46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @_M8subInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = call ptr @_M7factAcc46test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8External(ptr null, ptr %10)
  %12 = call ptr @_M8mulInt6434runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x247_x248646test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %_M14acc_x248_x248746test_x2Ftestcases_x2Fmalgo_x2FFactorial_x2Emlg8Temporal_0)
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
