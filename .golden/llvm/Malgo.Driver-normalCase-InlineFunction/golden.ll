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

define internal ptr @_M27let_x247f_x5Fclosure_x24f9551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"f$6$70_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"f$6$70_0" = load ptr, ptr %"f$6$70_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x247f_x24f8251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"f$6$70_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$7f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"f$6$70_0" = getelementptr { ptr }, ptr %"let$7f_capture_0", i32 0, i32 0
  store ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"f$6$70_0", align 8
  %"let$7f_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$7f_capture_0", ptr %"let$7f_capture_1", align 8
  %"let$7f_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x247f_x5Fclosure_x24f9551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$7f_func_0", align 8
  ret ptr %2
}

define internal ptr @_M23raw_x5Flet_x247f_x24f8251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12x_x247_x247151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr null, ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %_M12f_x246_x247051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %2)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M12x_x247_x247151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  ret ptr %12
}

define internal ptr @_M24raw_x5Flet_x24344_x24f8051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M17_x5F_x244e_x2433334runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
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

define internal ptr @_M27let_x24f4_x5Fclosure_x24f9651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"f$8$a5_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"f$8$a5_0" = load ptr, ptr %"f$8$a5_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24f4_x24f9351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"f$8$a5_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M23raw_x5Ffun_x24f5_x24f9451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$f4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"f$8$a5_0" = getelementptr { ptr }, ptr %"let$f4_capture_0", i32 0, i32 0
  store ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"f$8$a5_0", align 8
  %"let$f4_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$f4_capture_0", ptr %"let$f4_capture_1", align 8
  %"let$f4_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24f4_x5Fclosure_x24f9651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$f4_func_0", align 8
  ret ptr %2
}

define internal ptr @_M40fun_x24b1_x24d7_x24f42_x5Fclosure_x24f9751test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M36raw_x5Ffun_x24b1_x24d7_x24f42_x24f9151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M40fun_x24c9_x24dd_x24f48_x5Fclosure_x24f9851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"f$8$a5_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"f$8$a5_0" = load ptr, ptr %"f$8$a5_addr_0", align 8
  %"n$9$a6_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"n$9$a6_0" = load ptr, ptr %"n$9$a6_addr_0", align 8
  %3 = call ptr @_M36raw_x5Ffun_x24c9_x24dd_x24f48_x24f9251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"f$8$a5_0", ptr %"n$9$a6_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M23raw_x5Flet_x24f4_x24f9351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @_M7leInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = call ptr @_M2if34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$b1$d7$f42_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$b1$d7$f42_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"fun$b1$d7$f42_capture_0", ptr %"fun$b1$d7$f42_capture_1", align 8
  %"fun$b1$d7$f42_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M40fun_x24b1_x24d7_x24f42_x5Fclosure_x24f9751test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"fun$b1$d7$f42_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$c9$dd$f48_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"f$8$a5_0" = getelementptr { ptr, ptr }, ptr %"fun$c9$dd$f48_capture_0", i32 0, i32 0
  store ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"f$8$a5_0", align 8
  %"n$9$a6_0" = getelementptr { ptr, ptr }, ptr %"fun$c9$dd$f48_capture_0", i32 0, i32 1
  store ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"n$9$a6_0", align 8
  %"fun$c9$dd$f48_capture_1" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %"fun$c9$dd$f48_capture_0", ptr %"fun$c9$dd$f48_capture_1", align 8
  %"fun$c9$dd$f48_func_0" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M40fun_x24c9_x24dd_x24f48_x5Fclosure_x24f9851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"fun$c9$dd$f48_func_0", align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %18)
  ret ptr %23
}

define internal ptr @_M36raw_x5Ffun_x24c9_x24dd_x24f48_x24f9251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M33_x24_x5F_x245a_x24b5_x24de_x24f4951test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @_M8subInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %2)
  %11 = getelementptr { ptr, ptr }, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, { i32 } }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %16, i32 0, i32 1, i32 0
  store i32 2, ptr %18, align 4
  %19 = call ptr @_M8subInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12n_x249_x24a651test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %20 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %16)
  %25 = getelementptr { ptr, ptr }, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %_M12f_x248_x24a551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr %28(ptr %26, ptr %24)
  %30 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %15)
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr %34(ptr %32, ptr %29)
  ret ptr %35
}

define internal ptr @_M27fun_x24f5_x5Fclosure_x24f9951test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x24f5_x24f9451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M35let_x242c2_x24f22_x5Fclosure_x24f9a51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$889$f79_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$889$f79_0" = load ptr, ptr %"d$889$f79_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$889$f79_0")
  ret ptr %7
}

define internal ptr @_M4main51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x246f_x24a451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$f5_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$f5_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"fun$f5_capture_0", ptr %"fun$f5_capture_1", align 8
  %"fun$f5_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27fun_x24f5_x5Fclosure_x24f9951test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"fun$f5_func_0", align 8
  %3 = call ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr null, ptr %2)
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1, i32 0
  store i32 5, ptr %6, align 4
  %7 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %4)
  %12 = getelementptr { i8, <4 x i8> }, ptr %11, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %14 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = call ptr @malgo_int32_t_to_string(i32 %16)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %17, ptr %20, align 8
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$f22_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$889$f79_0" = getelementptr { ptr }, ptr %"let$2c2$f22_capture_0", i32 0, i32 0
  store ptr %18, ptr %"d$889$f79_0", align 8
  %"let$2c2$f22_capture_1" = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  store ptr %"let$2c2$f22_capture_0", ptr %"let$2c2$f22_capture_1", align 8
  %"let$2c2$f22_func_0" = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  store ptr @_M35let_x242c2_x24f22_x5Fclosure_x24f9a51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$2c2$f22_func_0", align 8
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %27 = call ptr %25(ptr %23, ptr %26)
  ret ptr %27

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36raw_x5Ffun_x24b1_x24d7_x24f42_x24f9151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M33_x24_x5F_x2449_x24af_x24d8_x24f4351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M28let_x24345_x5Fclosure_x24f9b51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"true$331_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"true$331_0" = load ptr, ptr %"true$331_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24345_x24f8151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"true$331_0", ptr %1)
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
  store ptr @_M28let_x24345_x5Fclosure_x24f9b51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$345_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x24a3_x5Fclosure_x24f9c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$2$98_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$2$98_0" = load ptr, ptr %"x$2$98_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24a3_x24f8551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"x$2$98_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2B51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M12x_x242_x249851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$a3_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$2$98_0" = getelementptr { ptr }, ptr %"let$a3_capture_0", i32 0, i32 0
  store ptr %_M12x_x242_x249851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"x$2$98_0", align 8
  %"let$a3_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$a3_capture_0", ptr %"let$a3_capture_1", align 8
  %"let$a3_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24a3_x5Fclosure_x24f9c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$a3_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x2497_x5Fclosure_x24f9d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$4$8c_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$4$8c_0" = load ptr, ptr %"x$4$8c_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2497_x24f8451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"x$4$8c_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2D51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M12x_x244_x248c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$97_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$4$8c_0" = getelementptr { ptr }, ptr %"let$97_capture_0", i32 0, i32 0
  store ptr %_M12x_x244_x248c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"x$4$8c_0", align 8
  %"let$97_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$97_capture_0", ptr %"let$97_capture_1", align 8
  %"let$97_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2497_x5Fclosure_x24f9d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$97_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x248b_x5Fclosure_x24f9e51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$0$80_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$0$80_0" = load ptr, ptr %"x$0$80_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x248b_x24f8351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"x$0$80_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3C_x3D51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M12x_x240_x248051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8b_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$0$80_0" = getelementptr { ptr }, ptr %"let$8b_capture_0", i32 0, i32 0
  store ptr %_M12x_x240_x248051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %"x$0$80_0", align 8
  %"let$8b_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$8b_capture_0", ptr %"let$8b_capture_1", align 8
  %"let$8b_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x248b_x5Fclosure_x24f9e51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$8b_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x24fda_x5Fclosure_x24f9f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$fc7_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$fc7_0" = load ptr, ptr %"int32#$fc7_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24fda_x24f7e51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"int32#$fc7_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$fda_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#$fc7_0" = getelementptr { ptr }, ptr %"let$fda_capture_0", i32 0, i32 0
  store ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int32#$fc7_0", align 8
  %"let$fda_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$fda_capture_0", ptr %"let$fda_capture_1", align 8
  %"let$fda_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24fda_x5Fclosure_x24f9f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$fda_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x24b2f_x5Fclosure_x24fa051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$b1c_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$b1c_0" = load ptr, ptr %"int32#$b1c_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24b2f_x24f7d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"int32#$b1c_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7leInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int32_x23_x24b1c34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$b2f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#$b1c_0" = getelementptr { ptr }, ptr %"let$b2f_capture_0", i32 0, i32 0
  store ptr %_M16int32_x23_x24b1c34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int32#$b1c_0", align 8
  %"let$b2f_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$b2f_capture_0", ptr %"let$b2f_capture_1", align 8
  %"let$b2f_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24b2f_x5Fclosure_x24fa051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$b2f_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x2490e_x5Fclosure_x24fa151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$8fb_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$8fb_0" = load ptr, ptr %"int32#$8fb_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x2490e_x24f7c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"int32#$8fb_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8subInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M16int32_x23_x248fb34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$90e_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#$8fb_0" = getelementptr { ptr }, ptr %"let$90e_capture_0", i32 0, i32 0
  store ptr %_M16int32_x23_x248fb34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int32#$8fb_0", align 8
  %"let$90e_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$90e_capture_0", ptr %"let$90e_capture_1", align 8
  %"let$90e_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x2490e_x5Fclosure_x24fa151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$90e_func_0", align 8
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

define internal ptr @_M28let_x24344_x5Fclosure_x24fa251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, ptr %1) {
  %"t$4d$332_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"t$4d$332_0" = load ptr, ptr %"t$4d$332_addr_0", align 8
  %"true$331_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"true$331_0" = load ptr, ptr %"true$331_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24344_x24f8051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr null, ptr %"t$4d$332_0", ptr %"true$331_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M24raw_x5Flet_x24345_x24f8151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$344_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"t$4d$332_0" = getelementptr { ptr, ptr }, ptr %"let$344_capture_0", i32 0, i32 0
  store ptr %_M14t_x244d_x2433234runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"t$4d$332_0", align 8
  %"true$331_0" = getelementptr { ptr, ptr }, ptr %"let$344_capture_0", i32 0, i32 1
  store ptr %_M11true_x2433134runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"true$331_0", align 8
  %"let$344_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$344_capture_0", ptr %"let$344_capture_1", align 8
  %"let$344_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24344_x5Fclosure_x24fa251test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$344_func_0", align 8
  ret ptr %2
}

define internal ptr @_M23raw_x5Flet_x248b_x24f8351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12x_x240_x248051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12y_x241_x248151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @_M7leInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x240_x248051test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x241_x248151test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal i32 @_M35let_x2471b_x241dd_x5Fclosure_x24fa351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$8fd$907$1c0_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$8fd$907$1c0_0" = load i32, ptr %"p$8fd$907$1c0_addr_0", align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %"p$8fd$907$1c0_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M24raw_x5Flet_x2490e_x24f7c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M16int32_x23_x248fb34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int32_x23_x248fc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x248fb34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x248fb34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x248fc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x248fc34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$71b$1dd_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$8fd$907$1c0_0" = getelementptr { i32 }, ptr %"let$71b$1dd_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$8fd$907$1c0_0", align 4
  %"let$71b$1dd_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$71b$1dd_capture_0", ptr %"let$71b$1dd_capture_1", align 8
  %"let$71b$1dd_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x2471b_x241dd_x5Fclosure_x24fa351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$71b$1dd_func_0", align 8
  %13 = call i32 @malgo_sub_int32_t(i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M23raw_x5Flet_x2497_x24f8451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12x_x244_x248c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12y_x245_x248d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @_M8subInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x244_x248c51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x245_x248d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal ptr @_M23raw_x5Flet_x24a3_x24f8551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M12x_x242_x249851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0, ptr %_M12y_x243_x249951test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0) {
  %2 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x242_x249851test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x243_x249951test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal i32 @_M35let_x2478f_x244df_x5Fclosure_x24fa451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$b1e$b28$4c1_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$b1e$b28$4c1_0" = load i32, ptr %"p$b1e$b28$4c1_addr_0", align 4
  %3 = call i32 @malgo_le_int32_t(i32 %"p$b1e$b28$4c1_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M24raw_x5Flet_x24b2f_x24f7d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M16int32_x23_x24b1c34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int32_x23_x24b1d34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x24b1c34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x24b1c34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x24b1d34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x24b1d34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$78f$4df_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$b1e$b28$4c1_0" = getelementptr { i32 }, ptr %"let$78f$4df_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$b1e$b28$4c1_0", align 4
  %"let$78f$4df_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$78f$4df_capture_0", ptr %"let$78f$4df_capture_1", align 8
  %"let$78f$4df_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x2478f_x244df_x5Fclosure_x24fa451test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$78f$4df_func_0", align 8
  %13 = call i32 @malgo_le_int32_t(i32 %6, i32 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %"switch_branch_Int32#_1"
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %"switch_branch_Int32#_1"
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M35let_x24715_x24d5a_x5Fclosure_x24fa551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$fc9$fd3$d3d_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$fc9$fd3$d3d_0" = load i32, ptr %"p$fc9$fd3$d3d_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"p$fc9$fd3$d3d_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M24raw_x5Flet_x24fda_x24f7e51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Temporal(ptr %0, ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int32_x23_x24fc834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M16int32_x23_x24fc834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M16int32_x23_x24fc834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$715$d5a_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$fc9$fd3$d3d_0" = getelementptr { i32 }, ptr %"let$715$d5a_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$fc9$fd3$d3d_0", align 4
  %"let$715$d5a_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$715$d5a_capture_0", ptr %"let$715$d5a_capture_1", align 8
  %"let$715$d5a_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24715_x24d5a_x5Fclosure_x24fa551test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8Internal, ptr %"let$715$d5a_func_0", align 8
  %13 = call i32 @malgo_add_int32_t(i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/InlineFunction.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/InlineFunction.mlg"() {
  ret void
}
