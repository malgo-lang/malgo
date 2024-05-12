; ModuleID = 'test/testcases/malgo/With.mlg'
source_filename = "test/testcases/malgo/With.mlg"

@str3827 = unnamed_addr constant [4 x i8] c"end\00"
@str3828 = unnamed_addr constant [4 x i8] c"foo\00"

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

define internal ptr @_M5twice41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8External(ptr %0, ptr %_M12k_x246_x244f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { ptr, ptr }, ptr %_M12k_x246_x244f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %_M12k_x246_x244f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, {} }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { ptr, ptr }, ptr %_M12k_x246_x244f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %_M12k_x246_x244f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %9)
  ret ptr %15
}

define internal ptr @_M27let_x2472_x5Fclosure_x24eef41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %"finalizer$2$63_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"finalizer$2$63_0" = load ptr, ptr %"finalizer$2$63_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2472_x24eaf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %"finalizer$2$63_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7finally41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8External(ptr %0, ptr %_M20finalizer_x242_x246341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"finalizer$2$63_0" = getelementptr { ptr }, ptr %"let$72_capture_0", i32 0, i32 0
  store ptr %_M20finalizer_x242_x246341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %"finalizer$2$63_0", align 8
  %"let$72_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$72_capture_0", ptr %"let$72_capture_1", align 8
  %"let$72_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2472_x5Fclosure_x24eef41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$72_func_0", align 8
  ret ptr %2
}

define internal ptr @_M30raw_x5Flet_x2472_x24e87_x24edf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M10cast_x247a41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %_M19k_x243_x2464_x24e8841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { ptr, ptr }, ptr %_M19k_x243_x2464_x24e8841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %_M19k_x243_x2464_x24e8841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = getelementptr { ptr, ptr }, ptr %_M10cast_x247a41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %_M10cast_x247a41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %8)
  ret ptr %13
}

define internal ptr @_M23raw_x5Flet_x2472_x24eaf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M20finalizer_x242_x246341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %_M12k_x243_x246441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { ptr, ptr }, ptr %_M12k_x243_x246441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %_M12k_x243_x246441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = getelementptr { ptr, ptr }, ptr %_M20finalizer_x242_x246341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %_M20finalizer_x242_x246341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %8)
  ret ptr %13
}

define internal ptr @_M27fun_x2478_x5Fclosure_x24ef041test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2478_x24ecf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M34let_x2472_x24e87_x5Fclosure_x24ef141test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$7a_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$7a_0" = load ptr, ptr %"cast$7a_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x2472_x24e87_x24edf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %"cast$7a_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M27fun_x248c_x5Fclosure_x24ef241test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x248c_x24eee41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x244e_x247341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$78_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$78_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"fun$78_capture_0", ptr %"fun$78_capture_1", align 8
  %"fun$78_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27fun_x2478_x5Fclosure_x24ef041test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$78_func_0", align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72$e87_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$7a_0" = getelementptr { ptr }, ptr %"let$72$e87_capture_0", i32 0, i32 0
  store ptr %2, ptr %"cast$7a_0", align 8
  %"let$72$e87_capture_1" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %"let$72$e87_capture_0", ptr %"let$72$e87_capture_1", align 8
  %"let$72$e87_func_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @_M34let_x2472_x24e87_x5Fclosure_x24ef141test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$72$e87_func_0", align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$8c_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$8c_capture_1" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %"fun$8c_capture_0", ptr %"fun$8c_capture_1", align 8
  %"fun$8c_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @_M27fun_x248c_x5Fclosure_x24ef241test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$8c_func_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %4)
  ret ptr %9
}

define internal ptr @_M23raw_x5Ffun_x2478_x24ecf41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M11tuple_x247441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11tuple_x247441test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1, i32 0
  store ptr @str3827, ptr %6, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %4, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Tuple#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  ret ptr %12

switch_default_0:                                 ; preds = %"switch_branch_Tuple#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M34let_x2462_x24e91_x5Fclosure_x24ef541test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$7f_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$7f_0" = load ptr, ptr %"d$7f_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x2462_x24e91_x24eeb41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %"d$7f_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M27fun_x2485_x5Fclosure_x24ef641test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2485_x24eec41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M23raw_x5Ffun_x2488_x24eed41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2448_x247e41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str3828, ptr %4, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$62$e91_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$7f_0" = getelementptr { ptr }, ptr %"let$62$e91_capture_0", i32 0, i32 0
  store ptr %2, ptr %"d$7f_0", align 8
  %"let$62$e91_capture_1" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %"let$62$e91_capture_0", ptr %"let$62$e91_capture_1", align 8
  %"let$62$e91_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M34let_x2462_x24e91_x5Fclosure_x24ef541test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$62$e91_func_0", align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$85_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$85_capture_1" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %"fun$85_capture_0", ptr %"fun$85_capture_1", align 8
  %"fun$85_func_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @_M27fun_x2485_x5Fclosure_x24ef641test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$85_func_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %6)
  ret ptr %11
}

define internal ptr @_M27fun_x2488_x5Fclosure_x24ef741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2488_x24eed41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M23raw_x5Ffun_x248c_x24eee41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x244b_x247d41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$88_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$88_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"fun$88_capture_0", ptr %"fun$88_capture_1", align 8
  %"fun$88_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27fun_x2488_x5Fclosure_x24ef741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"fun$88_func_0", align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, {} }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %3)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %10)
  ret ptr %16
}

define internal ptr @_M27let_x2462_x5Fclosure_x24ef841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal(ptr %0, ptr %1) {
  %"str$7$57_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"str$7$57_0" = load ptr, ptr %"str$7$57_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2462_x24eae41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr null, ptr %"str$7$57_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M14printAndReturn41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8External(ptr %0, ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$62_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"str$7$57_0" = getelementptr { ptr }, ptr %"let$62_capture_0", i32 0, i32 0
  store ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %"str$7$57_0", align 8
  %"let$62_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$62_capture_0", ptr %"let$62_capture_1", align 8
  %"let$62_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2462_x5Fclosure_x24ef841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Internal, ptr %"let$62_func_0", align 8
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x2485_x24eec41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M12x_x249_x248341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M12x_x249_x248341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M12x_x249_x248341test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M30raw_x5Flet_x2462_x24e91_x24eeb41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M7d_x247f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %_M19k_x248_x2458_x24e9241test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M7d_x247f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M7d_x247f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = getelementptr { ptr, ptr }, ptr %_M19k_x248_x2458_x24e9241test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %_M19k_x248_x2458_x24e9241test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M7d_x247f41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M23raw_x5Flet_x2462_x24eae41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal(ptr %0, ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, ptr %_M12k_x248_x245841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = getelementptr { ptr, ptr }, ptr %_M12k_x248_x245841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %_M12k_x248_x245841test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M14str_x247_x245741test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8Temporal_0)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/With.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FWith_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/With.mlg"() {
  ret void
}
