; ModuleID = 'test/testcases/malgo/RightAssoc.mlg'
source_filename = "test/testcases/malgo/RightAssoc.mlg"

@str4101 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2455_x24ba47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  %5 = call ptr @_M1f47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8External(ptr null, ptr %2)
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1, i32 0
  store ptr @str4101, ptr %8, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %11 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr @malgo_print_string(ptr %13)
  ret ptr %14

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M23raw_x5Ffun_x245b_x24f8547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2411_x245947test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x2465_x24fc547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2414_x246347test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x246f_x24fe547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2417_x246d47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x2479_x24ff547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x241a_x247747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x248c_x24ffd47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x242f_x248a47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24raw_x5Ffun_x2496_x24100147test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2432_x249447test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24raw_x5Ffun_x24a0_x24100347test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2435_x249e47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M24raw_x5Ffun_x24aa_x24100447test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M20_x24_x5F_x2438_x24a847test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M36let_x242ba_x24ea6_x5Fclosure_x24100647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %"eta$56_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"eta$56_0" = load ptr, ptr %"eta$56_addr_0", align 8
  %3 = call ptr @_M31raw_x5Flet_x242ba_x24ea6_x24f0547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %"eta$56_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M12_x3C_x7C_x3E47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8External(ptr %0, ptr %_M9eta_x245647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2ba$ea6_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"eta$56_0" = getelementptr { ptr }, ptr %"let$2ba$ea6_capture_0", i32 0, i32 0
  store ptr %_M9eta_x245647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0, ptr %"eta$56_0", align 8
  %"let$2ba$ea6_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$2ba$ea6_capture_0", ptr %"let$2ba$ea6_capture_1", align 8
  %"let$2ba$ea6_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M36let_x242ba_x24ea6_x5Fclosure_x24100647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"let$2ba$ea6_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28fun_x245b_x5Fclosure_x24100747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x245b_x24f8547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x2465_x5Fclosure_x24100847test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2465_x24fc547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x246f_x5Fclosure_x24100947test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x246f_x24fe547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x2479_x5Fclosure_x24100a47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2479_x24ff547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x248c_x5Fclosure_x24100b47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x248c_x24ffd47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x2496_x5Fclosure_x24100c47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M24raw_x5Ffun_x2496_x24100147test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x24a0_x5Fclosure_x24100d47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M24raw_x5Ffun_x24a0_x24100347test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M28fun_x24aa_x5Fclosure_x24100e47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M24raw_x5Ffun_x24aa_x24100447test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M1f47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8External(ptr %0, ptr %_M12n_x241_x245747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$5b_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$5b_capture_1" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %"fun$5b_capture_0", ptr %"fun$5b_capture_1", align 8
  %"fun$5b_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @_M28fun_x245b_x5Fclosure_x24100747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$5b_func_0", align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 1, i32 1
  store ptr %4, ptr %8, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, {} }, ptr %9, i32 0, i32 0
  store i8 1, ptr %10, align 1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$65_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$65_capture_1" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %"fun$65_capture_0", ptr %"fun$65_capture_1", align 8
  %"fun$65_func_0" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @_M28fun_x2465_x5Fclosure_x24100847test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$65_func_0", align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %9, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 1
  store ptr %11, ptr %15, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 1, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$6f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$6f_capture_1" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %"fun$6f_capture_0", ptr %"fun$6f_capture_1", align 8
  %"fun$6f_func_0" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M28fun_x246f_x5Fclosure_x24100947test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$6f_func_0", align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr %16, ptr %21, align 8
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1, i32 1
  store ptr %18, ptr %22, align 8
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %24 = getelementptr { i8, {} }, ptr %23, i32 0, i32 0
  store i8 1, ptr %24, align 1
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$79_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$79_capture_1" = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %"fun$79_capture_0", ptr %"fun$79_capture_1", align 8
  %"fun$79_func_0" = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @_M28fun_x2479_x5Fclosure_x24100a47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$79_func_0", align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %23, ptr %28, align 8
  %29 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 1, i32 1
  store ptr %25, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 0
  store i8 1, ptr %33, align 1
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %26, ptr %34, align 8
  %35 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 1, i32 1
  store ptr %30, ptr %35, align 8
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 0
  store i8 1, ptr %37, align 1
  %38 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 1, i32 0
  store ptr %19, ptr %38, align 8
  %39 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 1, i32 1
  store ptr %32, ptr %39, align 8
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %41 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 0
  store i8 1, ptr %41, align 1
  %42 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 1, i32 0
  store ptr %12, ptr %42, align 8
  %43 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 1, i32 1
  store ptr %36, ptr %43, align 8
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 0
  store i8 1, ptr %45, align 1
  %46 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr %5, ptr %46, align 8
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 1
  store ptr %40, ptr %47, align 8
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %49 = getelementptr { i8, {} }, ptr %48, i32 0, i32 0
  store i8 1, ptr %49, align 1
  %50 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$8c_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$8c_capture_1" = getelementptr { ptr, ptr }, ptr %50, i32 0, i32 0
  store ptr %"fun$8c_capture_0", ptr %"fun$8c_capture_1", align 8
  %"fun$8c_func_0" = getelementptr { ptr, ptr }, ptr %50, i32 0, i32 1
  store ptr @_M28fun_x248c_x5Fclosure_x24100b47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$8c_func_0", align 8
  %51 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %52 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 0
  store i8 0, ptr %52, align 1
  %53 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 1, i32 0
  store ptr %48, ptr %53, align 8
  %54 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 1, i32 1
  store ptr %50, ptr %54, align 8
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %56 = getelementptr { i8, {} }, ptr %55, i32 0, i32 0
  store i8 1, ptr %56, align 1
  %57 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$96_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$96_capture_1" = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 0
  store ptr %"fun$96_capture_0", ptr %"fun$96_capture_1", align 8
  %"fun$96_func_0" = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 1
  store ptr @_M28fun_x2496_x5Fclosure_x24100c47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$96_func_0", align 8
  %58 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %59 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 0
  store i8 0, ptr %59, align 1
  %60 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 1, i32 0
  store ptr %55, ptr %60, align 8
  %61 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 1, i32 1
  store ptr %57, ptr %61, align 8
  %62 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %63 = getelementptr { i8, {} }, ptr %62, i32 0, i32 0
  store i8 1, ptr %63, align 1
  %64 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$a0_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$a0_capture_1" = getelementptr { ptr, ptr }, ptr %64, i32 0, i32 0
  store ptr %"fun$a0_capture_0", ptr %"fun$a0_capture_1", align 8
  %"fun$a0_func_0" = getelementptr { ptr, ptr }, ptr %64, i32 0, i32 1
  store ptr @_M28fun_x24a0_x5Fclosure_x24100d47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$a0_func_0", align 8
  %65 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %66 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 0
  store i8 0, ptr %66, align 1
  %67 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 1, i32 0
  store ptr %62, ptr %67, align 8
  %68 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 1, i32 1
  store ptr %64, ptr %68, align 8
  %69 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %70 = getelementptr { i8, {} }, ptr %69, i32 0, i32 0
  store i8 1, ptr %70, align 1
  %71 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$aa_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$aa_capture_1" = getelementptr { ptr, ptr }, ptr %71, i32 0, i32 0
  store ptr %"fun$aa_capture_0", ptr %"fun$aa_capture_1", align 8
  %"fun$aa_func_0" = getelementptr { ptr, ptr }, ptr %71, i32 0, i32 1
  store ptr @_M28fun_x24aa_x5Fclosure_x24100e47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Internal, ptr %"fun$aa_func_0", align 8
  %72 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %73 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 0
  store i8 0, ptr %73, align 1
  %74 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 1, i32 0
  store ptr %69, ptr %74, align 8
  %75 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 1, i32 1
  store ptr %71, ptr %75, align 8
  %76 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %77 = getelementptr { i8, {} }, ptr %76, i32 0, i32 0
  store i8 0, ptr %77, align 1
  %78 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %79 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 0
  store i8 1, ptr %79, align 1
  %80 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 1, i32 0
  store ptr %72, ptr %80, align 8
  %81 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 1, i32 1
  store ptr %76, ptr %81, align 8
  %82 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %83 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 0
  store i8 1, ptr %83, align 1
  %84 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 1, i32 0
  store ptr %65, ptr %84, align 8
  %85 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 1, i32 1
  store ptr %78, ptr %85, align 8
  %86 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %87 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 0
  store i8 1, ptr %87, align 1
  %88 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 1, i32 0
  store ptr %58, ptr %88, align 8
  %89 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 1, i32 1
  store ptr %82, ptr %89, align 8
  %90 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %91 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 0
  store i8 1, ptr %91, align 1
  %92 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 1, i32 0
  store ptr %51, ptr %92, align 8
  %93 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 1, i32 1
  store ptr %86, ptr %93, align 8
  ret ptr %90
}

define internal ptr @_M31raw_x5Flet_x242ba_x24ea6_x24f0547test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal(ptr %0, ptr %_M9eta_x245647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0, ptr %_M15p_x242b4_x24ea747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M9eta_x245647test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %_M15p_x242b4_x24ea747test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8Temporal_0, ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/RightAssoc.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FRightAssoc_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/RightAssoc.mlg"() {
  ret void
}
