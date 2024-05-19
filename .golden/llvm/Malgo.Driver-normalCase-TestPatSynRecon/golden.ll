; ModuleID = 'test/testcases/malgo/TestPatSynRecon.mlg'
source_filename = "test/testcases/malgo/TestPatSynRecon.mlg"

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

define internal i64 @_M27let_x245e_x5Fclosure_x24f0252test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr %0, i64 %1) {
  %"x$0$53_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"x$0$53_0" = load i64, ptr %"x$0$53_addr_0", align 4
  %3 = call i64 @_M23raw_x5Flet_x245e_x24f0052test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal(ptr null, i64 %"x$0$53_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M8_x2B_x2352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr %0, i64 %_M12x_x240_x245352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$5e_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"x$0$53_0" = getelementptr { i64 }, ptr %"let$5e_capture_0", i32 0, i32 0
  store i64 %_M12x_x240_x245352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, ptr %"x$0$53_0", align 4
  %"let$5e_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$5e_capture_0", ptr %"let$5e_capture_1", align 8
  %"let$5e_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x245e_x5Fclosure_x24f0252test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$5e_func_0", align 8
  ret ptr %2
}

define internal i64 @_M35let_x2472d_x24e8f_x5Fclosure_x24f0352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr %0, i64 %1) {
  %"x$0$53_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"x$0$53_0" = load i64, ptr %"x$0$53_addr_0", align 4
  %3 = call i64 @malgo_add_int64_t(i64 %"x$0$53_0", i64 %1)
  ret i64 %3
}

define internal i64 @_M23raw_x5Flet_x245e_x24f0052test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal(ptr %0, i64 %_M12x_x240_x245352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i64 %_M12y_x241_x245452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72d$e8f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"x$0$53_0" = getelementptr { i64 }, ptr %"let$72d$e8f_capture_0", i32 0, i32 0
  store i64 %_M12x_x240_x245352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, ptr %"x$0$53_0", align 4
  %"let$72d$e8f_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$72d$e8f_capture_0", ptr %"let$72d$e8f_capture_1", align 8
  %"let$72d$e8f_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M35let_x2472d_x24e8f_x5Fclosure_x24f0352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$72d$e8f_func_0", align 8
  %3 = call i64 @malgo_add_int64_t(i64 %_M12x_x240_x245352test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i64 %_M12y_x241_x245452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0)
  ret i64 %3
}

define internal ptr @_M35let_x242c2_x24ee6_x5Fclosure_x24f0452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$83_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$83_0" = load ptr, ptr %"d$83_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$83_0")
  ret ptr %7
}

define internal ptr @_M27fun_x2489_x5Fclosure_x24f0552test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2489_x24f0152test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2452_x247552test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i64 } }, ptr %5, i32 0, i32 1, i32 0
  store i64 2, ptr %7, align 4
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 0
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1, i32 0
  store ptr %5, ptr %12, align 8
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1, i32 1
  store ptr %8, ptr %13, align 8
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %2, ptr %16, align 8
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %14, i32 0, i32 1, i32 1
  store ptr %10, ptr %17, align 8
  %18 = call ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr null, ptr %14)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$ee6_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$83_0" = getelementptr { ptr }, ptr %"let$2c2$ee6_capture_0", i32 0, i32 0
  store ptr %18, ptr %"d$83_0", align 8
  %"let$2c2$ee6_capture_1" = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr %"let$2c2$ee6_capture_0", ptr %"let$2c2$ee6_capture_1", align 8
  %"let$2c2$ee6_func_0" = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @_M35let_x242c2_x24ee6_x5Fclosure_x24f0452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$2c2$ee6_func_0", align 8
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$89_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$89_capture_1" = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  store ptr %"fun$89_capture_0", ptr %"fun$89_capture_1", align 8
  %"fun$89_func_0" = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  store ptr @_M27fun_x2489_x5Fclosure_x24f0552test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"fun$89_func_0", align 8
  %21 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr %24(ptr %22, ptr %20)
  ret ptr %25
}

define internal ptr @_M23raw_x5Ffun_x2489_x24f0152test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal(ptr %0, ptr %_M12i_x245_x248752test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M12i_x245_x248752test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M12i_x245_x248752test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call ptr @malgo_int64_t_to_string(i64 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int64#_0"
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @_M35let_x2472d_x24ed2_x5Fclosure_x24f0652test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$63_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$63_0" = load i64, ptr %"p$63_addr_0", align 4
  %3 = call i64 @malgo_add_int64_t(i64 %"p$63_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr %0, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { i64 } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %4, i32 0, i32 1, i32 0
  store i64 0, ptr %6, align 4
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i32 0, i32 1
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { i8, <8 x i8> }, ptr %9, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %switch_branch_Cons_0
  %14 = getelementptr { i8, { i64 } }, ptr %9, i32 0, i32 1
  %15 = getelementptr { i64 }, ptr %14, i32 0, i32 0
  %16 = load i64, ptr %15, align 4
  %17 = call ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr null, ptr %11)
  %18 = getelementptr { i8, <8 x i8> }, ptr %17, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %20 = getelementptr { i8, { i64 } }, ptr %17, i32 0, i32 1
  %21 = getelementptr { i64 }, ptr %20, i32 0, i32 0
  %22 = load i64, ptr %21, align 4
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72d$ed2_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$63_0" = getelementptr { i64 }, ptr %"let$72d$ed2_capture_0", i32 0, i32 0
  store i64 %16, ptr %"p$63_0", align 4
  %"let$72d$ed2_capture_1" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %"let$72d$ed2_capture_0", ptr %"let$72d$ed2_capture_1", align 8
  %"let$72d$ed2_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @_M35let_x2472d_x24ed2_x5Fclosure_x24f0652test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$72d$ed2_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call i64 %27(ptr %25, i64 %22)
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, { i64 } }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { i64 } }, ptr %29, i32 0, i32 1, i32 0
  store i64 %28, ptr %31, align 4
  ret ptr %29

switch_default_0:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Cons_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestPatSynRecon.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestPatSynRecon.mlg"() {
  ret void
}
