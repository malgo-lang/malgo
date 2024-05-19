; ModuleID = 'test/testcases/malgo/Test0.mlg'
source_filename = "test/testcases/malgo/Test0.mlg"

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

define internal i64 @_M27let_x2453_x5Fclosure_x24d5142test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr %0, i64 %1) {
  %"x$4$48_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"x$4$48_0" = load i64, ptr %"x$4$48_addr_0", align 4
  %3 = call i64 @_M23raw_x5Flet_x2453_x24d5042test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal(ptr null, i64 %"x$4$48_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M8_x2B_x2342test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8External(ptr %0, i64 %_M12x_x244_x244842test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$53_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"x$4$48_0" = getelementptr { i64 }, ptr %"let$53_capture_0", i32 0, i32 0
  store i64 %_M12x_x244_x244842test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, ptr %"x$4$48_0", align 4
  %"let$53_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$53_capture_0", ptr %"let$53_capture_1", align 8
  %"let$53_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2453_x5Fclosure_x24d5142test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"let$53_func_0", align 8
  ret ptr %2
}

define internal i64 @_M35let_x2472d_x24d4a_x5Fclosure_x24d5242test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr %0, i64 %1) {
  %"x$4$48_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"x$4$48_0" = load i64, ptr %"x$4$48_addr_0", align 4
  %3 = call i64 @malgo_add_int64_t(i64 %"x$4$48_0", i64 %1)
  ret i64 %3
}

define internal i64 @_M23raw_x5Flet_x2453_x24d5042test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal(ptr %0, i64 %_M12x_x244_x244842test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, i64 %_M12y_x245_x244942test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72d$d4a_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"x$4$48_0" = getelementptr { i64 }, ptr %"let$72d$d4a_capture_0", i32 0, i32 0
  store i64 %_M12x_x244_x244842test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, ptr %"x$4$48_0", align 4
  %"let$72d$d4a_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$72d$d4a_capture_0", ptr %"let$72d$d4a_capture_1", align 8
  %"let$72d$d4a_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M35let_x2472d_x24d4a_x5Fclosure_x24d5242test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"let$72d$d4a_func_0", align 8
  %3 = call i64 @malgo_add_int64_t(i64 %_M12x_x244_x244842test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, i64 %_M12y_x245_x244942test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0)
  ret i64 %3
}

define internal ptr @_M34let_x2439_x24d22_x5Fclosure_x24d5342test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$3b_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$3b_0" = load ptr, ptr %"d$3b_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$3b_0")
  ret ptr %7
}

define internal ptr @_M27fun_x2445_x5Fclosure_x24d5442test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M23raw_x5Ffun_x2445_x24d4f42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2429_x243a42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$39$d22_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$3b_0" = getelementptr { ptr }, ptr %"let$39$d22_capture_0", i32 0, i32 0
  store ptr %2, ptr %"d$3b_0", align 8
  %"let$39$d22_capture_1" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %"let$39$d22_capture_0", ptr %"let$39$d22_capture_1", align 8
  %"let$39$d22_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M34let_x2439_x24d22_x5Fclosure_x24d5342test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"let$39$d22_func_0", align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$45_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$45_capture_1" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %"fun$45_capture_0", ptr %"fun$45_capture_1", align 8
  %"fun$45_func_0" = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @_M27fun_x2445_x5Fclosure_x24d5442test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"fun$45_func_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %6)
  ret ptr %11
}

define internal ptr @_M27let_x2439_x5Fclosure_x24d5542test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$2$32_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$2$32_0" = load ptr, ptr %"x$2$32_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"x$2$32_0")
  ret ptr %7
}

define internal ptr @_M8_x7C_x3E42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8External(ptr %0, ptr %_M12x_x242_x243242test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$39_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$2$32_0" = getelementptr { ptr }, ptr %"let$39_capture_0", i32 0, i32 0
  store ptr %_M12x_x242_x243242test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, ptr %"x$2$32_0", align 8
  %"let$39_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$39_capture_0", ptr %"let$39_capture_1", align 8
  %"let$39_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2439_x5Fclosure_x24d5542test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"let$39_func_0", align 8
  ret ptr %2
}

define internal ptr @_M23raw_x5Ffun_x2445_x24d4f42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal(ptr %0, ptr %_M15int64_x23_x243f42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M15int64_x23_x243f42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M15int64_x23_x243f42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call ptr @malgo_int64_t_to_string(i64 %6)
  %8 = call ptr @malgo_print_string(ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test0.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test0.mlg"() {
  ret void
}
