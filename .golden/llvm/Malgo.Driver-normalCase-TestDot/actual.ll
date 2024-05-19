; ModuleID = 'test/testcases/malgo/TestDot.mlg'
source_filename = "test/testcases/malgo/TestDot.mlg"

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

define internal i64 @_M28let_x2472d_x5Fclosure_x24e9944test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$728_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$728_0" = load i64, ptr %"p$728_addr_0", align 4
  %3 = call i64 @malgo_add_int64_t(i64 %"p$728_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M26malgo_x5Fadd_x5Fint64_x5Ft34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i64 %_M8p_x2472834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$72d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$728_0" = getelementptr { i64 }, ptr %"let$72d_capture_0", i32 0, i32 0
  store i64 %_M8p_x2472834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"p$728_0", align 4
  %"let$72d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$72d_capture_0", ptr %"let$72d_capture_1", align 8
  %"let$72d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x2472d_x5Fclosure_x24e9944test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal, ptr %"let$72d_func_0", align 8
  ret ptr %2
}

define internal i64 @_M32raw_x5Flet_x241089_x24e71_x24e9644test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal(ptr %0, i64 %_M14p_x241f_x24e6444test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0, i64 %_M22y_x241d_x24107f_x24e7244test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0) {
  %2 = call ptr @_M26malgo_x5Fadd_x5Fint64_x5Ft34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %_M14p_x241f_x24e6444test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %_M22y_x241d_x24107f_x24e7244test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  ret i64 %7
}

define internal i64 @_M32raw_x5Flet_x241089_x24e7a_x24e9744test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal(ptr %0, i64 %_M14p_x241f_x24e6b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0, i64 %_M22y_x241d_x24107f_x24e7b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0) {
  %2 = call ptr @_M26malgo_x5Fadd_x5Fint64_x5Ft34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %_M14p_x241f_x24e6b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %_M22y_x241d_x24107f_x24e7b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  ret i64 %7
}

define internal i64 @_M32raw_x5Flet_x241089_x24e83_x24e9844test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal(ptr %0, i64 %_M14p_x241f_x24e6b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0, i64 %_M22y_x241d_x24107f_x24e8444test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0) {
  %2 = call ptr @_M26malgo_x5Fadd_x5Fint64_x5Ft34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i64 %_M14p_x241f_x24e6b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %_M22y_x241d_x24107f_x24e8444test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0)
  ret i64 %7
}

define internal i64 @_M36let_x241089_x24e71_x5Fclosure_x24e9a44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$1f$e64_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$1f$e64_0" = load i64, ptr %"p$1f$e64_addr_0", align 4
  %3 = call i64 @_M32raw_x5Flet_x241089_x24e71_x24e9644test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal(ptr null, i64 %"p$1f$e64_0", i64 %1)
  ret i64 %3
}

define internal i64 @_M36let_x241089_x24e7a_x5Fclosure_x24e9b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal(ptr %0, i64 %1) {
  %"p$1f$e6b_addr_0" = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %"p$1f$e6b_0" = load i64, ptr %"p$1f$e6b_addr_0", align 4
  %3 = call i64 @_M32raw_x5Flet_x241089_x24e7a_x24e9744test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal(ptr null, i64 %"p$1f$e6b_0", i64 %1)
  ret i64 %3
}

define internal ptr @_M4main44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x241d_x242644test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_3 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %7 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i64 }, ptr %7, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$1089$e71_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$1f$e64_0" = getelementptr { i64 }, ptr %"let$1089$e71_capture_0", i32 0, i32 0
  store i64 %9, ptr %"p$1f$e64_0", align 4
  %"let$1089$e71_capture_1" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr %"let$1089$e71_capture_0", ptr %"let$1089$e71_capture_1", align 8
  %"let$1089$e71_func_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @_M36let_x241089_x24e71_x5Fclosure_x24e9a44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal, ptr %"let$1089$e71_func_0", align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call i64 %14(ptr %12, i64 1)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, { i64 } }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i64 } }, ptr %16, i32 0, i32 1, i32 0
  store i64 %15, ptr %18, align 4
  %19 = getelementptr { i8, <8 x i8> }, ptr %16, i32 0, i32 0
  %20 = load i8, ptr %19, align 1
  switch i8 %20, label %switch_default_2 [
    i8 0, label %"switch_branch_Int64#_1"
  ]

"switch_branch_Int64#_1":                         ; preds = %"switch_branch_Int64#_0"
  %21 = getelementptr { i8, { i64 } }, ptr %16, i32 0, i32 1
  %22 = getelementptr { i64 }, ptr %21, i32 0, i32 0
  %23 = load i64, ptr %22, align 4
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$1089$e7a_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %"p$1f$e6b_0" = getelementptr { i64 }, ptr %"let$1089$e7a_capture_0", i32 0, i32 0
  store i64 %23, ptr %"p$1f$e6b_0", align 4
  %"let$1089$e7a_capture_1" = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  store ptr %"let$1089$e7a_capture_0", ptr %"let$1089$e7a_capture_1", align 8
  %"let$1089$e7a_func_0" = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  store ptr @_M36let_x241089_x24e7a_x5Fclosure_x24e9b44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Internal, ptr %"let$1089$e7a_func_0", align 8
  %25 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call i64 %28(ptr %26, i64 1)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, { i64 } }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { i64 } }, ptr %30, i32 0, i32 1, i32 0
  store i64 %29, ptr %32, align 4
  %33 = getelementptr { i8, <8 x i8> }, ptr %30, i32 0, i32 0
  %34 = load i8, ptr %33, align 1
  switch i8 %34, label %switch_default_1 [
    i8 0, label %"switch_branch_Int64#_2"
  ]

"switch_branch_Int64#_2":                         ; preds = %"switch_branch_Int64#_1"
  %35 = getelementptr { i8, { i64 } }, ptr %30, i32 0, i32 1
  %36 = getelementptr { i64 }, ptr %35, i32 0, i32 0
  %37 = load i64, ptr %36, align 4
  %38 = call ptr @malgo_int64_t_to_string(i64 %37)
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %40 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 0
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 1, i32 0
  store ptr %38, ptr %41, align 8
  %42 = getelementptr { i8, <8 x i8> }, ptr %39, i32 0, i32 0
  %43 = load i8, ptr %42, align 1
  switch i8 %43, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int64#_2"
  %44 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 1
  %45 = getelementptr { ptr }, ptr %44, i32 0, i32 0
  %46 = load ptr, ptr %45, align 8
  %47 = call ptr @malgo_print_string(ptr %46)
  ret ptr %47

switch_default_0:                                 ; preds = %"switch_branch_Int64#_2"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_Int64#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_Int64#_0"
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4succ44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8External(ptr %0, ptr %_M15int64_x23_x241e44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M15int64_x23_x241e44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Int64#_0"
  ]

"switch_branch_Int64#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %_M15int64_x23_x241e44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call i64 @malgo_add_int64_t(i64 %6, i64 1)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { i64 } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %8, i32 0, i32 1, i32 0
  store i64 %7, ptr %10, align 4
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestDot.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestDot.mlg"() {
  ret void
}
