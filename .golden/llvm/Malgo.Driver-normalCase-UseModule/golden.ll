; ModuleID = 'test/testcases/malgo/UseModule.mlg'
source_filename = "test/testcases/malgo/UseModule.mlg"

@str3706 = unnamed_addr constant [14 x i8] c"Hello, world!\00"

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

define internal ptr @_M35let_x24468_x24e26_x5Fclosure_x24e7b46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$2f_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$2f_0" = load ptr, ptr %"cast$2f_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %"cast$2f_0", i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %"cast$2f_0", i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %1)
  ret ptr %7
}

define internal ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2423_x242a46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str3706, ptr %4, align 8
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %7 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_print_string(ptr %9)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_newline(ptr %13)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printInt32_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %printInt32_capture_0, align 8
  %printInt32_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M10printInt3234runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %printInt32_func_0, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$468$e26_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$2f_0" = getelementptr { ptr }, ptr %"let$468$e26_capture_0", i32 0, i32 0
  store ptr %16, ptr %"cast$2f_0", align 8
  %"let$468$e26_capture_1" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %"let$468$e26_capture_0", ptr %"let$468$e26_capture_1", align 8
  %"let$468$e26_func_0" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @_M35let_x24468_x24e26_x5Fclosure_x24e7b46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal, ptr %"let$468$e26_func_0", align 8
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 1, ptr %20, align 4
  %21 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %18)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, { i32 } }, ptr %22, i32 0, i32 0
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { i32 } }, ptr %22, i32 0, i32 1, i32 0
  store i32 1, ptr %24, align 4
  %25 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr %28(ptr %26, ptr %22)
  %30 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %29)
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %32 = getelementptr { i8, { i32 } }, ptr %31, i32 0, i32 0
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { i32 } }, ptr %31, i32 0, i32 1, i32 0
  store i32 1, ptr %33, align 4
  %34 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr %37(ptr %35, ptr %31)
  %39 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %40 = load ptr, ptr %39, align 8
  %41 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %42 = load ptr, ptr %41, align 8
  %43 = call ptr %42(ptr %40, ptr %38)
  ret ptr %43

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M28let_x24fda_x5Fclosure_x24e7c46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$fc7_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$fc7_0" = load ptr, ptr %"int32#$fc7_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24fda_x24e7946test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Temporal(ptr null, ptr %"int32#$fc7_0", ptr %1)
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
  store ptr @_M28let_x24fda_x5Fclosure_x24e7c46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal, ptr %"let$fda_func_0", align 8
  ret ptr %2
}

define internal ptr @_M10printInt3234runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M14i_x245c_x2430534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M14i_x245c_x2430534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M14i_x245c_x2430534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
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

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int32#_0"
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4succ46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8External(ptr %0, ptr %_M12x_x240_x242446test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Temporal_0) {
  %2 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x240_x242446test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Temporal_0)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %3)
  ret ptr %10
}

define internal i32 @_M35let_x24715_x24c90_x5Fclosure_x24e7d46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$fc9$fd3$c73_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$fc9$fd3$c73_0" = load i32, ptr %"p$fc9$fd3$c73_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"p$fc9$fd3$c73_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M24raw_x5Flet_x24fda_x24e7946test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Temporal(ptr %0, ptr %_M16int32_x23_x24fc734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M16int32_x23_x24fc834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
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
  %"let$715$c90_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$fc9$fd3$c73_0" = getelementptr { i32 }, ptr %"let$715$c90_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$fc9$fd3$c73_0", align 4
  %"let$715$c90_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$715$c90_capture_0", ptr %"let$715$c90_capture_1", align 8
  %"let$715$c90_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24715_x24c90_x5Fclosure_x24e7d46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8Internal, ptr %"let$715$c90_func_0", align 8
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
  call void @"malgo_load_test/testcases/malgo/UseModule.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FUseModule_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/UseModule.mlg"() {
  ret void
}
