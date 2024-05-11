; ModuleID = 'test/testcases/malgo/Seq.mlg'
source_filename = "test/testcases/malgo/Seq.mlg"

@_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External = global ptr undef

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

define internal ptr @_M4main40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg10Temporal43_0) {
  %2 = load ptr, ptr @_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %5 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr %8, ptr %11, align 8
  %12 = getelementptr { i8, <8 x i8> }, ptr %9, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int32#_0"
  %14 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1
  %15 = getelementptr { ptr }, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr @malgo_print_string(ptr %16)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Internal3632(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Temporal3631(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Internal3632, ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @_M14let_x5Fclosure40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Internal3633(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Temporal3631(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4040_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4040_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4040_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg12Internal3633, ptr %let_func_0, align 8
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
  call void @"malgo_load_test/testcases/malgo/Seq.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Seq.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1, i32 0
  store i32 2, ptr %6, align 4
  %7 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %1)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %4)
  %13 = getelementptr { i8, <4 x i8> }, ptr %12, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %0
  %15 = getelementptr { i8, { i32 } }, ptr %12, i32 0, i32 1
  %16 = getelementptr { i32 }, ptr %15, i32 0, i32 0
  %17 = load i32, ptr %16, align 4
  %18 = call ptr @malgo_int32_t_to_string(i32 %17)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr %18, ptr %21, align 8
  %22 = getelementptr { i8, <8 x i8> }, ptr %19, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int32#_0"
  %24 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1
  %25 = getelementptr { ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr @malgo_print_string(ptr %26)
  %28 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %1)
  %29 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %4)
  store ptr %33, ptr @_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External, align 8
  ret void

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %0
  unreachable
}
