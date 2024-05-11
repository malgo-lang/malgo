; ModuleID = 'test/testcases/malgo/RecordTest.mlg'
source_filename = "test/testcases/malgo/RecordTest.mlg"

@str3662 = unnamed_addr constant [2 x i8] c"a\00"
@str3663 = unnamed_addr constant [2 x i8] c"b\00"

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

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal59_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 32, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 1, i32 0
  store i32 10, ptr %7, align 4
  %8 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %8, ptr @str3662, ptr %2)
  call void @malgo_hash_table_insert(ptr %8, ptr @str3663, ptr %5)
  %9 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3662)
  %10 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3663)
  %11 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %9)
  %12 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3662)
  %13 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3663)
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  %19 = getelementptr { i8, <4 x i8> }, ptr %18, i32 0, i32 0
  %20 = load i8, ptr %19, align 1
  switch i8 %20, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %21 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1
  %22 = getelementptr { i32 }, ptr %21, i32 0, i32 0
  %23 = load i32, ptr %22, align 4
  %24 = call ptr @malgo_int32_t_to_string(i32 %23)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1, i32 0
  store ptr %24, ptr %27, align 8
  %28 = getelementptr { i8, <8 x i8> }, ptr %25, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %"switch_branch_Int32#_0"
  %30 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1
  %31 = getelementptr { ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr @malgo_print_string(ptr %32)
  ret ptr %33

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M1B47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg8External(ptr %0, ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal50_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal50_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M1f47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg8External(ptr %0, ptr %_M6record47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal56_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M6record47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal56_0, ptr @str3662)
  %3 = call ptr @malgo_hash_table_get(ptr %_M6record47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal56_0, ptr @str3663)
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3664(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Temporal3661(ptr null, ptr %"int32#_0", ptr %1)
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
  store ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3664, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M1g47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg8External(ptr %0, ptr %_M1b47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal52_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1b47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal52_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_B_0
  ]

switch_branch_B_0:                                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1b47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg10Temporal52_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_hash_table_get(ptr %6, ptr @str3662)
  %8 = call ptr @malgo_hash_table_get(ptr %6, ptr @str3663)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3665(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Temporal3661(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4040_0) {
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
  store ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3665, ptr %let_func_0, align 8
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
  call void @"malgo_load_test/testcases/malgo/RecordTest.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/RecordTest.mlg"() {
  ret void
}
