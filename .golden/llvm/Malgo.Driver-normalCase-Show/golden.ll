; ModuleID = 'test/testcases/malgo/Show.mlg'
source_filename = "test/testcases/malgo/Show.mlg"

@_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External = global ptr undef
@str3224 = unnamed_addr constant [5 x i8] c"show\00"
@str3226 = unnamed_addr constant [2 x i8] c"(\00"
@str3227 = unnamed_addr constant [3 x i8] c", \00"
@str3228 = unnamed_addr constant [2 x i8] c")\00"

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

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3220(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M16appendString_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3963_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3963_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3220, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1802_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1802_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M25appendString_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3968(ptr %0, ptr %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3969_0, ptr %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3970_0) {
  %2 = call ptr @malgo_string_append(ptr %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3969_0, ptr %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3970_0)
  ret ptr %2
}

define internal ptr @_M34malgo_x5Fstring_x5Fappend_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2142(ptr %0, ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2143_0, ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2144_0) {
  %2 = call ptr @malgo_string_append(ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2143_0, ptr %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2144_0)
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3221(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3219(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal225_0) {
  %2 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %3 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %4 = call ptr @_M19showTuple2_x5Fcurry41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal186(ptr null, ptr %2, ptr %3)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %4, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3221, ptr %let_func_0, align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 2, ptr %11, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %6, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 1
  store ptr %9, ptr %15, align 8
  %16 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %12)
  ret ptr %20
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3222(ptr %0, ptr %1) {
  %showDictA_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %3 = call ptr @_M19showTuple2_x5Fcurry41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal186(ptr null, ptr %showDictA_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10showTuple241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal149_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDictA_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal149_0, ptr %showDictA_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3222, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3223(ptr %0, ptr %1) {
  %showDict_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDict_0 = load ptr, ptr %showDict_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3217(ptr null, ptr %showDict_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M5print41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M8showDict41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal121_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDict_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M8showDict41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal121_0, ptr %showDict_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3223, ptr %let_func_0, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @_M4show41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M6record41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal119_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M6record41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal119_0, ptr @str3224)
  ret ptr %2
}

define internal ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3225(ptr %0, ptr %1) {
  %showDictA_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %showDictB_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3216(ptr null, ptr %showDictA_0, ptr %showDictB_0, ptr %1)
  ret ptr %3
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal ptr @_M19showTuple2_x5Fcurry41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal186(ptr %0, ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal187_0, ptr %_M9showDictB41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal188_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %showDictA_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal187_0, ptr %showDictA_0, align 8
  %showDictB_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %_M9showDictB41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal188_0, ptr %showDictB_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3225, ptr %fun_func_0, align 8
  %3 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %3, ptr @str3224, ptr %2)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3229(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3208(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3230(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3212(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3231(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3214(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3232(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3215(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Ffun41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3216(ptr %0, ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal187_0, ptr %_M9showDictB41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal188_0, ptr %_M5tuple41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal190_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M5tuple41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal190_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %_M5tuple41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal190_0, i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr @str3226, ptr %11, align 8
  %12 = call ptr @malgo_hash_table_get(ptr %_M9showDictA41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal187_0, ptr @str3224)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr @str3227, ptr %20, align 8
  %21 = call ptr @malgo_hash_table_get(ptr %_M9showDictB41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal188_0, ptr @str3224)
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %8)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr @str3228, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %26, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3229, ptr %let_func_0, align 8
  %31 = call ptr @_M21appendString_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3984(ptr null, ptr %26, ptr %27)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %18, ptr %d_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3230, ptr %let_func_1, align 8
  %33 = call ptr @_M21appendString_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3984(ptr null, ptr %18, ptr %31)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %17, ptr %d_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3231, ptr %let_func_2, align 8
  %35 = call ptr @_M21appendString_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3984(ptr null, ptr %17, ptr %33)
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %9, ptr %d_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3232, ptr %let_func_3, align 8
  %37 = call ptr @_M21appendString_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3984(ptr null, ptr %9, ptr %35)
  ret ptr %37

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3217(ptr %0, ptr %_M8showDict41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal121_0, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal122_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M8showDict41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal121_0, ptr @str3224)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal122_0)
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3219(ptr %0, ptr %_M4cast41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal230_0, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3173_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M4cast41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal230_0, ptr @str3224)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3173_0)
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3233(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3218(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3C_x3E41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal137_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal137_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3233, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3234(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M21appendString_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3984(ptr %0, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3985_0, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3986_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3985_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3985_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3986_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M10string_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3986_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3234, ptr %let_func_0, align 8
  %13 = call ptr @_M34malgo_x5Fstring_x5Fappend_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2142(ptr null, ptr %6, ptr %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  ret ptr %14

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3208(ptr %0, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal209_0, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3060_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal209_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal209_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3060_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3060_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @_M16appendString_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @_M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3212(ptr %0, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal203_0, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3062_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal203_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal203_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3062_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3062_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @_M16appendString_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @_M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3214(ptr %0, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal199_0, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3064_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal199_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal199_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3064_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3064_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @_M16appendString_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @_M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3215(ptr %0, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal193_0, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3066_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal193_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal193_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3066_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M10string_x2341test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3066_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @_M16appendString_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @_M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3235(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @_M25appendString_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal3968(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Temporal3218(ptr %0, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal137_0, ptr %_M1y41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal138_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal137_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M1x41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal137_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M1y41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal138_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { ptr } }, ptr %_M1y41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg11Temporal138_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3235, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %17, ptr %20, align 8
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Show.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3240(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr %8, ptr %11, align 8
  ret ptr %9

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal void @"malgo_load_test/testcases/malgo/Show.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg12Internal3240, ptr %fun_func_0, align 8
  %2 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %2, ptr @str3224, ptr %1)
  store ptr %2, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  ret void
}
