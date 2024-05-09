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

define internal ptr @_M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3486(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3418(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3486, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3416(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3142_0) {
  %2 = call ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr null, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0)
  %3 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %2)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3142_0)
  ret ptr %12
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3487(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3416(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3417(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3135_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3487, ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %2)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3135_0)
  ret ptr %12
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3488(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3417(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3418(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal113_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3488, ptr %let_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal112_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %2)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal113_0)
  ret ptr %12
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3489(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3484(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M3fun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal245(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3489, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3490(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3468(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3491(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Ffun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3483(ptr null, ptr %f_0, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3484(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3490, ptr %let_func_0, align 8
  %6 = call ptr @_M16leInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2853(ptr null, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %2)
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, ptr %f_0, align 8
  %n_1 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %n_1, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3491, ptr %fun_func_0, align 8
  %8 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %switch_branch__M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
    i8 1, label %switch_branch__M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %10)
  ret ptr %16

switch_branch__M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, {} }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 1, i32 0
  store i32 1, ptr %21, align 4
  ret ptr %19

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3492(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3479(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3493(ptr %0, ptr %1) {
  %n_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3481(ptr null, ptr %n_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3494(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3482(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M10raw_x5Ffun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3483(ptr %0, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %_M8_x24_x5F51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3270_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %n_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3492, ptr %let_func_0, align 8
  %6 = call ptr @_M17subInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2308(ptr null, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %2)
  %7 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %6)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { i32 } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %12, i32 0, i32 1, i32 0
  store i32 2, ptr %14, align 4
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %n_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %n_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3493, ptr %let_func_1, align 8
  %16 = call ptr @_M17subInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2308(ptr null, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %12)
  %17 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %_M1f51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal165_0, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %16)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %11, ptr %d_0, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3494, ptr %let_func_2, align 8
  %23 = call ptr @_M17addInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4048(ptr null, ptr %11, ptr %21)
  ret ptr %23
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3495(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M12addInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4027_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4027_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3495, ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3496(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_le_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M11leInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2727_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2727_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3496, ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3497(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M12subInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2287_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2287_0, ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3497, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M35malgo_x5Fint32_x5Ft_x5Fto_x5Fstring34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2155_0) {
  %2 = call ptr @malgo_int32_t_to_string(i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2155_0)
  ret ptr %2
}

define internal ptr @_M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1792_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1792_0, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M10isTrue_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, i32 %_M7unboxed34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2777_0) {
  switch i32 %_M7unboxed34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2777_0, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %1
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2

switch-unboxed_default_0:                         ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4
}

define internal i32 @_M21addInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4032(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4033_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4034_0) {
  %2 = call i32 @malgo_add_int32_t(i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4033_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4034_0)
  ret i32 %2
}

define internal i32 @_M20leInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2732(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2733_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2734_0) {
  %2 = call i32 @malgo_le_int32_t(i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2733_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2734_0)
  ret i32 %2
}

define internal i32 @_M35malgo_x5Fadd_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1810(ptr %0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1811_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1812_0) {
  %2 = call i32 @malgo_add_int32_t(i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1811_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1812_0)
  ret i32 %2
}

define internal i32 @_M34malgo_x5Fle_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1932(ptr %0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1933_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1934_0) {
  %2 = call i32 @malgo_le_int32_t(i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1933_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1934_0)
  ret i32 %2
}

define internal i32 @_M35malgo_x5Fsub_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1816(ptr %0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1817_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1818_0) {
  %2 = call i32 @malgo_sub_int32_t(i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1817_0, i32 %_M1p34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1818_0)
  ret i32 %2
}

define internal i32 @_M21subInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2292(ptr %0, i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2293_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2294_0) {
  %2 = call i32 @malgo_sub_int32_t(i32 %_M1x34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2293_0, i32 %_M1y34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2294_0)
  ret i32 %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3498(ptr %0, ptr %1) {
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3485(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3499(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @_M4main51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal164_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 5, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3498, ptr %let_func_0, align 8
  %6 = call ptr @_M3fun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal245(ptr null, ptr %5)
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %2)
  %12 = getelementptr { i8, <4 x i8> }, ptr %11, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %14 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = call ptr @_M35malgo_x5Fint32_x5Ft_x5Fto_x5Fstring34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %16)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %17, ptr %20, align 8
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %18, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3499, ptr %let_func_1, align 8
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

define internal ptr @_M3fun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3263(ptr %0, ptr %_M8_x24_x5F51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3264_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3500(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3421(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2B51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal152_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal152_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3500, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3501(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3420(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4_x2D51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal140_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal140_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3501, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3502(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3419(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3C_x3D51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal128_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal128_0, ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3502, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M10String_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i32 0, i32 1
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

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3485(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3362_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %fun_capture_0, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M3fun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal245, ptr %fun_func_0, align 8
  %3 = call ptr @_M3fix51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg8External(ptr null, ptr %2)
  %4 = call ptr @_M3fun51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal245(ptr null, ptr %3)
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3362_0)
  ret ptr %9
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3503(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M17addInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4048(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4049_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4050_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4049_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4049_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4050_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4050_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3503, ptr %let_func_0, align 8
  %13 = call i32 @_M35malgo_x5Fadd_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1810(ptr null, i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3504(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M16leInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2853(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2854_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2855_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2854_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2854_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2855_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2855_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3504, ptr %let_func_0, align 8
  %13 = call i32 @_M34malgo_x5Fle_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1932(ptr null, i32 %6, i32 %11)
  switch i32 %13, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 1, ptr %15, align 1
  ret ptr %14

switch-unboxed_default_0:                         ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  ret ptr %16

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3505(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @_M20leInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2732(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3419(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal128_0, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal129_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal128_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal128_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal129_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal129_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3505, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  switch i32 %17, label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %18 = call ptr @_M4True34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null)
  ret ptr %18

switch-unboxed_default_0:                         ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  %19 = call ptr @_M5False34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null)
  ret ptr %19

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3506(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @_M21subInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2292(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3420(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal140_0, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal141_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal140_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal140_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal141_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal141_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3506, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3507(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @_M21addInt32_x23_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4032(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3421(ptr %0, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal152_0, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal153_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal152_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1x51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal152_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal153_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M1y51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal153_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3507, ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3468(ptr %0, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3349_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3349_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3349_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @_M11leInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @_M10isTrue_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3479(ptr %0, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3351_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3351_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3351_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @_M12subInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @_M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3481(ptr %0, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3353_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1n51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg11Temporal166_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3353_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3353_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @_M12subInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @_M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M10raw_x5Flet51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3482(ptr %0, ptr %_M1d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3280_0, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3355_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M1d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3280_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M1d51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3280_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3355_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2351test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Temporal3355_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @_M12addInt32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @_M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3508(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M17subInt32_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2308(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2309_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2310_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2309_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0: ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2309_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2310_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1
  ]

switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_1: ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
  %9 = getelementptr { i8, { i32 } }, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal2310_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3508, ptr %let_func_0, align 8
  %13 = call i32 @_M35malgo_x5Fsub_x5Fint32_x5Ft_x5Fcurry34runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal1816(ptr null, i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %switch_branch__M9Int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External_0
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
