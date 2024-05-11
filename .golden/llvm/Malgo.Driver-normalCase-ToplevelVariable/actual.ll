; ModuleID = 'test/testcases/malgo/ToplevelVariable.mlg'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External = global ptr undef
@_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External = global ptr undef
@str3779 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3780(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3775(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3781(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3777(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg11Temporal104_0) {
  %2 = load ptr, ptr @_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_5 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr @str3779, ptr %7, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %5, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_2 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Nothing_0
  %10 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %const_func_0, align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %identity_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %15, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3780, ptr %let_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %14)
  %22 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %23 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %22)
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %29 = call ptr %27(ptr %25, ptr %28)
  %30 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %31 = load ptr, ptr %30, align 8
  %32 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr %33(ptr %31, ptr %29)
  %35 = getelementptr { i8, <4 x i8> }, ptr %34, i32 0, i32 0
  %36 = load i8, ptr %35, align 1
  switch i8 %36, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %"switch_branch_String#_0"
  %37 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1
  %38 = getelementptr { i32 }, ptr %37, i32 0, i32 0
  %39 = load i32, ptr %38, align 4
  %40 = call ptr @malgo_int32_t_to_string(i32 %39)
  %41 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %42 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 0
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1, i32 0
  store ptr %40, ptr %43, align 8
  %44 = getelementptr { i8, <8 x i8> }, ptr %41, i32 0, i32 0
  %45 = load i8, ptr %44, align 1
  switch i8 %45, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_1"
  ]

"switch_branch_String#_1":                        ; preds = %"switch_branch_Int32#_0"
  %46 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1
  %47 = getelementptr { ptr }, ptr %46, i32 0, i32 0
  %48 = load ptr, ptr %47, align 8
  %49 = call ptr @malgo_print_string(ptr %48)
  ret ptr %49

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_String#_0"
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Nothing_0
  unreachable

switch_branch_Just_0:                             ; preds = %1
  %50 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %51 = getelementptr { ptr }, ptr %50, i32 0, i32 0
  %52 = load ptr, ptr %51, align 8
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_1 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  store ptr null, ptr %const_capture_1, align 8
  %const_func_1 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  store ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %const_func_1, align 8
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  store ptr null, ptr %identity_capture_1, align 8
  %identity_func_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  store ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %identity_func_1, align 8
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %54, ptr %cast_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3781, ptr %let_func_1, align 8
  %56 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  %57 = load ptr, ptr %56, align 8
  %58 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  %59 = load ptr, ptr %58, align 8
  %60 = call ptr %59(ptr %57, ptr %53)
  %61 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %62 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %61)
  %63 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 0
  %64 = load ptr, ptr %63, align 8
  %65 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 1
  %66 = load ptr, ptr %65, align 8
  %67 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %68 = call ptr %66(ptr %64, ptr %67)
  %69 = getelementptr { ptr, ptr }, ptr %60, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = getelementptr { ptr, ptr }, ptr %60, i32 0, i32 1
  %72 = load ptr, ptr %71, align 8
  %73 = call ptr %72(ptr %70, ptr %68)
  %74 = getelementptr { i8, <4 x i8> }, ptr %73, i32 0, i32 0
  %75 = load i8, ptr %74, align 1
  switch i8 %75, label %switch_default_4 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %switch_branch_Just_0
  %76 = getelementptr { i8, { i32 } }, ptr %73, i32 0, i32 1
  %77 = getelementptr { i32 }, ptr %76, i32 0, i32 0
  %78 = load i32, ptr %77, align 4
  %79 = call ptr @malgo_int32_t_to_string(i32 %78)
  %80 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %81 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 0
  store i8 0, ptr %81, align 1
  %82 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 1, i32 0
  store ptr %79, ptr %82, align 8
  %83 = getelementptr { i8, <8 x i8> }, ptr %80, i32 0, i32 0
  %84 = load i8, ptr %83, align 1
  switch i8 %84, label %switch_default_3 [
    i8 0, label %"switch_branch_String#_2"
  ]

"switch_branch_String#_2":                        ; preds = %"switch_branch_Int32#_1"
  %85 = getelementptr { i8, { ptr } }, ptr %80, i32 0, i32 1
  %86 = getelementptr { ptr }, ptr %85, i32 0, i32 0
  %87 = load ptr, ptr %86, align 8
  %88 = call ptr @malgo_print_string(ptr %87)
  ret ptr %88

switch_default_3:                                 ; preds = %"switch_branch_Int32#_1"
  unreachable

switch_default_4:                                 ; preds = %switch_branch_Just_0
  unreachable

switch_default_5:                                 ; preds = %1
  unreachable
}

define internal ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3782(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3772(ptr null, ptr %"int32#_0", ptr %1)
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
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3782, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3783(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3773(ptr null, ptr %a_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M5const34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M1a34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal956_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %a_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %_M1a34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal956_0, ptr %a_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3783, ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3784(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3774(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @_M7constId53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External(ptr %0, ptr %_M3eta53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg10Temporal87_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %2, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3784, ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %_M3eta53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg10Temporal87_0)
  ret ptr %8
}

define internal ptr @_M6addOne53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External(ptr %0, ptr %_M3eta53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg11Temporal102_0) {
  %2 = load ptr, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %3 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %2)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %_M3eta53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg11Temporal102_0)
  ret ptr %8
}

define internal ptr @_M8identity34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M1x34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal890_0) {
  ret ptr %_M1x34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal890_0
}

define internal i32 @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3785(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3772(ptr %0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4039_0, ptr %_M9int32_x2334runtime_x2Fmalgo_x2FBuiltin_x2Emlg12Temporal4040_0) {
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
  store ptr @_M14let_x5Fclosure53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Internal3785, ptr %let_func_0, align 8
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

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3773(ptr %0, ptr %_M1a34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal956_0, ptr %_M4_x5F34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal957_0) {
  ret ptr %_M1a34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal956_0
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3774(ptr %0, ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg10Temporal84_0, ptr %_M4_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3690_0) {
  ret ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg10Temporal84_0
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3775(ptr %0, ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0, ptr %_M4_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3717_0) {
  ret ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3776(ptr %0, ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0, ptr %_M4_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3717_0) {
  ret ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3777(ptr %0, ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0, ptr %_M4_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3730_0) {
  ret ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0
}

define internal ptr @_M10raw_x5Flet53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3778(ptr %0, ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0, ptr %_M4_x5F53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3743_0) {
  ret ptr %_M4cast53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg12Temporal3704_0
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/ToplevelVariable.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/ToplevelVariable.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %1, ptr @_M3one53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 0
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1, i32 0
  store ptr %4, ptr %8, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %0
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  store ptr %11, ptr @_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  ret void

switch_branch_Just_0:                             ; preds = %0
  %13 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  store ptr %16, ptr @_M4comp53test_x2Ftestcases_x2Fmalgo_x2FToplevelVariable_x2Emlg8External, align 8
  ret void

switch_default_0:                                 ; preds = %0
  unreachable
}
