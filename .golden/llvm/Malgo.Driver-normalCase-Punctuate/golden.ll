; ModuleID = 'test/testcases/malgo/Punctuate.mlg'
source_filename = "test/testcases/malgo/Punctuate.mlg"

@str3841 = unnamed_addr constant [2 x i8] c"x\00"
@str3842 = unnamed_addr constant [2 x i8] c"y\00"
@str3843 = unnamed_addr constant [2 x i8] c"z\00"
@str3846 = unnamed_addr constant [1 x i8] zeroinitializer
@str3847 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str3848 = unnamed_addr constant [6 x i8] c"SInt \00"
@str3849 = unnamed_addr constant [8 x i8] c"SList [\00"
@str3850 = unnamed_addr constant [3 x i8] c", \00"
@str3851 = unnamed_addr constant [2 x i8] c"]\00"

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

define internal ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr %0, ptr %1) {
  %"_$26$309_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"_$26$309_0" = load ptr, ptr %"_$26$309_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24324_x24eff46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr null, ptr %"_$26$309_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7mapList34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$324_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"_$26$309_0" = getelementptr { ptr }, ptr %"let$324_capture_0", i32 0, i32 0
  store ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"_$26$309_0", align 8
  %"let$324_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$324_capture_0", ptr %"let$324_capture_1", align 8
  %"let$324_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24324_x5Fclosure_x24f0046test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$324_func_0", align 8
  ret ptr %2
}

define internal ptr @_M24raw_x5Flet_x24324_x24eff46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr %0, ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M10nil_x2430a34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M10nil_x2430a34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %_M10nil_x2430a34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = call ptr @_M7mapList34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %_M17_x5F_x2426_x2430934runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0)
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %10)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 0
  store i8 1, ptr %23, align 1
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 0
  store ptr %15, ptr %24, align 8
  %25 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 1
  store ptr %21, ptr %25, align 8
  ret ptr %22

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2470_x249f46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str3841, ptr %4, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr @str3842, ptr %10, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1, i32 0
  store ptr %8, ptr %13, align 8
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr @str3843, ptr %16, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %14, ptr %19, align 8
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, {} }, ptr %20, i32 0, i32 0
  store i8 0, ptr %21, align 1
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 0
  store i8 1, ptr %23, align 1
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 0
  store ptr %17, ptr %24, align 8
  %25 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 1
  store ptr %20, ptr %25, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 2, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %22, ptr %28, align 8
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 0
  store i8 1, ptr %32, align 1
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 1, i32 0
  store ptr %26, ptr %33, align 8
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 1, i32 1
  store ptr %29, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 0
  store i8 1, ptr %36, align 1
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 1, i32 0
  store ptr %11, ptr %37, align 8
  %38 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 1, i32 1
  store ptr %31, ptr %38, align 8
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %40 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 0
  store i8 2, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 1, i32 0
  store ptr %35, ptr %41, align 8
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %43 = getelementptr { i8, {} }, ptr %42, i32 0, i32 0
  store i8 0, ptr %43, align 1
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 0
  store i8 1, ptr %45, align 1
  %46 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr %39, ptr %46, align 8
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 1
  store ptr %42, ptr %47, align 8
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %49 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 0
  store i8 1, ptr %49, align 1
  %50 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 1, i32 0
  store ptr %5, ptr %50, align 8
  %51 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 1, i32 1
  store ptr %44, ptr %51, align 8
  %52 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %53 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 0
  store i8 2, ptr %53, align 1
  %54 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 1, i32 0
  store ptr %48, ptr %54, align 8
  %55 = call ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr null, ptr %52)
  %56 = getelementptr { i8, <8 x i8> }, ptr %55, i32 0, i32 0
  %57 = load i8, ptr %56, align 1
  switch i8 %57, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %58 = getelementptr { i8, { ptr } }, ptr %55, i32 0, i32 1
  %59 = getelementptr { ptr }, ptr %58, i32 0, i32 0
  %60 = load ptr, ptr %59, align 8
  %61 = call ptr @malgo_print_string(ptr %60)
  %62 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %63 = getelementptr { i8, {} }, ptr %62, i32 0, i32 0
  store i8 0, ptr %63, align 1
  %64 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %65 = getelementptr { i8, {} }, ptr %64, i32 0, i32 0
  store i8 0, ptr %65, align 1
  %66 = call ptr @malgo_newline(ptr %64)
  ret ptr %66

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M8_x3C_x3E46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M9eta_x247746test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M9eta_x247746test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr %0, ptr %1) {
  %"string#$f87_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#$f87_0" = load ptr, ptr %"string#$f87_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24f9a_x24efd46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr null, ptr %"string#$f87_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$f9a_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#$f87_0" = getelementptr { ptr }, ptr %"let$f9a_capture_0", i32 0, i32 0
  store ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"string#$f87_0", align 8
  %"let$f9a_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$f9a_capture_0", ptr %"let$f9a_capture_1", align 8
  %"let$f9a_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f0446test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$f9a_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr %0, ptr %1) {
  %"_$37$2d5_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"_$37$2d5_0" = load ptr, ptr %"_$37$2d5_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24300_x24efe46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr null, ptr %"_$37$2d5_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M9punctuate34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M17_x5F_x2437_x242d534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$300_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"_$37$2d5_0" = getelementptr { ptr }, ptr %"let$300_capture_0", i32 0, i32 0
  store ptr %_M17_x5F_x2437_x242d534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %"_$37$2d5_0", align 8
  %"let$300_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$300_capture_0", ptr %"let$300_capture_1", align 8
  %"let$300_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24300_x5Fclosure_x24f0546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$300_func_0", align 8
  ret ptr %2
}

define internal ptr @_M4SInt46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M7p_x247346test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x247346test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M5SList46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M7p_x247546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x247546test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M6Symbol46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M7p_x247146test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x247146test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1, i32 0
  store ptr @str3846, ptr %6, align 8
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %_M10nil_x243ce34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %9)
  %13 = call ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %11)
  %14 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr %0, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch_Symbol_0
    i8 1, label %switch_branch_SInt_0
    i8 2, label %switch_branch_SList_0
  ]

switch_branch_Symbol_0:                           ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr @str3847, ptr %9, align 8
  %10 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %7)
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %6)
  ret ptr %15

switch_branch_SInt_0:                             ; preds = %1
  %16 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i32 0, i32 1
  %17 = getelementptr { ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr @str3848, ptr %21, align 8
  %22 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %19)
  %23 = getelementptr { i8, <4 x i8> }, ptr %18, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_SInt_0
  %25 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1
  %26 = getelementptr { i32 }, ptr %25, i32 0, i32 0
  %27 = load i32, ptr %26, align 4
  %28 = call ptr @malgo_int32_t_to_string(i32 %27)
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, { ptr } }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %29, i32 0, i32 1, i32 0
  store ptr %28, ptr %31, align 8
  %32 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %35 = load ptr, ptr %34, align 8
  %36 = call ptr %35(ptr %33, ptr %29)
  ret ptr %36

switch_default_0:                                 ; preds = %switch_branch_SInt_0
  unreachable

switch_branch_SList_0:                            ; preds = %1
  %37 = getelementptr { i8, { ptr } }, ptr %_M12symbol_x247846test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal_0, i32 0, i32 1
  %38 = getelementptr { ptr }, ptr %37, i32 0, i32 0
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %41 = getelementptr { i8, { ptr } }, ptr %40, i32 0, i32 0
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { ptr } }, ptr %40, i32 0, i32 1, i32 0
  store ptr @str3849, ptr %42, align 8
  %43 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %40)
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 0
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr @str3850, ptr %46, align 8
  %47 = call ptr @_M9punctuate34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %44)
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %show_capture_0 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 0
  store ptr null, ptr %show_capture_0, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 1
  store ptr @_M4show46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External, ptr %show_func_0, align 8
  %49 = call ptr @_M7mapList34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %48)
  %50 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 0
  %51 = load ptr, ptr %50, align 8
  %52 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 1
  %53 = load ptr, ptr %52, align 8
  %54 = call ptr %53(ptr %51, ptr %39)
  %55 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = call ptr %58(ptr %56, ptr %54)
  %60 = call ptr @_M12concatString34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %59)
  %61 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %60)
  %62 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %63 = getelementptr { i8, { ptr } }, ptr %62, i32 0, i32 0
  store i8 0, ptr %63, align 1
  %64 = getelementptr { i8, { ptr } }, ptr %62, i32 0, i32 1, i32 0
  store ptr @str3851, ptr %64, align 8
  %65 = getelementptr { ptr, ptr }, ptr %61, i32 0, i32 0
  %66 = load ptr, ptr %65, align 8
  %67 = getelementptr { ptr, ptr }, ptr %61, i32 0, i32 1
  %68 = load ptr, ptr %67, align 8
  %69 = call ptr %68(ptr %66, ptr %62)
  %70 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 0
  %71 = load ptr, ptr %70, align 8
  %72 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 1
  %73 = load ptr, ptr %72, align 8
  %74 = call ptr %73(ptr %71, ptr %69)
  ret ptr %74

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M24raw_x5Flet_x24300_x24efe46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr %0, ptr %_M17_x5F_x2437_x242d534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %_M10nil_x242d634runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M10nil_x242d634runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %_M10nil_x242d634runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %switch_branch_Nil_1
    i8 1, label %switch_branch_Cons_1
  ]

switch_branch_Nil_1:                              ; preds = %switch_branch_Cons_0
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 0
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 1, i32 0
  store ptr %8, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 1, i32 1
  store ptr %13, ptr %18, align 8
  ret ptr %15

switch_branch_Cons_1:                             ; preds = %switch_branch_Cons_0
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %20 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr @_M9punctuate34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr null, ptr %_M17_x5F_x2437_x242d534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0)
  %25 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr %28(ptr %26, ptr %10)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, { ptr, ptr } }, ptr %30, i32 0, i32 0
  store i8 1, ptr %31, align 1
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %30, i32 0, i32 1, i32 0
  store ptr %_M17_x5F_x2437_x242d534runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, ptr %32, align 8
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %30, i32 0, i32 1, i32 1
  store ptr %29, ptr %33, align 8
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { ptr, ptr } }, ptr %34, i32 0, i32 0
  store i8 1, ptr %35, align 1
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %34, i32 0, i32 1, i32 0
  store ptr %8, ptr %36, align 8
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %34, i32 0, i32 1, i32 1
  store ptr %30, ptr %37, align 8
  ret ptr %34

switch_default_0:                                 ; preds = %switch_branch_Cons_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @_M35let_x24861_x24cb1_x5Fclosure_x24f0c46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal(ptr %0, ptr %1) {
  %"p$f89$f93$c94_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"p$f89$f93$c94_0" = load ptr, ptr %"p$f89$f93$c94_addr_0", align 8
  %3 = call ptr @malgo_string_append(ptr %"p$f89$f93$c94_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M24raw_x5Flet_x24f9a_x24efd46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Temporal(ptr %0, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_1"
  ]

"switch_branch_String#_1":                        ; preds = %"switch_branch_String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$861$cb1_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"p$f89$f93$c94_0" = getelementptr { ptr }, ptr %"let$861$cb1_capture_0", i32 0, i32 0
  store ptr %6, ptr %"p$f89$f93$c94_0", align 8
  %"let$861$cb1_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$861$cb1_capture_0", ptr %"let$861$cb1_capture_1", align 8
  %"let$861$cb1_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24861_x24cb1_x5Fclosure_x24f0c46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8Internal, ptr %"let$861$cb1_func_0", align 8
  %13 = call ptr @malgo_string_append(ptr %6, ptr %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Punctuate.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main46test_x2Ftestcases_x2Fmalgo_x2FPunctuate_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Punctuate.mlg"() {
  ret void
}
