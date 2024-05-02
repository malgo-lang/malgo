; ModuleID = 'test/testcases/malgo/ToplevelVariable.mlg'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.one" = global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.comp" = global ptr undef
@str300 = unnamed_addr constant [1 x i8] zeroinitializer
@str320 = unnamed_addr constant [10 x i8] c"no branch\00"
@str327 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal ptr @"runtime/malgo/Builtin.mlg.$addDouble_curry_4112"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_4113_0", ptr %"runtime/malgo/Builtin.mlg.$double#_4114_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_4113_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_4113_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_4114_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_4114_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"addDouble#_capture_0", align 8
  %"addDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addDouble#", ptr %"addDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call double %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, double %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"runtime/malgo/Builtin.mlg.$addDouble#_curry_4096"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_4097_0", double %"runtime/malgo/Builtin.mlg.$y_4098_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_add_double_capture_0, align 8
  %malgo_add_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_double", ptr %malgo_add_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_4097_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_4098_0")
  ret double %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$addFloat_curry_4080"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_4081_0", ptr %"runtime/malgo/Builtin.mlg.$float#_4082_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_4081_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_4081_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_4082_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_4082_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"addFloat#_capture_0", align 8
  %"addFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addFloat#", ptr %"addFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call float %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, float %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"runtime/malgo/Builtin.mlg.$addFloat#_curry_4064"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_4065_0", float %"runtime/malgo/Builtin.mlg.$y_4066_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_add_float_capture_0, align 8
  %malgo_add_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_float", ptr %malgo_add_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_4065_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call float %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_4066_0")
  ret float %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$addInt32_curry_4048"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4049_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_4050_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_0", align 8
  %"addInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32#", ptr %"addInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_add_int32_t_capture_0, align 8
  %malgo_add_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_4033_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_4034_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$addInt64_curry_4016"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_4017_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_4018_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_4017_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_4017_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_4018_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_4018_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"addInt64#_capture_0", align 8
  %"addInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt64#", ptr %"addInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i64 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$addInt64#_curry_4000"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_4001_0", i64 %"runtime/malgo/Builtin.mlg.$y_4002_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_add_int64_t_capture_0, align 8
  %malgo_add_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_add_int64_t", ptr %malgo_add_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_4001_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i64 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_4002_0")
  ret i64 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"appendString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"appendString#_capture_0", align 8
  %"appendString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString#", ptr %"appendString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, ptr %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$appendString#_curry_3968"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3969_0", ptr %"runtime/malgo/Builtin.mlg.$y_3970_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_string_append_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_string_append_capture_0, align 8
  %malgo_string_append_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_append", ptr %malgo_string_append_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_3969_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_3970_0")
  ret ptr %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$atString_curry_3952"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3953_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3954_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3953_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3953_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3954_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3954_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"atString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"atString#_capture_0", align 8
  %"atString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.atString#", ptr %"atString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i8 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Char#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Char#_capture_0", align 8
  %"Char#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Char#", ptr %"Char#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i8 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i8 @"runtime/malgo/Builtin.mlg.$atString#_curry_3936"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$i_3937_0", ptr %"runtime/malgo/Builtin.mlg.$x_3938_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_string_at_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_string_at_capture_0, align 8
  %malgo_string_at_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_at", ptr %malgo_string_at_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$i_3937_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i8 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$x_3938_0")
  ret i8 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$consString_curry_3918"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3919_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3920_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3919_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3919_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3920_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3920_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"consString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"consString#_capture_0", align 8
  %"consString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.consString#", ptr %"consString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, ptr %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$consString#_curry_3902"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$c_3903_0", ptr %"runtime/malgo/Builtin.mlg.$cs_3904_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_string_cons_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_string_cons_capture_0, align 8
  %malgo_string_cons_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_cons", ptr %malgo_string_cons_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$c_3903_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$cs_3904_0")
  ret ptr %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$divDouble_curry_3886"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3887_0", ptr %"runtime/malgo/Builtin.mlg.$double#_3888_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3887_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3887_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3888_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3888_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"divDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"divDouble#_capture_0", align 8
  %"divDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.divDouble#", ptr %"divDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call double %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, double %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"runtime/malgo/Builtin.mlg.$divDouble#_curry_3870"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3871_0", double %"runtime/malgo/Builtin.mlg.$y_3872_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_div_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_div_double_capture_0, align 8
  %malgo_div_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_div_double", ptr %malgo_div_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_3871_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_3872_0")
  ret double %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$divFloat_curry_3854"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3855_0", ptr %"runtime/malgo/Builtin.mlg.$float#_3856_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3855_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3855_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3856_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3856_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"divFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"divFloat#_capture_0", align 8
  %"divFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.divFloat#", ptr %"divFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call float %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, float %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"runtime/malgo/Builtin.mlg.$divFloat#_curry_3838"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3839_0", float %"runtime/malgo/Builtin.mlg.$y_3840_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_div_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_div_float_capture_0, align 8
  %malgo_div_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_div_float", ptr %malgo_div_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_3839_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call float %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_3840_0")
  ret float %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$divInt32_curry_3822"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3823_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_3824_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3823_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3823_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3824_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3824_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"divInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"divInt32#_capture_0", align 8
  %"divInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.divInt32#", ptr %"divInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$divInt32#_curry_3806"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3807_0", i32 %"runtime/malgo/Builtin.mlg.$y_3808_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_div_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_div_int32_t_capture_0, align 8
  %malgo_div_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_div_int32_t", ptr %malgo_div_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_3807_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_3808_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$divInt64_curry_3790"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3791_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3792_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3791_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3791_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3792_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3792_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"divInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"divInt64#_capture_0", align 8
  %"divInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.divInt64#", ptr %"divInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i64 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$divInt64#_curry_3774"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3775_0", i64 %"runtime/malgo/Builtin.mlg.$y_3776_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_div_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_div_int64_t_capture_0, align 8
  %malgo_div_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_div_int64_t", ptr %malgo_div_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_3775_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i64 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_3776_0")
  ret i64 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqChar_curry_3756"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3757_0", ptr %"runtime/malgo/Builtin.mlg.$char#_3758_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3757_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3757_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3758_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3758_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqChar#_capture_0", align 8
  %"eqChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqChar#", ptr %"eqChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqChar#_curry_3740"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3741_0", i8 %"runtime/malgo/Builtin.mlg.$y_3742_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_char_capture_0, align 8
  %malgo_eq_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_char", ptr %malgo_eq_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3741_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_3742_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqDouble_curry_3724"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3725_0", ptr %"runtime/malgo/Builtin.mlg.$double#_3726_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3725_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3725_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3726_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3726_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqDouble#_capture_0", align 8
  %"eqDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqDouble#", ptr %"eqDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqDouble#_curry_3708"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3709_0", double %"runtime/malgo/Builtin.mlg.$y_3710_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_double_capture_0, align 8
  %malgo_eq_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_double", ptr %malgo_eq_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_3709_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_3710_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqFloat_curry_3692"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3693_0", ptr %"runtime/malgo/Builtin.mlg.$float#_3694_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3693_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3693_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3694_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3694_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqFloat#_capture_0", align 8
  %"eqFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqFloat#", ptr %"eqFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqFloat#_curry_3676"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3677_0", float %"runtime/malgo/Builtin.mlg.$y_3678_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_float_capture_0, align 8
  %malgo_eq_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_float", ptr %malgo_eq_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_3677_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_3678_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqInt32_curry_3660"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3661_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_3662_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3661_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3661_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3662_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3662_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqInt32#_capture_0", align 8
  %"eqInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqInt32#", ptr %"eqInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqInt32#_curry_3644"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3645_0", i32 %"runtime/malgo/Builtin.mlg.$y_3646_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_int32_t_capture_0, align 8
  %malgo_eq_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_int32_t", ptr %malgo_eq_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_3645_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_3646_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqInt64_curry_3628"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3629_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3630_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqInt64#_capture_0", align 8
  %"eqInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqInt64#", ptr %"eqInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqInt64#_curry_3612"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3613_0", i64 %"runtime/malgo/Builtin.mlg.$y_3614_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_int64_t_capture_0, align 8
  %malgo_eq_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_int64_t", ptr %malgo_eq_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_3613_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_3614_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$eqString_curry_3596"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3597_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3598_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3597_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3597_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3598_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3598_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"eqString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"eqString#_capture_0", align 8
  %"eqString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqString#", ptr %"eqString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$eqString#_curry_3580"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3581_0", ptr %"runtime/malgo/Builtin.mlg.$y_3582_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_eq_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_eq_string_capture_0, align 8
  %malgo_eq_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_eq_string", ptr %malgo_eq_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_3581_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_3582_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geChar_curry_3554"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3555_0", ptr %"runtime/malgo/Builtin.mlg.$char#_3556_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3555_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3555_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3556_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3556_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geChar#_capture_0", align 8
  %"geChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geChar#", ptr %"geChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geChar#_curry_3538"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3539_0", i8 %"runtime/malgo/Builtin.mlg.$y_3540_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_char_capture_0, align 8
  %malgo_ge_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_char", ptr %malgo_ge_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3539_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_3540_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geDouble_curry_3522"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3523_0", ptr %"runtime/malgo/Builtin.mlg.$double#_3524_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3523_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3523_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3524_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3524_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geDouble#_capture_0", align 8
  %"geDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geDouble#", ptr %"geDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geDouble#_curry_3506"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3507_0", double %"runtime/malgo/Builtin.mlg.$y_3508_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_double_capture_0, align 8
  %malgo_ge_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_double", ptr %malgo_ge_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_3507_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_3508_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geFloat_curry_3490"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3491_0", ptr %"runtime/malgo/Builtin.mlg.$float#_3492_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3491_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3491_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3492_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3492_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geFloat#_capture_0", align 8
  %"geFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geFloat#", ptr %"geFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geFloat#_curry_3474"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3475_0", float %"runtime/malgo/Builtin.mlg.$y_3476_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_float_capture_0, align 8
  %malgo_ge_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_float", ptr %malgo_ge_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_3475_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_3476_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geInt32_curry_3458"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3459_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_3460_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3459_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3459_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3460_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3460_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geInt32#_capture_0", align 8
  %"geInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geInt32#", ptr %"geInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geInt32#_curry_3442"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3443_0", i32 %"runtime/malgo/Builtin.mlg.$y_3444_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_int32_t_capture_0, align 8
  %malgo_ge_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_int32_t", ptr %malgo_ge_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_3443_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_3444_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geInt64_curry_3426"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3427_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3428_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3427_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3427_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3428_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3428_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geInt64#_capture_0", align 8
  %"geInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geInt64#", ptr %"geInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geInt64#_curry_3410"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3411_0", i64 %"runtime/malgo/Builtin.mlg.$y_3412_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_int64_t_capture_0, align 8
  %malgo_ge_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_int64_t", ptr %malgo_ge_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_3411_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_3412_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$geString_curry_3394"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3395_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3396_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3395_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3395_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3396_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3396_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"geString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"geString#_capture_0", align 8
  %"geString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.geString#", ptr %"geString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$geString#_curry_3378"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3379_0", ptr %"runtime/malgo/Builtin.mlg.$y_3380_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ge_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ge_string_capture_0, align 8
  %malgo_ge_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ge_string", ptr %malgo_ge_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_3379_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_3380_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtChar_curry_3350"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3351_0", ptr %"runtime/malgo/Builtin.mlg.$char#_3352_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3351_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3351_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3352_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3352_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtChar#_capture_0", align 8
  %"gtChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtChar#", ptr %"gtChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtChar#_curry_3334"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3335_0", i8 %"runtime/malgo/Builtin.mlg.$y_3336_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_char_capture_0, align 8
  %malgo_gt_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_char", ptr %malgo_gt_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3335_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_3336_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtDouble_curry_3318"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3319_0", ptr %"runtime/malgo/Builtin.mlg.$double#_3320_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3319_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3319_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3320_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3320_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtDouble#_capture_0", align 8
  %"gtDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtDouble#", ptr %"gtDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtDouble#_curry_3302"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3303_0", double %"runtime/malgo/Builtin.mlg.$y_3304_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_double_capture_0, align 8
  %malgo_gt_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_double", ptr %malgo_gt_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_3303_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_3304_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtFloat_curry_3286"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3287_0", ptr %"runtime/malgo/Builtin.mlg.$float#_3288_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3287_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3287_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3288_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3288_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtFloat#_capture_0", align 8
  %"gtFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtFloat#", ptr %"gtFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtFloat#_curry_3270"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3271_0", float %"runtime/malgo/Builtin.mlg.$y_3272_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_float_capture_0, align 8
  %malgo_gt_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_float", ptr %malgo_gt_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_3271_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_3272_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtInt32_curry_3254"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3255_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_3256_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3255_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3255_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3256_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3256_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtInt32#_capture_0", align 8
  %"gtInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtInt32#", ptr %"gtInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtInt32#_curry_3238"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3239_0", i32 %"runtime/malgo/Builtin.mlg.$y_3240_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_int32_t_capture_0, align 8
  %malgo_gt_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_int32_t", ptr %malgo_gt_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_3239_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_3240_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtInt64_curry_3222"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3223_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3224_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3223_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3223_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3224_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3224_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtInt64#_capture_0", align 8
  %"gtInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtInt64#", ptr %"gtInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtInt64#_curry_3206"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3207_0", i64 %"runtime/malgo/Builtin.mlg.$y_3208_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_int64_t_capture_0, align 8
  %malgo_gt_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_int64_t", ptr %malgo_gt_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_3207_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_3208_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$gtString_curry_3190"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3191_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3192_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3191_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3191_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3192_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3192_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"gtString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"gtString#_capture_0", align 8
  %"gtString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.gtString#", ptr %"gtString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$gtString#_curry_3174"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3175_0", ptr %"runtime/malgo/Builtin.mlg.$y_3176_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_gt_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_gt_string_capture_0, align 8
  %malgo_gt_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_gt_string", ptr %malgo_gt_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_3175_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_3176_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neString_curry_3133"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3134_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3135_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3134_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3134_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3135_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3135_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neString#_capture_0", align 8
  %"neString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neString#", ptr %"neString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neInt64_curry_3113"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3114_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_3115_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3114_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3114_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3115_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_3115_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neInt64#_capture_0", align 8
  %"neInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neInt64#", ptr %"neInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neInt32_curry_3093"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3094_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_3095_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3094_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3094_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3095_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_3095_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neInt32#_capture_0", align 8
  %"neInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neInt32#", ptr %"neInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neFloat_curry_3073"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3074_0", ptr %"runtime/malgo/Builtin.mlg.$float#_3075_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3074_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3074_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_3075_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_3075_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neFloat#_capture_0", align 8
  %"neFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neFloat#", ptr %"neFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neDouble_curry_3053"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3054_0", ptr %"runtime/malgo/Builtin.mlg.$double#_3055_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3054_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3054_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_3055_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_3055_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neDouble#_capture_0", align 8
  %"neDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neDouble#", ptr %"neDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$neChar_curry_3033"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3034_0", ptr %"runtime/malgo/Builtin.mlg.$char#_3035_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3034_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3034_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3035_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"neChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"neChar#_capture_0", align 8
  %"neChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.neChar#", ptr %"neChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltString_curry_3013"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3014_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3015_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3014_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3014_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3015_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3015_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltString#_capture_0", align 8
  %"ltString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltString#", ptr %"ltString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltInt64_curry_2993"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2994_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2995_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2994_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2994_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2995_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2995_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltInt64#_capture_0", align 8
  %"ltInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltInt64#", ptr %"ltInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltInt32_curry_2973"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2974_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2975_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2974_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2974_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2975_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2975_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltInt32#_capture_0", align 8
  %"ltInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltInt32#", ptr %"ltInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltFloat_curry_2953"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2954_0", ptr %"runtime/malgo/Builtin.mlg.$float#_2955_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2954_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2954_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2955_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2955_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltFloat#_capture_0", align 8
  %"ltFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltFloat#", ptr %"ltFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltDouble_curry_2933"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2934_0", ptr %"runtime/malgo/Builtin.mlg.$double#_2935_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2934_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2934_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2935_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2935_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltDouble#_capture_0", align 8
  %"ltDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltDouble#", ptr %"ltDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$ltChar_curry_2913"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2914_0", ptr %"runtime/malgo/Builtin.mlg.$char#_2915_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2914_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2914_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2915_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2915_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ltChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"ltChar#_capture_0", align 8
  %"ltChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ltChar#", ptr %"ltChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leString_curry_2893"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2894_0", ptr %"runtime/malgo/Builtin.mlg.$string#_2895_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2894_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2894_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2895_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2895_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leString#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leString#_capture_0", align 8
  %"leString#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leString#", ptr %"leString#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, ptr %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leInt64_curry_2873"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2874_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2875_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2874_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2874_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2875_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2875_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leInt64#_capture_0", align 8
  %"leInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt64#", ptr %"leInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leInt32_curry_2853"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2854_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2855_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leInt32#_capture_0", align 8
  %"leInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leInt32#", ptr %"leInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leFloat_curry_2833"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2834_0", ptr %"runtime/malgo/Builtin.mlg.$float#_2835_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2834_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2834_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2835_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2835_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leFloat#_capture_0", align 8
  %"leFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leFloat#", ptr %"leFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leDouble_curry_2813"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2814_0", ptr %"runtime/malgo/Builtin.mlg.$double#_2815_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2814_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2814_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2815_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2815_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leDouble#_capture_0", align 8
  %"leDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leDouble#", ptr %"leDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$leChar_curry_2793"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2794_0", ptr %"runtime/malgo/Builtin.mlg.$char#_2795_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2794_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2794_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2795_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2795_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"leChar#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"leChar#_capture_0", align 8
  %"leChar#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.leChar#", ptr %"leChar#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i8 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i8 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leChar#_curry_2768"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2769_0", i8 %"runtime/malgo/Builtin.mlg.$y_2770_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_char_capture_0, align 8
  %malgo_le_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_char", ptr %malgo_le_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2769_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_2770_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leDouble#_curry_2756"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2757_0", double %"runtime/malgo/Builtin.mlg.$y_2758_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_double_capture_0, align 8
  %malgo_le_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_double", ptr %malgo_le_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2757_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_2758_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leFloat#_curry_2744"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2745_0", float %"runtime/malgo/Builtin.mlg.$y_2746_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_float_capture_0, align 8
  %malgo_le_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_float", ptr %malgo_le_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2745_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_2746_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leInt32#_curry_2732"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2733_0", i32 %"runtime/malgo/Builtin.mlg.$y_2734_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_int32_t_capture_0, align 8
  %malgo_le_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_int32_t", ptr %malgo_le_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2733_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_2734_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leInt64#_curry_2720"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2721_0", i64 %"runtime/malgo/Builtin.mlg.$y_2722_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_int64_t_capture_0, align 8
  %malgo_le_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_int64_t", ptr %malgo_le_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2721_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_2722_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$leString#_curry_2708"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2709_0", ptr %"runtime/malgo/Builtin.mlg.$y_2710_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_le_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_le_string_capture_0, align 8
  %malgo_le_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_le_string", ptr %malgo_le_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2709_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_2710_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltChar#_curry_2689"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2690_0", i8 %"runtime/malgo/Builtin.mlg.$y_2691_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_char_capture_0, align 8
  %malgo_lt_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_char", ptr %malgo_lt_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2690_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_2691_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltDouble#_curry_2677"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2678_0", double %"runtime/malgo/Builtin.mlg.$y_2679_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_double_capture_0, align 8
  %malgo_lt_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_double", ptr %malgo_lt_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2678_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_2679_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltFloat#_curry_2665"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2666_0", float %"runtime/malgo/Builtin.mlg.$y_2667_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_float_capture_0, align 8
  %malgo_lt_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_float", ptr %malgo_lt_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2666_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_2667_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltInt32#_curry_2653"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2654_0", i32 %"runtime/malgo/Builtin.mlg.$y_2655_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_int32_t_capture_0, align 8
  %malgo_lt_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_int32_t", ptr %malgo_lt_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2654_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_2655_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltInt64#_curry_2641"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2642_0", i64 %"runtime/malgo/Builtin.mlg.$y_2643_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_int64_t_capture_0, align 8
  %malgo_lt_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_int64_t", ptr %malgo_lt_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2642_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_2643_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$ltString#_curry_2629"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2630_0", ptr %"runtime/malgo/Builtin.mlg.$y_2631_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_lt_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_lt_string_capture_0, align 8
  %malgo_lt_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_lt_string", ptr %malgo_lt_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2630_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_2631_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$mulDouble_curry_2613"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2614_0", ptr %"runtime/malgo/Builtin.mlg.$double#_2615_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2614_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2614_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2615_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2615_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"mulDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"mulDouble#_capture_0", align 8
  %"mulDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulDouble#", ptr %"mulDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call double %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, double %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"runtime/malgo/Builtin.mlg.$mulDouble#_curry_2597"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2598_0", double %"runtime/malgo/Builtin.mlg.$y_2599_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_mul_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_mul_double_capture_0, align 8
  %malgo_mul_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_mul_double", ptr %malgo_mul_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2598_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_2599_0")
  ret double %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$mulFloat_curry_2581"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2582_0", ptr %"runtime/malgo/Builtin.mlg.$float#_2583_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2582_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2582_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2583_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2583_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"mulFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"mulFloat#_capture_0", align 8
  %"mulFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulFloat#", ptr %"mulFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call float %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, float %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"runtime/malgo/Builtin.mlg.$mulFloat#_curry_2565"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2566_0", float %"runtime/malgo/Builtin.mlg.$y_2567_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_mul_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_mul_float_capture_0, align 8
  %malgo_mul_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_mul_float", ptr %malgo_mul_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2566_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call float %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_2567_0")
  ret float %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$mulInt32_curry_2549"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2550_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2551_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2550_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2550_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2551_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2551_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"mulInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"mulInt32#_capture_0", align 8
  %"mulInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulInt32#", ptr %"mulInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$mulInt32#_curry_2533"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2534_0", i32 %"runtime/malgo/Builtin.mlg.$y_2535_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_mul_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_mul_int32_t_capture_0, align 8
  %malgo_mul_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_mul_int32_t", ptr %malgo_mul_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2534_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_2535_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$mulInt64_curry_2517"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2518_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2519_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"mulInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"mulInt64#_capture_0", align 8
  %"mulInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulInt64#", ptr %"mulInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i64 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$mulInt64#_curry_2501"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2502_0", i64 %"runtime/malgo/Builtin.mlg.$y_2503_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_mul_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_mul_int64_t_capture_0, align 8
  %malgo_mul_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_mul_int64_t", ptr %malgo_mul_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2502_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i64 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_2503_0")
  ret i64 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neChar#_curry_2489"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2490_0", i8 %"runtime/malgo/Builtin.mlg.$y_2491_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_char_capture_0, align 8
  %malgo_ne_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_char", ptr %malgo_ne_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2490_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %"runtime/malgo/Builtin.mlg.$y_2491_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neDouble#_curry_2477"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2478_0", double %"runtime/malgo/Builtin.mlg.$y_2479_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_double_capture_0, align 8
  %malgo_ne_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_double", ptr %malgo_ne_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2478_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_2479_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neFloat#_curry_2465"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2466_0", float %"runtime/malgo/Builtin.mlg.$y_2467_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_float_capture_0, align 8
  %malgo_ne_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_float", ptr %malgo_ne_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2466_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_2467_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neInt32#_curry_2453"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2454_0", i32 %"runtime/malgo/Builtin.mlg.$y_2455_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_int32_t_capture_0, align 8
  %malgo_ne_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_int32_t", ptr %malgo_ne_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2454_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_2455_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neInt64#_curry_2441"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2442_0", i64 %"runtime/malgo/Builtin.mlg.$y_2443_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_int64_t_capture_0, align 8
  %malgo_ne_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_int64_t", ptr %malgo_ne_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2442_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_2443_0")
  ret i32 %12
}

define internal i32 @"runtime/malgo/Builtin.mlg.$neString#_curry_2429"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2430_0", ptr %"runtime/malgo/Builtin.mlg.$y_2431_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_ne_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_ne_string_capture_0, align 8
  %malgo_ne_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_ne_string", ptr %malgo_ne_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2430_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, ptr %"runtime/malgo/Builtin.mlg.$y_2431_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subDouble_curry_2372"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2373_0", ptr %"runtime/malgo/Builtin.mlg.$double#_2374_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2373_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2373_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2374_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2374_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"subDouble#_capture_0", align 8
  %"subDouble#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subDouble#", ptr %"subDouble#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, double %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call double %21(ptr %19, double %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, double %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"runtime/malgo/Builtin.mlg.$subDouble#_curry_2356"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2357_0", double %"runtime/malgo/Builtin.mlg.$y_2358_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_double_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_sub_double_capture_0, align 8
  %malgo_sub_double_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_double", ptr %malgo_sub_double_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2357_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %"runtime/malgo/Builtin.mlg.$y_2358_0")
  ret double %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subFloat_curry_2340"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2341_0", ptr %"runtime/malgo/Builtin.mlg.$float#_2342_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2341_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2341_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2342_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2342_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"subFloat#_capture_0", align 8
  %"subFloat#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subFloat#", ptr %"subFloat#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, float %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call float %21(ptr %19, float %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, float %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"runtime/malgo/Builtin.mlg.$subFloat#_curry_2324"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2325_0", float %"runtime/malgo/Builtin.mlg.$y_2326_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_float_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_sub_float_capture_0, align 8
  %malgo_sub_float_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_float", ptr %malgo_sub_float_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2325_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call float %11(ptr %9, float %"runtime/malgo/Builtin.mlg.$y_2326_0")
  ret float %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"subInt32#_capture_0", align 8
  %"subInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt32#", ptr %"subInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2293_0", i32 %"runtime/malgo/Builtin.mlg.$y_2294_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_sub_int32_t_capture_0, align 8
  %malgo_sub_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t", ptr %malgo_sub_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2293_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"runtime/malgo/Builtin.mlg.$y_2294_0")
  ret i32 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subInt64_curry_2276"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2277_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2278_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"subInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"subInt64#_capture_0", align 8
  %"subInt64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt64#", ptr %"subInt64#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i64 %21(ptr %19, i64 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i64 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"runtime/malgo/Builtin.mlg.$subInt64#_curry_2260"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2261_0", i64 %"runtime/malgo/Builtin.mlg.$y_2262_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_int64_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_sub_int64_t_capture_0, align 8
  %malgo_sub_int64_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int64_t", ptr %malgo_sub_int64_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2261_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i64 %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$y_2262_0")
  ret i64 %12
}

define internal ptr @"runtime/malgo/Builtin.mlg.$substring_curry_2239"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2240_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2241_0", ptr %"runtime/malgo/Builtin.mlg.$int64#_2242_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2240_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2240_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2241_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2241_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2242_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  %14 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2242_0", i32 0, i32 1
  %15 = getelementptr { i64 }, ptr %14, i32 0, i32 0
  %16 = load i64, ptr %15, align 4
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"substring#_capture_0" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr null, ptr %"substring#_capture_0", align 8
  %"substring#_func_0" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.substring#", ptr %"substring#_func_0", align 8
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %6)
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i64 %11)
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, i64 %16)
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %34 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr %37(ptr %35, ptr %32)
  ret ptr %38

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$substring#_curry_2215"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$str_2216_0", i64 %"runtime/malgo/Builtin.mlg.$start_2217_0", i64 %"runtime/malgo/Builtin.mlg.$end_2218_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_substring_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_substring_capture_0, align 8
  %malgo_substring_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_substring", ptr %malgo_substring_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$str_2216_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i64 %"runtime/malgo/Builtin.mlg.$start_2217_0")
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i64 %"runtime/malgo/Builtin.mlg.$end_2218_0")
  ret ptr %17
}

define internal ptr @"runtime/malgo/Builtin.mlg.$malgo_substring_curry_2149"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2150_0", i64 %"runtime/malgo/Builtin.mlg.$p_2151_0", i64 %"runtime/malgo/Builtin.mlg.$p_2152_0") {
  %2 = call ptr @malgo_substring(ptr %"runtime/malgo/Builtin.mlg.$p_2150_0", i64 %"runtime/malgo/Builtin.mlg.$p_2151_0", i64 %"runtime/malgo/Builtin.mlg.$p_2152_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.$malgo_string_append_curry_2142"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2143_0", ptr %"runtime/malgo/Builtin.mlg.$p_2144_0") {
  %2 = call ptr @malgo_string_append(ptr %"runtime/malgo/Builtin.mlg.$p_2143_0", ptr %"runtime/malgo/Builtin.mlg.$p_2144_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.$malgo_string_cons_curry_2136"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2137_0", ptr %"runtime/malgo/Builtin.mlg.$p_2138_0") {
  %2 = call ptr @malgo_string_cons(i8 %"runtime/malgo/Builtin.mlg.$p_2137_0", ptr %"runtime/malgo/Builtin.mlg.$p_2138_0")
  ret ptr %2
}

define internal i8 @"runtime/malgo/Builtin.mlg.$malgo_string_at_curry_2130"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_2131_0", ptr %"runtime/malgo/Builtin.mlg.$p_2132_0") {
  %2 = call i8 @malgo_string_at(i64 %"runtime/malgo/Builtin.mlg.$p_2131_0", ptr %"runtime/malgo/Builtin.mlg.$p_2132_0")
  ret i8 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_string_curry_2118"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2119_0", ptr %"runtime/malgo/Builtin.mlg.$p_2120_0") {
  %2 = call i32 @malgo_ge_string(ptr %"runtime/malgo/Builtin.mlg.$p_2119_0", ptr %"runtime/malgo/Builtin.mlg.$p_2120_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_string_curry_2112"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2113_0", ptr %"runtime/malgo/Builtin.mlg.$p_2114_0") {
  %2 = call i32 @malgo_le_string(ptr %"runtime/malgo/Builtin.mlg.$p_2113_0", ptr %"runtime/malgo/Builtin.mlg.$p_2114_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_string_curry_2106"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2107_0", ptr %"runtime/malgo/Builtin.mlg.$p_2108_0") {
  %2 = call i32 @malgo_gt_string(ptr %"runtime/malgo/Builtin.mlg.$p_2107_0", ptr %"runtime/malgo/Builtin.mlg.$p_2108_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_string_curry_2100"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2101_0", ptr %"runtime/malgo/Builtin.mlg.$p_2102_0") {
  %2 = call i32 @malgo_lt_string(ptr %"runtime/malgo/Builtin.mlg.$p_2101_0", ptr %"runtime/malgo/Builtin.mlg.$p_2102_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_string_curry_2094"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2095_0", ptr %"runtime/malgo/Builtin.mlg.$p_2096_0") {
  %2 = call i32 @malgo_ne_string(ptr %"runtime/malgo/Builtin.mlg.$p_2095_0", ptr %"runtime/malgo/Builtin.mlg.$p_2096_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_string_curry_2088"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2089_0", ptr %"runtime/malgo/Builtin.mlg.$p_2090_0") {
  %2 = call i32 @malgo_eq_string(ptr %"runtime/malgo/Builtin.mlg.$p_2089_0", ptr %"runtime/malgo/Builtin.mlg.$p_2090_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_char_curry_2082"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2083_0", i8 %"runtime/malgo/Builtin.mlg.$p_2084_0") {
  %2 = call i32 @malgo_ge_char(i8 %"runtime/malgo/Builtin.mlg.$p_2083_0", i8 %"runtime/malgo/Builtin.mlg.$p_2084_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_char_curry_2076"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2077_0", i8 %"runtime/malgo/Builtin.mlg.$p_2078_0") {
  %2 = call i32 @malgo_le_char(i8 %"runtime/malgo/Builtin.mlg.$p_2077_0", i8 %"runtime/malgo/Builtin.mlg.$p_2078_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_char_curry_2070"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2071_0", i8 %"runtime/malgo/Builtin.mlg.$p_2072_0") {
  %2 = call i32 @malgo_gt_char(i8 %"runtime/malgo/Builtin.mlg.$p_2071_0", i8 %"runtime/malgo/Builtin.mlg.$p_2072_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_char_curry_2064"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2065_0", i8 %"runtime/malgo/Builtin.mlg.$p_2066_0") {
  %2 = call i32 @malgo_lt_char(i8 %"runtime/malgo/Builtin.mlg.$p_2065_0", i8 %"runtime/malgo/Builtin.mlg.$p_2066_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_char_curry_2058"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2059_0", i8 %"runtime/malgo/Builtin.mlg.$p_2060_0") {
  %2 = call i32 @malgo_ne_char(i8 %"runtime/malgo/Builtin.mlg.$p_2059_0", i8 %"runtime/malgo/Builtin.mlg.$p_2060_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_char_curry_2052"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2053_0", i8 %"runtime/malgo/Builtin.mlg.$p_2054_0") {
  %2 = call i32 @malgo_eq_char(i8 %"runtime/malgo/Builtin.mlg.$p_2053_0", i8 %"runtime/malgo/Builtin.mlg.$p_2054_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_double_curry_2046"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2047_0", double %"runtime/malgo/Builtin.mlg.$p_2048_0") {
  %2 = call i32 @malgo_ge_double(double %"runtime/malgo/Builtin.mlg.$p_2047_0", double %"runtime/malgo/Builtin.mlg.$p_2048_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_double_curry_2040"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2041_0", double %"runtime/malgo/Builtin.mlg.$p_2042_0") {
  %2 = call i32 @malgo_le_double(double %"runtime/malgo/Builtin.mlg.$p_2041_0", double %"runtime/malgo/Builtin.mlg.$p_2042_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_double_curry_2034"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2035_0", double %"runtime/malgo/Builtin.mlg.$p_2036_0") {
  %2 = call i32 @malgo_gt_double(double %"runtime/malgo/Builtin.mlg.$p_2035_0", double %"runtime/malgo/Builtin.mlg.$p_2036_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_double_curry_2028"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2029_0", double %"runtime/malgo/Builtin.mlg.$p_2030_0") {
  %2 = call i32 @malgo_lt_double(double %"runtime/malgo/Builtin.mlg.$p_2029_0", double %"runtime/malgo/Builtin.mlg.$p_2030_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_double_curry_2022"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2023_0", double %"runtime/malgo/Builtin.mlg.$p_2024_0") {
  %2 = call i32 @malgo_ne_double(double %"runtime/malgo/Builtin.mlg.$p_2023_0", double %"runtime/malgo/Builtin.mlg.$p_2024_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_double_curry_2016"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2017_0", double %"runtime/malgo/Builtin.mlg.$p_2018_0") {
  %2 = call i32 @malgo_eq_double(double %"runtime/malgo/Builtin.mlg.$p_2017_0", double %"runtime/malgo/Builtin.mlg.$p_2018_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_float_curry_2010"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_2011_0", float %"runtime/malgo/Builtin.mlg.$p_2012_0") {
  %2 = call i32 @malgo_ge_float(float %"runtime/malgo/Builtin.mlg.$p_2011_0", float %"runtime/malgo/Builtin.mlg.$p_2012_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_float_curry_2004"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_2005_0", float %"runtime/malgo/Builtin.mlg.$p_2006_0") {
  %2 = call i32 @malgo_le_float(float %"runtime/malgo/Builtin.mlg.$p_2005_0", float %"runtime/malgo/Builtin.mlg.$p_2006_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_float_curry_1998"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1999_0", float %"runtime/malgo/Builtin.mlg.$p_2000_0") {
  %2 = call i32 @malgo_gt_float(float %"runtime/malgo/Builtin.mlg.$p_1999_0", float %"runtime/malgo/Builtin.mlg.$p_2000_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_float_curry_1992"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1993_0", float %"runtime/malgo/Builtin.mlg.$p_1994_0") {
  %2 = call i32 @malgo_lt_float(float %"runtime/malgo/Builtin.mlg.$p_1993_0", float %"runtime/malgo/Builtin.mlg.$p_1994_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_float_curry_1986"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1987_0", float %"runtime/malgo/Builtin.mlg.$p_1988_0") {
  %2 = call i32 @malgo_ne_float(float %"runtime/malgo/Builtin.mlg.$p_1987_0", float %"runtime/malgo/Builtin.mlg.$p_1988_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_float_curry_1980"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1981_0", float %"runtime/malgo/Builtin.mlg.$p_1982_0") {
  %2 = call i32 @malgo_eq_float(float %"runtime/malgo/Builtin.mlg.$p_1981_0", float %"runtime/malgo/Builtin.mlg.$p_1982_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_int64_t_curry_1974"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1975_0", i64 %"runtime/malgo/Builtin.mlg.$p_1976_0") {
  %2 = call i32 @malgo_ge_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1975_0", i64 %"runtime/malgo/Builtin.mlg.$p_1976_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int64_t_curry_1968"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1969_0", i64 %"runtime/malgo/Builtin.mlg.$p_1970_0") {
  %2 = call i32 @malgo_le_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1969_0", i64 %"runtime/malgo/Builtin.mlg.$p_1970_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_int64_t_curry_1962"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1963_0", i64 %"runtime/malgo/Builtin.mlg.$p_1964_0") {
  %2 = call i32 @malgo_gt_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1963_0", i64 %"runtime/malgo/Builtin.mlg.$p_1964_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_int64_t_curry_1956"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1957_0", i64 %"runtime/malgo/Builtin.mlg.$p_1958_0") {
  %2 = call i32 @malgo_lt_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1957_0", i64 %"runtime/malgo/Builtin.mlg.$p_1958_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_int64_t_curry_1950"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1951_0", i64 %"runtime/malgo/Builtin.mlg.$p_1952_0") {
  %2 = call i32 @malgo_ne_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1951_0", i64 %"runtime/malgo/Builtin.mlg.$p_1952_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int64_t_curry_1944"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1945_0", i64 %"runtime/malgo/Builtin.mlg.$p_1946_0") {
  %2 = call i32 @malgo_eq_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1945_0", i64 %"runtime/malgo/Builtin.mlg.$p_1946_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_int32_t_curry_1938"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1939_0", i32 %"runtime/malgo/Builtin.mlg.$p_1940_0") {
  %2 = call i32 @malgo_ge_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1939_0", i32 %"runtime/malgo/Builtin.mlg.$p_1940_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int32_t_curry_1932"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1933_0", i32 %"runtime/malgo/Builtin.mlg.$p_1934_0") {
  %2 = call i32 @malgo_le_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1933_0", i32 %"runtime/malgo/Builtin.mlg.$p_1934_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_int32_t_curry_1926"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1927_0", i32 %"runtime/malgo/Builtin.mlg.$p_1928_0") {
  %2 = call i32 @malgo_gt_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1927_0", i32 %"runtime/malgo/Builtin.mlg.$p_1928_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_int32_t_curry_1920"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1921_0", i32 %"runtime/malgo/Builtin.mlg.$p_1922_0") {
  %2 = call i32 @malgo_lt_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1921_0", i32 %"runtime/malgo/Builtin.mlg.$p_1922_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_int32_t_curry_1914"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1915_0", i32 %"runtime/malgo/Builtin.mlg.$p_1916_0") {
  %2 = call i32 @malgo_ne_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1915_0", i32 %"runtime/malgo/Builtin.mlg.$p_1916_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int32_t_curry_1908"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1909_0", i32 %"runtime/malgo/Builtin.mlg.$p_1910_0") {
  %2 = call i32 @malgo_eq_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1909_0", i32 %"runtime/malgo/Builtin.mlg.$p_1910_0")
  ret i32 %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_div_double_curry_1900"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1901_0", double %"runtime/malgo/Builtin.mlg.$p_1902_0") {
  %2 = call double @malgo_div_double(double %"runtime/malgo/Builtin.mlg.$p_1901_0", double %"runtime/malgo/Builtin.mlg.$p_1902_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_mul_double_curry_1894"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1895_0", double %"runtime/malgo/Builtin.mlg.$p_1896_0") {
  %2 = call double @malgo_mul_double(double %"runtime/malgo/Builtin.mlg.$p_1895_0", double %"runtime/malgo/Builtin.mlg.$p_1896_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_sub_double_curry_1888"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1889_0", double %"runtime/malgo/Builtin.mlg.$p_1890_0") {
  %2 = call double @malgo_sub_double(double %"runtime/malgo/Builtin.mlg.$p_1889_0", double %"runtime/malgo/Builtin.mlg.$p_1890_0")
  ret double %2
}

define internal double @"runtime/malgo/Builtin.mlg.$malgo_add_double_curry_1882"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1883_0", double %"runtime/malgo/Builtin.mlg.$p_1884_0") {
  %2 = call double @malgo_add_double(double %"runtime/malgo/Builtin.mlg.$p_1883_0", double %"runtime/malgo/Builtin.mlg.$p_1884_0")
  ret double %2
}

define internal float @"runtime/malgo/Builtin.mlg.$malgo_div_float_curry_1876"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1877_0", float %"runtime/malgo/Builtin.mlg.$p_1878_0") {
  %2 = call float @malgo_div_float(float %"runtime/malgo/Builtin.mlg.$p_1877_0", float %"runtime/malgo/Builtin.mlg.$p_1878_0")
  ret float %2
}

define internal float @"runtime/malgo/Builtin.mlg.$malgo_mul_float_curry_1870"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1871_0", float %"runtime/malgo/Builtin.mlg.$p_1872_0") {
  %2 = call float @malgo_mul_float(float %"runtime/malgo/Builtin.mlg.$p_1871_0", float %"runtime/malgo/Builtin.mlg.$p_1872_0")
  ret float %2
}

define internal float @"runtime/malgo/Builtin.mlg.$malgo_sub_float_curry_1864"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1865_0", float %"runtime/malgo/Builtin.mlg.$p_1866_0") {
  %2 = call float @malgo_sub_float(float %"runtime/malgo/Builtin.mlg.$p_1865_0", float %"runtime/malgo/Builtin.mlg.$p_1866_0")
  ret float %2
}

define internal float @"runtime/malgo/Builtin.mlg.$malgo_add_float_curry_1858"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1859_0", float %"runtime/malgo/Builtin.mlg.$p_1860_0") {
  %2 = call float @malgo_add_float(float %"runtime/malgo/Builtin.mlg.$p_1859_0", float %"runtime/malgo/Builtin.mlg.$p_1860_0")
  ret float %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_div_int64_t_curry_1852"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1853_0", i64 %"runtime/malgo/Builtin.mlg.$p_1854_0") {
  %2 = call i64 @malgo_div_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1853_0", i64 %"runtime/malgo/Builtin.mlg.$p_1854_0")
  ret i64 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_mul_int64_t_curry_1846"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1847_0", i64 %"runtime/malgo/Builtin.mlg.$p_1848_0") {
  %2 = call i64 @malgo_mul_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1847_0", i64 %"runtime/malgo/Builtin.mlg.$p_1848_0")
  ret i64 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_sub_int64_t_curry_1840"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1841_0", i64 %"runtime/malgo/Builtin.mlg.$p_1842_0") {
  %2 = call i64 @malgo_sub_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1841_0", i64 %"runtime/malgo/Builtin.mlg.$p_1842_0")
  ret i64 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.$malgo_add_int64_t_curry_1834"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1835_0", i64 %"runtime/malgo/Builtin.mlg.$p_1836_0") {
  %2 = call i64 @malgo_add_int64_t(i64 %"runtime/malgo/Builtin.mlg.$p_1835_0", i64 %"runtime/malgo/Builtin.mlg.$p_1836_0")
  ret i64 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_div_int32_t_curry_1828"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1829_0", i32 %"runtime/malgo/Builtin.mlg.$p_1830_0") {
  %2 = call i32 @malgo_div_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1829_0", i32 %"runtime/malgo/Builtin.mlg.$p_1830_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_mul_int32_t_curry_1822"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1823_0", i32 %"runtime/malgo/Builtin.mlg.$p_1824_0") {
  %2 = call i32 @malgo_mul_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1823_0", i32 %"runtime/malgo/Builtin.mlg.$p_1824_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1811_0", i32 %"runtime/malgo/Builtin.mlg.$p_1812_0")
  ret i32 %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1794_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1794_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Float#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1796_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { float } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { float } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { float } }, ptr %2, i32 0, i32 1, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1796_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Double#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1798_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { double } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { double } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { double } }, ptr %2, i32 0, i32 1, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1798_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Char#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_1800_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i8 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i8 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i8 } }, ptr %2, i32 0, i32 1, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_1800_0", ptr %4, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.String#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.False"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.True"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_panic"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_1806_0") {
  %2 = call ptr @malgo_panic(ptr %"runtime/malgo/Builtin.mlg.$p_1806_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_125"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_add_int32_t_curry_1810"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1808_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1808_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_125", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_126"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1814_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1814_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_126", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_127"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_mul_int32_t_curry_1822"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_mul_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1820_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1820_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_127", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_128"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_div_int32_t_curry_1828"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_div_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1826_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1826_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_128", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_129"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_add_int64_t_curry_1834"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1832_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1832_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_129", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_130"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_sub_int64_t_curry_1840"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1838_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1838_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_130", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_131"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_mul_int64_t_curry_1846"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_mul_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1844_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1844_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_131", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_132"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$malgo_div_int64_t_curry_1852"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_div_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1850_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1850_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_132", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_133"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$malgo_add_float_curry_1858"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1856_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1856_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_133", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_134"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$malgo_sub_float_curry_1864"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1862_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1862_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_134", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_135"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$malgo_mul_float_curry_1870"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_mul_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1868_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1868_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_135", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_136"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$malgo_div_float_curry_1876"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_div_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1874_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1874_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_136", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_137"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_add_double_curry_1882"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_add_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1880_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1880_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_137", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_138"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_sub_double_curry_1888"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1886_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1886_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_138", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_139"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_mul_double_curry_1894"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_mul_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1892_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1892_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_139", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_140"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$malgo_div_double_curry_1900"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_div_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1898_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1898_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_140", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"runtime/malgo/Builtin.mlg.sqrtf"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1904_0") {
  %2 = call float @sqrtf(float %"runtime/malgo/Builtin.mlg.$p_1904_0")
  ret float %2
}

define internal double @"runtime/malgo/Builtin.mlg.sqrt"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_1905_0") {
  %2 = call double @sqrt(double %"runtime/malgo/Builtin.mlg.$p_1905_0")
  ret double %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_141"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int32_t_curry_1908"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1906_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1906_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_141", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_142"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_int32_t_curry_1914"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1912_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1912_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_142", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_143"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_int32_t_curry_1920"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1918_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1918_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_143", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_144"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_int32_t_curry_1926"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1924_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1924_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_144", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_145"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int32_t_curry_1932"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1930_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1930_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_145", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_146"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_int32_t_curry_1938"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1936_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1936_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_146", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_147"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_int64_t_curry_1944"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1942_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1942_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_147", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_int64_t_curry_1950"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1948_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1948_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_148", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_149"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_int64_t_curry_1956"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1954_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1954_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_149", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_150"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_int64_t_curry_1962"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1960_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1960_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_150", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_151"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_int64_t_curry_1968"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1966_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1966_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_151", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_152"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_int64_t_curry_1974"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_int64_t"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_1972_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1972_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_152", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_153"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_float_curry_1980"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1978_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1978_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_153", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_154"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_float_curry_1986"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1984_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1984_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_154", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_float_curry_1992"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1990_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1990_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_155", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_156"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_float_curry_1998"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_1996_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_1996_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_156", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_157"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_float_curry_2004"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_2002_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_2002_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_157", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_158"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_float_curry_2010"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_float"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_2008_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$p_2008_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_158", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_159"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_double_curry_2016"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2014_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2014_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_159", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_160"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_double_curry_2022"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2020_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2020_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_160", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_161"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_double_curry_2028"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2026_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2026_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_161", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_162"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_double_curry_2034"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2032_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2032_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_162", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_163"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_double_curry_2040"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2038_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2038_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_163", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_164"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_double_curry_2046"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_double"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2044_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_2044_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_164", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_165"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_char_curry_2052"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2050_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2050_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_165", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_166"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_char_curry_2058"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2056_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2056_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_166", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_167"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_char_curry_2064"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2062_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2062_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_167", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_168"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_char_curry_2070"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2068_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2068_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_168", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_169"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_char_curry_2076"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2074_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2074_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_169", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_170"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_char_curry_2082"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2080_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2080_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_170", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_171"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_eq_string_curry_2088"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_eq_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2086_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2086_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_171", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_172"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ne_string_curry_2094"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ne_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2092_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2092_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_172", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_173"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_lt_string_curry_2100"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_lt_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2098_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2098_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_173", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_174"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_gt_string_curry_2106"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_gt_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2104_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_174", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_175"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_le_string_curry_2112"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_le_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2110_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2110_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_175", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_176"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_ge_string_curry_2118"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_ge_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2116_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2116_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_176", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.malgo_char_ord"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2122_0") {
  %2 = call i32 @malgo_char_ord(i8 %"runtime/malgo/Builtin.mlg.$p_2122_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.malgo_is_digit"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2123_0") {
  %2 = call i32 @malgo_is_digit(i8 %"runtime/malgo/Builtin.mlg.$p_2123_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.malgo_is_lower"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2124_0") {
  %2 = call i32 @malgo_is_lower(i8 %"runtime/malgo/Builtin.mlg.$p_2124_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.malgo_is_upper"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2125_0") {
  %2 = call i32 @malgo_is_upper(i8 %"runtime/malgo/Builtin.mlg.$p_2125_0")
  ret i32 %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.malgo_is_alphanum"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2126_0") {
  %2 = call i32 @malgo_is_alphanum(i8 %"runtime/malgo/Builtin.mlg.$p_2126_0")
  ret i32 %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.malgo_string_length"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2127_0") {
  %2 = call i64 @malgo_string_length(ptr %"runtime/malgo/Builtin.mlg.$p_2127_0")
  ret i64 %2
}

define internal i8 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_177"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i8 @"runtime/malgo/Builtin.mlg.$malgo_string_at_curry_2130"(ptr null, i64 %p_0, ptr %1)
  ret i8 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_string_at"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_2128_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_2128_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_177", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_178"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$malgo_string_cons_curry_2136"(ptr null, i8 %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_string_cons"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2134_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$p_2134_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_178", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_179"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$malgo_string_append_curry_2142"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_string_append"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2140_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_179", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_181"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %p_addr_1 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 1
  %p_1 = load i64, ptr %p_addr_1, align 4
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$malgo_substring_curry_2149"(ptr null, ptr %p_0, i64 %p_1, i64 %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_180"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, i64 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 0
  store ptr %p_0, ptr %p_1, align 8
  %p_2 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 1
  store i64 %1, ptr %p_2, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_181", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_substring"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2146_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_2146_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_180", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$p_2156_0") {
  %2 = call ptr @malgo_int64_t_to_string(i64 %"runtime/malgo/Builtin.mlg.$p_2156_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_float_to_string"(ptr %0, float %"runtime/malgo/Builtin.mlg.$p_2157_0") {
  %2 = call ptr @malgo_float_to_string(float %"runtime/malgo/Builtin.mlg.$p_2157_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string"(ptr %0, double %"runtime/malgo/Builtin.mlg.$p_2158_0") {
  %2 = call ptr @malgo_double_to_string(double %"runtime/malgo/Builtin.mlg.$p_2158_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_char_to_string"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2159_0") {
  %2 = call ptr @malgo_char_to_string(i8 %"runtime/malgo/Builtin.mlg.$p_2159_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2160_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"runtime/malgo/Builtin.mlg.$p_2160_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_newline"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2161_0") {
  %2 = call ptr @malgo_newline(ptr %"runtime/malgo/Builtin.mlg.$p_2161_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_char"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$p_2162_0") {
  %2 = call ptr @malgo_print_char(i8 %"runtime/malgo/Builtin.mlg.$p_2162_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_flush"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2164_0") {
  %2 = call ptr @malgo_flush(ptr %"runtime/malgo/Builtin.mlg.$p_2164_0")
  ret ptr %2
}

define internal i8 @"runtime/malgo/Builtin.mlg.malgo_get_char"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2165_0") {
  %2 = call i8 @malgo_get_char(ptr %"runtime/malgo/Builtin.mlg.$p_2165_0")
  ret i8 %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_get_contents"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2166_0") {
  %2 = call ptr @malgo_get_contents(ptr %"runtime/malgo/Builtin.mlg.$p_2166_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2172_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int64_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_int64_t_to_string_capture_0, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_2172_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2174_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2174_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0": ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"runtime/malgo/Builtin.mlg.$int64#_2174_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringInt64#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringInt64#_capture_0", align 8
  %"toStringInt64#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt64#", ptr %"toStringInt64#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i64 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_0, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringInt32#_capture_0", align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i32 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2186_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_float_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_float_to_string_capture_0, align 8
  %malgo_float_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_float_to_string", ptr %malgo_float_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2186_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2188_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2188_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2188_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringFloat#_capture_0", align 8
  %"toStringFloat#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringFloat#", ptr %"toStringFloat#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, float %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2193_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_double_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_double_to_string_capture_0, align 8
  %malgo_double_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string", ptr %malgo_double_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2193_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2195_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2195_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2195_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringDouble#_capture_0", align 8
  %"toStringDouble#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringDouble#", ptr %"toStringDouble#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, double %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2200_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_char_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_char_to_string_capture_0, align 8
  %malgo_char_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_char_to_string", ptr %malgo_char_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2200_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2202_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2202_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2202_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringChar#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringChar#_capture_0", align 8
  %"toStringChar#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringChar#", ptr %"toStringChar#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_183"(ptr %0, i64 %1) {
  %str_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %start_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 1
  %start_0 = load i64, ptr %start_addr_0, align 4
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$substring#_curry_2215"(ptr null, ptr %str_0, i64 %start_0, i64 %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_182"(ptr %0, i64 %1) {
  %str_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, i64 }, ptr null, i32 1) to i64))
  %str_1 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 0
  store ptr %str_0, ptr %str_1, align 8
  %start_0 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 1
  store i64 %1, ptr %start_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_183", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.substring#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$str_2207_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %str_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$str_2207_0", ptr %str_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_182", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_185"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %"string#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$substring_curry_2239"(ptr null, ptr %"string#_0", ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_184"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %1, ptr %"int64#_0", align 8
  %"string#_1" = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"string#_0", ptr %"string#_1", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_185", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.substring"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2226_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_2226_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_184", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_186"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$subInt64#_curry_2260"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2255_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2255_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_186", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_187"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt64_curry_2276"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2267_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_2267_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_187", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_188"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2287_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2287_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_188", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_189"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_189", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_190"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$subFloat#_curry_2324"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2319_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_2319_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_190", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_191"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subFloat_curry_2340"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2331_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_2331_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_191", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_192"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$subDouble#_curry_2356"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2351_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_2351_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_192", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_193"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subDouble_curry_2372"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2363_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_2363_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_193", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.string#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2383_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2383_0")
  ret ptr %7
}

define internal float @"runtime/malgo/Builtin.mlg.sqrtFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2385_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %sqrtf_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %sqrtf_capture_0, align 8
  %sqrtf_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.sqrtf", ptr %sqrtf_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call float %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_2385_0")
  ret float %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.sqrtFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2387_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$float#_2387_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Float#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Float#_0": ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"runtime/malgo/Builtin.mlg.$float#_2387_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"sqrtFloat#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"sqrtFloat#_capture_0", align 8
  %"sqrtFloat#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.sqrtFloat#", ptr %"sqrtFloat#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call float %11(ptr %9, float %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, float %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal double @"runtime/malgo/Builtin.mlg.sqrtDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2392_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %sqrt_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %sqrt_capture_0, align 8
  %sqrt_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.sqrt", ptr %sqrt_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_2392_0")
  ret double %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.sqrtDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2394_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$double#_2394_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Double#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Double#_0": ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2394_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"sqrtDouble#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"sqrtDouble#_capture_0", align 8
  %"sqrtDouble#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.sqrtDouble#", ptr %"sqrtDouble#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call double %11(ptr %9, double %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, double %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"printString#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"printString#_capture_0", align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %6)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.printChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2404_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_char_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_print_char_capture_0, align 8
  %malgo_print_char_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_char", ptr %malgo_print_char_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2404_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.printChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2406_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2406_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2406_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"printChar#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"printChar#_capture_0", align 8
  %"printChar#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printChar#", ptr %"printChar#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i8 %6)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.panic"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2409_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2409_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2409_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_panic_capture_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %malgo_panic_capture_0, align 8
  %malgo_panic_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_panic", ptr %malgo_panic_func_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %6)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.ordChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2412_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_char_ord_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_char_ord_capture_0, align 8
  %malgo_char_ord_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_char_ord", ptr %malgo_char_ord_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2412_0")
  ret i32 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.ordChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2414_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2414_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2414_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"ordChar#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"ordChar#_capture_0", align 8
  %"ordChar#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.ordChar#", ptr %"ordChar#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i32 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.not"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$true_2419_0") {
  %2 = getelementptr { i8, {} }, ptr %"runtime/malgo/Builtin.mlg.$true_2419_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Builtin.mlg.$true_2419_0", i32 0, i32 1
  %5 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %5

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"runtime/malgo/Builtin.mlg.$true_2419_0", i32 0, i32 1
  %7 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.newline"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$__2420_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_newline_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_newline_capture_0, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %2)
  ret ptr %9
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_194"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neString#_curry_2429"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2424_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_2424_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_194", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_195"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neInt64#_curry_2441"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2436_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2436_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_195", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_196"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neInt32#_curry_2453"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2448_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2448_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_196", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_197"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neFloat#_curry_2465"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2460_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_2460_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_197", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_198"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neDouble#_curry_2477"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2472_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_2472_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_198", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_199"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$neChar#_curry_2489"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2484_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_2484_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_199", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_200"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$mulInt64#_curry_2501"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2496_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2496_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_200", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_201"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$mulInt64_curry_2517"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2508_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_2508_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_201", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_202"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$mulInt32#_curry_2533"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2528_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2528_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_202", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_203"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$mulInt32_curry_2549"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2540_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2540_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_203", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_204"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$mulFloat#_curry_2565"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2560_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_2560_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_204", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_205"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$mulFloat_curry_2581"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2572_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_2572_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_205", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_206"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$mulDouble#_curry_2597"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2592_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_2592_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_206", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_207"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$mulDouble_curry_2613"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2604_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_2604_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_207", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_208"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltString#_curry_2629"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2624_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_2624_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_208", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_209"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltInt64#_curry_2641"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2636_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2636_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_209", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_210"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltInt32#_curry_2653"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2648_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2648_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_210", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_211"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltFloat#_curry_2665"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2660_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_2660_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_211", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_212"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltDouble#_curry_2677"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2672_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_2672_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_212", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_213"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$ltChar#_curry_2689"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2684_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_2684_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_213", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"runtime/malgo/Builtin.mlg.lengthString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2696_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_string_length_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_string_length_capture_0, align 8
  %malgo_string_length_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_string_length", ptr %malgo_string_length_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, ptr %"runtime/malgo/Builtin.mlg.$x_2696_0")
  ret i64 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.lengthString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2698_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_2698_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2698_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"lengthString#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"lengthString#_capture_0", align 8
  %"lengthString#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.lengthString#", ptr %"lengthString#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i64 %11(ptr %9, ptr %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i64 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_214"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leString#_curry_2708"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_2703_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_2703_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_214", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_215"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leInt64#_curry_2720"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_2715_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_2715_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_215", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_216"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leInt32#_curry_2732"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2727_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2727_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_216", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_217"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leFloat#_curry_2744"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_2739_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_2739_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_217", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_218"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leDouble#_curry_2756"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_2751_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_2751_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_218", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_219"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$leChar#_curry_2768"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2763_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_2763_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_219", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.isUpper#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_2775_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_is_upper_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_is_upper_capture_0, align 8
  %malgo_is_upper_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_is_upper", ptr %malgo_is_upper_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_2775_0")
  ret i32 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.isTrue#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0") {
  switch i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0", label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %1
  %2 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %2

switch-unboxed_default_0:                         ; preds = %1
  %3 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.isUpper"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2779_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_2779_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_2779_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isUpper#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isUpper#_capture_0", align 8
  %"isUpper#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isUpper#", ptr %"isUpper#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i32 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_220"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leChar_curry_2793"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2784_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_2784_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_220", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_221"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leDouble_curry_2813"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2804_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_2804_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_221", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_222"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leFloat_curry_2833"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2824_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_2824_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_222", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_223"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leInt32_curry_2853"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2844_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2844_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_223", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_224"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leInt64_curry_2873"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2864_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_2864_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_224", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_225"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$leString_curry_2893"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.leString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_2884_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_2884_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_225", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_226"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltChar_curry_2913"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_2904_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_2904_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_226", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_227"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltDouble_curry_2933"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_2924_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_2924_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_227", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_228"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltFloat_curry_2953"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_2944_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_2944_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_228", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_229"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltInt32_curry_2973"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2964_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2964_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_229", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_230"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltInt64_curry_2993"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_2984_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_2984_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_230", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_231"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$ltString_curry_3013"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.ltString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3004_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3004_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_231", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_232"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neChar_curry_3033"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3024_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_3024_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_232", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_233"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neDouble_curry_3053"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3044_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_3044_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_233", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_234"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neFloat_curry_3073"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3064_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_3064_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_234", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_235"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neInt32_curry_3093"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3084_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_3084_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_235", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_236"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neInt64_curry_3113"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3104_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_236", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_237"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$neString_curry_3133"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.neString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3124_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3124_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_237", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.isLower#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3144_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_is_lower_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_is_lower_capture_0, align 8
  %malgo_is_lower_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_is_lower", ptr %malgo_is_lower_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3144_0")
  ret i32 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.isLower"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3146_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3146_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3146_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isLower#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isLower#_capture_0", align 8
  %"isLower#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isLower#", ptr %"isLower#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i32 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.isDigit#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3151_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_is_digit_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_is_digit_capture_0, align 8
  %malgo_is_digit_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_is_digit", ptr %malgo_is_digit_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3151_0")
  ret i32 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.isDigit"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3153_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3153_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3153_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isDigit#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isDigit#_capture_0", align 8
  %"isDigit#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isDigit#", ptr %"isDigit#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i32 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"runtime/malgo/Builtin.mlg.isAlphanum#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3158_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_is_alphanum_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_is_alphanum_capture_0, align 8
  %malgo_is_alphanum_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_is_alphanum", ptr %malgo_is_alphanum_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3158_0")
  ret i32 %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.isAlphanum"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3160_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$char#_3160_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Builtin.mlg.$char#_3160_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isAlphanum#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"isAlphanum#_capture_0", align 8
  %"isAlphanum#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isAlphanum#", ptr %"isAlphanum#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i8 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"isTrue#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"isTrue#_capture_0", align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, i32 %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.int64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3165_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"runtime/malgo/Builtin.mlg.$x_3165_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3167_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"runtime/malgo/Builtin.mlg.$x_3167_0")
  ret ptr %7
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_238"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtString#_curry_3174"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3169_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_3169_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_238", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_239"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtString_curry_3190"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3181_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3181_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_239", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_240"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtInt64#_curry_3206"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3201_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3201_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_240", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_241"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtInt64_curry_3222"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3213_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3213_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_241", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_242"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtInt32#_curry_3238"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3233_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_3233_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_242", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_243"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtInt32_curry_3254"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3245_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_3245_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_243", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_244"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtFloat#_curry_3270"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3265_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_3265_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_244", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_245"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtFloat_curry_3286"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3277_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_3277_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_245", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_246"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtDouble#_curry_3302"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3297_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_3297_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_246", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_247"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtDouble_curry_3318"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3309_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_3309_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_247", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_248"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$gtChar#_curry_3334"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3329_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_3329_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_248", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_249"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$gtChar_curry_3350"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.gtChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3341_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_3341_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_249", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.getContents"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$__3361_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_get_contents_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_get_contents_capture_0, align 8
  %malgo_get_contents_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_get_contents", ptr %malgo_get_contents_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %2)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %9)
  ret ptr %15
}

define internal ptr @"runtime/malgo/Builtin.mlg.getChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$__3367_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_get_char_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_get_char_capture_0, align 8
  %malgo_get_char_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_get_char", ptr %malgo_get_char_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call i8 %8(ptr %6, ptr %2)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Char#_capture_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %"Char#_capture_0", align 8
  %"Char#_func_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Char#", ptr %"Char#_func_0", align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, i8 %9)
  ret ptr %15
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_250"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geString#_curry_3378"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3373_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_3373_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_250", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_251"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geString_curry_3394"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3385_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3385_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_251", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_252"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geInt64#_curry_3410"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3405_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3405_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_252", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_253"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geInt64_curry_3426"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3417_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3417_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_253", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_254"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geInt32#_curry_3442"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3437_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_3437_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_254", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_255"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geInt32_curry_3458"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3449_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_3449_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_255", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_256"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geFloat#_curry_3474"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3469_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_3469_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_256", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_257"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geFloat_curry_3490"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3481_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_3481_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_257", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_258"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geDouble#_curry_3506"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3501_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_3501_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_258", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_259"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geDouble_curry_3522"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3513_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_3513_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_259", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_260"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$geChar#_curry_3538"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3533_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_3533_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_260", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_261"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$geChar_curry_3554"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.geChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3545_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_3545_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_261", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.flush"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$__3565_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_flush_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_flush_capture_0, align 8
  %malgo_flush_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_flush", ptr %malgo_flush_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %2)
  ret ptr %9
}

define internal ptr @"runtime/malgo/Builtin.mlg.float#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3569_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Float#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"Float#_capture_0", align 8
  %"Float#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Float#", ptr %"Float#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, float %"runtime/malgo/Builtin.mlg.$x_3569_0")
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.exitFailure"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$__3571_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_exit_failure_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_exit_failure_capture_0, align 8
  %malgo_exit_failure_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %2)
  ret ptr %9
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_262"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqString#_curry_3580"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3575_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_3575_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_262", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_263"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqString_curry_3596"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3587_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3587_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_263", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_264"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqInt64#_curry_3612"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3607_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3607_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_264", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_265"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqInt64_curry_3628"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3619_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3619_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_265", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_266"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqInt32#_curry_3644"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3639_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_3639_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_266", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_267"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqInt32_curry_3660"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3651_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_3651_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_267", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_268"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqFloat#_curry_3676"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3671_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_3671_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_268", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_269"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqFloat_curry_3692"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3683_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_3683_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_269", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqDouble#_curry_3708"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3703_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_3703_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_271"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqDouble_curry_3724"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3715_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_3715_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_271", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_272"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$eqChar#_curry_3740"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqChar#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3735_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$x_3735_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_272", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_273"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$eqChar_curry_3756"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqChar"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3747_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_3747_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_273", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.double#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3767_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Double#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"Double#_capture_0", align 8
  %"Double#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, double %"runtime/malgo/Builtin.mlg.$x_3767_0")
  ret ptr %7
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_274"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$divInt64#_curry_3774"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3769_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3769_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_274", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_275"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$divInt64_curry_3790"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3781_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3781_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_275", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_276"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$divInt32#_curry_3806"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_3801_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_3801_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_276", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_277"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$divInt32_curry_3822"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_3813_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_3813_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_277", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_278"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$divFloat#_curry_3838"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_3833_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_3833_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_278", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_279"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$divFloat_curry_3854"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_3845_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_3845_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_279", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_280"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$divDouble#_curry_3870"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_3865_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_3865_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_280", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_281"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$divDouble_curry_3886"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.divDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_3877_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_3877_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_281", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_282"(ptr %0, ptr %1) {
  %c_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %c_0 = load i8, ptr %c_addr_0, align 1
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$consString#_curry_3902"(ptr null, i8 %c_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.consString#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$c_3897_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %c_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"runtime/malgo/Builtin.mlg.$c_3897_0", ptr %c_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_282", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_283"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$consString_curry_3918"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.consString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$char#_3909_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$char#_3909_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_283", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.char#"(ptr %0, i8 %"runtime/malgo/Builtin.mlg.$x_3929_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Char#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"Char#_capture_0", align 8
  %"Char#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Char#", ptr %"Char#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i8 %"runtime/malgo/Builtin.mlg.$x_3929_0")
  ret ptr %7
}

define internal i8 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_284"(ptr %0, ptr %1) {
  %i_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %i_0 = load i64, ptr %i_addr_0, align 4
  %3 = call i8 @"runtime/malgo/Builtin.mlg.$atString#_curry_3936"(ptr null, i64 %i_0, ptr %1)
  ret i8 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.atString#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$i_3931_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %i_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$i_3931_0", ptr %i_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_284", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_285"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$atString_curry_3952"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.atString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_3943_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_3943_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_285", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_286"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$appendString#_curry_3968"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3963_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_3963_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_286", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_287"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3975_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$string#_3975_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_287", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_288"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"runtime/malgo/Builtin.mlg.$addInt64#_curry_4000"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt64#"(ptr %0, i64 %"runtime/malgo/Builtin.mlg.$x_3995_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$x_3995_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_288", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_289"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$addInt64_curry_4016"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt64"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int64#_4007_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int64#_4007_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_289", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_290"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4027_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_4027_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_290", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_291"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$addInt32_curry_4048"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_291", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_292"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"runtime/malgo/Builtin.mlg.$addFloat#_curry_4064"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addFloat#"(ptr %0, float %"runtime/malgo/Builtin.mlg.$x_4059_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"runtime/malgo/Builtin.mlg.$x_4059_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_292", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_293"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$addFloat_curry_4080"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addFloat"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$float#_4071_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$float#_4071_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_293", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_294"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"runtime/malgo/Builtin.mlg.$addDouble#_curry_4096"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addDouble#"(ptr %0, double %"runtime/malgo/Builtin.mlg.$x_4091_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"runtime/malgo/Builtin.mlg.$x_4091_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_294", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_295"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$addDouble_curry_4112"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.addDouble"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$double#_4103_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$double#_4103_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_295", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_296"(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %g_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %g_0 = load ptr, ptr %g_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %g_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %g_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %1)
  %8 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %f_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  ret ptr %12
}

define internal ptr @"runtime/malgo/Prelude.mlg.$._curry_1136"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$f_1137_0", ptr %"runtime/malgo/Prelude.mlg.$g_1138_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$f_1137_0", ptr %f_0, align 8
  %g_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"runtime/malgo/Prelude.mlg.$g_1138_0", ptr %g_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_296", ptr %fun_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.$<|_curry_1124"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$f_1125_0", ptr %"runtime/malgo/Prelude.mlg.$x_1126_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_1125_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_1125_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"runtime/malgo/Prelude.mlg.$x_1126_0")
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_298"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %tailString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %tailString_0 = load ptr, ptr %tailString_addr_0, align 8
  %pred_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %takeWhileString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %takeWhileString_0 = load ptr, ptr %takeWhileString_addr_0, align 8
  %consString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %consString_0 = load ptr, ptr %consString_addr_0, align 8
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 5
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %consString_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %consString_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %takeWhileString_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %takeWhileString_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %pred_0)
  %13 = getelementptr { ptr, ptr }, ptr %tailString_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %tailString_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %str_0)
  %18 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  %23 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_299"(ptr %0, ptr %1) {
  %"String#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr @str300)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_297"(ptr %0, ptr %1) {
  %tailString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %tailString_0 = load ptr, ptr %tailString_addr_0, align 8
  %pred_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %takeWhileString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %takeWhileString_0 = load ptr, ptr %takeWhileString_addr_0, align 8
  %consString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %consString_0 = load ptr, ptr %consString_addr_0, align 8
  %if_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 5
  %if_0 = load ptr, ptr %if_addr_0, align 8
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 6
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  ret ptr %str_0

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %2
  %6 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %7 = getelementptr { ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %pred_0, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %pred_0, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %8)
  %14 = getelementptr { ptr, ptr }, ptr %if_0, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %if_0, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %8, ptr %p_0, align 8
  %tailString_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %tailString_0, ptr %tailString_1, align 8
  %pred_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %pred_0, ptr %pred_1, align 8
  %takeWhileString_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %takeWhileString_0, ptr %takeWhileString_1, align 8
  %consString_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 4
  store ptr %consString_0, ptr %consString_1, align 8
  %str_1 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 5
  store ptr %str_0, ptr %str_1, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_298", ptr %fun_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %19)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"String#_1" = getelementptr { ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %"String#_0", ptr %"String#_1", align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_299", ptr %fun_func_1, align 8
  %26 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  ret ptr %30

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.$takeWhileString_curry_1085"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$pred_1086_0", ptr %"runtime/malgo/Prelude.mlg.$str_1087_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %headString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %headString_capture_0, align 8
  %headString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.headString", ptr %headString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_1087_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %case_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %case_capture_0, align 8
  %case_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.case", ptr %case_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %tailString_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %tailString_capture_0, align 8
  %tailString_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.tailString", ptr %tailString_func_0, align 8
  %tailString_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %15, ptr %tailString_0, align 8
  %pred_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"runtime/malgo/Prelude.mlg.$pred_1086_0", ptr %pred_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %"String#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %16, ptr %"String#_0", align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %takeWhileString_capture_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr null, ptr %takeWhileString_capture_0, align 8
  %takeWhileString_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.takeWhileString", ptr %takeWhileString_func_0, align 8
  %takeWhileString_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %17, ptr %takeWhileString_0, align 8
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %consString_capture_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr null, ptr %consString_capture_0, align 8
  %consString_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.consString", ptr %consString_func_0, align 8
  %consString_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 4
  store ptr %18, ptr %consString_0, align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %if_capture_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr null, ptr %if_capture_0, align 8
  %if_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0, align 8
  %if_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 5
  store ptr %19, ptr %if_0, align 8
  %str_0 = getelementptr { ptr, ptr, ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 6
  store ptr %"runtime/malgo/Prelude.mlg.$str_1087_0", ptr %str_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_297", ptr %fun_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %14)
  ret ptr %24
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_302"(ptr %0, ptr %1) {
  %tailString_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %tailString_0 = load ptr, ptr %tailString_addr_0, align 8
  %pred_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %dropWhileString_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %dropWhileString_0 = load ptr, ptr %dropWhileString_addr_0, align 8
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %dropWhileString_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %dropWhileString_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %pred_0)
  %8 = getelementptr { ptr, ptr }, ptr %tailString_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %tailString_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %str_0)
  %13 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_303"(ptr %0, ptr %1) {
  %str_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  ret ptr %str_0
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_301"(ptr %0, ptr %1) {
  %tailString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %tailString_0 = load ptr, ptr %tailString_addr_0, align 8
  %pred_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %dropWhileString_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %dropWhileString_0 = load ptr, ptr %dropWhileString_addr_0, align 8
  %if_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %if_0 = load ptr, ptr %if_addr_0, align 8
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 4
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  ret ptr %str_0

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %2
  %6 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %7 = getelementptr { ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %pred_0, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %pred_0, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %8)
  %14 = getelementptr { ptr, ptr }, ptr %if_0, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %if_0, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %tailString_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %tailString_0, ptr %tailString_1, align 8
  %pred_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %pred_0, ptr %pred_1, align 8
  %dropWhileString_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %dropWhileString_0, ptr %dropWhileString_1, align 8
  %str_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %str_0, ptr %str_1, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_302", ptr %fun_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %19)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %str_2 = getelementptr { ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %str_0, ptr %str_2, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_303", ptr %fun_func_1, align 8
  %26 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  ret ptr %30

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.$dropWhileString_curry_1020"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$pred_1021_0", ptr %"runtime/malgo/Prelude.mlg.$str_1022_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %headString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %headString_capture_0, align 8
  %headString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.headString", ptr %headString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_1022_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %case_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %case_capture_0, align 8
  %case_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.case", ptr %case_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %tailString_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %tailString_capture_0, align 8
  %tailString_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.tailString", ptr %tailString_func_0, align 8
  %tailString_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %15, ptr %tailString_0, align 8
  %pred_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"runtime/malgo/Prelude.mlg.$pred_1021_0", ptr %pred_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %dropWhileString_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %dropWhileString_capture_0, align 8
  %dropWhileString_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.dropWhileString", ptr %dropWhileString_func_0, align 8
  %dropWhileString_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %16, ptr %dropWhileString_0, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %if_capture_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr null, ptr %if_capture_0, align 8
  %if_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0, align 8
  %if_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %17, ptr %if_0, align 8
  %str_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 4
  store ptr %"runtime/malgo/Prelude.mlg.$str_1022_0", ptr %str_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_301", ptr %fun_func_0, align 8
  %18 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %14)
  ret ptr %22
}

define internal ptr @"runtime/malgo/Prelude.mlg.$case_curry_986"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_987_0", ptr %"runtime/malgo/Prelude.mlg.$f_988_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_988_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_988_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"runtime/malgo/Prelude.mlg.$x_987_0")
  ret ptr %6
}

define internal ptr @"runtime/malgo/Prelude.mlg.$const_curry_958"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$a_959_0", ptr %"runtime/malgo/Prelude.mlg.$__960_0") {
  ret ptr %"runtime/malgo/Prelude.mlg.$a_959_0"
}

define internal ptr @"runtime/malgo/Prelude.mlg.$foldl_curry_939"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__940_0", ptr %"runtime/malgo/Prelude.mlg.$z_941_0", ptr %"runtime/malgo/Prelude.mlg.$nil_942_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_942_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_942_0", i32 0, i32 1
  ret ptr %"runtime/malgo/Prelude.mlg.$z_941_0"

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_942_0", i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %foldl_capture_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %foldl_capture_0, align 8
  %foldl_func_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.foldl", ptr %foldl_func_0, align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %"runtime/malgo/Prelude.mlg.$__940_0")
  %16 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__940_0", i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__940_0", i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %"runtime/malgo/Prelude.mlg.$z_941_0")
  %21 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr %24(ptr %22, ptr %7)
  %26 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr %34(ptr %32, ptr %9)
  ret ptr %35

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_304"(ptr %0, ptr %1) {
  %tValue_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %tValue_0 = load ptr, ptr %tValue_addr_0, align 8
  ret ptr %tValue_0
}

define internal ptr @"runtime/malgo/Prelude.mlg.$unless_curry_876"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$c_877_0", ptr %"runtime/malgo/Prelude.mlg.$tValue_878_0", ptr %"runtime/malgo/Prelude.mlg.$f_879_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %if_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %if_capture_0, align 8
  %if_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$c_877_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %tValue_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$tValue_878_0", ptr %tValue_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_304", ptr %fun_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %8)
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %"runtime/malgo/Prelude.mlg.$f_879_0")
  ret ptr %18
}

define internal ptr @"runtime/malgo/Prelude.mlg.$if_curry_826"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$true_827_0", ptr %"runtime/malgo/Prelude.mlg.$t_828_0", ptr %"runtime/malgo/Prelude.mlg.$__829_0") {
  %2 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$true_827_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$true_827_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__829_0", i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__829_0", i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %5)
  ret ptr %11

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %1
  %12 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$true_827_0", i32 0, i32 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$t_828_0", i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$t_828_0", i32 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr %18(ptr %16, ptr %13)
  ret ptr %19

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.$mapList_curry_790"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__791_0", ptr %"runtime/malgo/Prelude.mlg.$nil_792_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", i32 0, i32 1
  %5 = call ptr @"runtime/malgo/Prelude.mlg.Nil"(ptr null)
  ret ptr %5

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_792_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__791_0", i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$__791_0", i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %Cons_capture_0, align 8
  %Cons_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %15)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %mapList_capture_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr null, ptr %mapList_capture_0, align 8
  %mapList_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.mapList", ptr %mapList_func_0, align 8
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %"runtime/malgo/Prelude.mlg.$__791_0")
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %10)
  %33 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %34 = load ptr, ptr %33, align 8
  %35 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = call ptr %36(ptr %34, ptr %32)
  ret ptr %37

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.$punctuate_curry_746"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__747_0", ptr %"runtime/malgo/Prelude.mlg.$nil_748_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", i32 0, i32 1
  %5 = call ptr @"runtime/malgo/Prelude.mlg.Nil"(ptr null)
  ret ptr %5

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_748_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_1"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_1"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_1":  ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %13 = getelementptr { i8, {} }, ptr %10, i32 0, i32 1
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %Cons_capture_0, align 8
  %Cons_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0, align 8
  %15 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr %18(ptr %16, ptr %8)
  %20 = call ptr @"runtime/malgo/Prelude.mlg.Nil"(ptr null)
  %21 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr %24(ptr %22, ptr %20)
  ret ptr %25

"switch_branch_runtime/malgo/Prelude.mlg.Cons_1": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %27 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_1 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  store ptr null, ptr %Cons_capture_1, align 8
  %Cons_func_1 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_1, align 8
  %32 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  %35 = load ptr, ptr %34, align 8
  %36 = call ptr %35(ptr %33, ptr %8)
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_2 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  store ptr null, ptr %Cons_capture_2, align 8
  %Cons_func_2 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_2, align 8
  %38 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  %39 = load ptr, ptr %38, align 8
  %40 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr %41(ptr %39, ptr %"runtime/malgo/Prelude.mlg.$__747_0")
  %43 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %punctuate_capture_0 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 0
  store ptr null, ptr %punctuate_capture_0, align 8
  %punctuate_func_0 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.punctuate", ptr %punctuate_func_0, align 8
  %44 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 0
  %45 = load ptr, ptr %44, align 8
  %46 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 1
  %47 = load ptr, ptr %46, align 8
  %48 = call ptr %47(ptr %45, ptr %"runtime/malgo/Prelude.mlg.$__747_0")
  %49 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 0
  %50 = load ptr, ptr %49, align 8
  %51 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 1
  %52 = load ptr, ptr %51, align 8
  %53 = call ptr %52(ptr %50, ptr %10)
  %54 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 0
  %55 = load ptr, ptr %54, align 8
  %56 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 1
  %57 = load ptr, ptr %56, align 8
  %58 = call ptr %57(ptr %55, ptr %53)
  %59 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  %60 = load ptr, ptr %59, align 8
  %61 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  %62 = load ptr, ptr %61, align 8
  %63 = call ptr %62(ptr %60, ptr %58)
  ret ptr %63

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.$|>_curry_702"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_703_0", ptr %"runtime/malgo/Prelude.mlg.$f_704_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_704_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"runtime/malgo/Prelude.mlg.$f_704_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"runtime/malgo/Prelude.mlg.$x_703_0")
  ret ptr %6
}

define internal ptr @"runtime/malgo/Prelude.mlg.$Cons_curry_694"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$p_695_0", ptr %"runtime/malgo/Prelude.mlg.$p_696_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$p_695_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"runtime/malgo/Prelude.mlg.$p_696_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.Nothing"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.Just"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$p_688_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$p_688_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.Nil"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_305"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$Cons_curry_694"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.Cons"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$p_691_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$p_691_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_305", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_306"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$|>_curry_702"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.|>"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_699_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$x_699_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_306", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.tail"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$cons_707_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$cons_707_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$cons_707_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %exitFailure_capture_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %exitFailure_capture_0, align 8
  %exitFailure_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.exitFailure", ptr %exitFailure_func_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %5)
  ret ptr %12

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$cons_707_0", i32 0, i32 1
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.snd"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$tuple_713_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$tuple_713_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$tuple_713_0", i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_716_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %newline_capture_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %newline_capture_0, align 8
  %newline_func_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0, align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  ret ptr %15
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_723_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_723_0")
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_307"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$punctuate_curry_746"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.punctuate"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__725_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$__725_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_307", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt64"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$i_769_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %toStringInt64_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %toStringInt64_capture_0, align 8
  %toStringInt64_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt64", ptr %toStringInt64_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$i_769_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  ret ptr %13
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$i_773_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %toStringInt32_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %toStringInt32_capture_0, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$i_773_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_308"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$mapList_curry_790"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.mapList"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__777_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$__777_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_308", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.listToString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$nil_805_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_805_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_805_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str300)
  ret ptr %10

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_805_0", i32 0, i32 1
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %consString_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %consString_capture_0, align 8
  %consString_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.consString", ptr %consString_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %13)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %listToString_capture_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr null, ptr %listToString_capture_0, align 8
  %listToString_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.listToString", ptr %listToString_func_0, align 8
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %15)
  %28 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %27)
  ret ptr %32

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.isWhiteSpace"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$char#_814_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$char#_814_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Char#_0": ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"runtime/malgo/Prelude.mlg.$char#_814_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch-unboxed_default_0 [
    i8 32, label %"switch-unboxed_branch_' '_0"
    i8 10, label %"switch-unboxed_branch_'\\n'_0"
    i8 13, label %"switch-unboxed_branch_'\\r'_0"
    i8 9, label %"switch-unboxed_branch_'\\t'_0"
  ]

"switch-unboxed_branch_' '_0":                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %7 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %7

"switch-unboxed_branch_'\\n'_0":                  ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %8 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %8

"switch-unboxed_branch_'\\r'_0":                  ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %9 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %9

"switch-unboxed_branch_'\\t'_0":                  ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %10 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %10

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Char#_0"
  %11 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_310"(ptr %0, ptr %1) {
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$if_curry_826"(ptr null, ptr %true_0, ptr %t_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_309"(ptr %0, ptr %1) {
  %true_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %true_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %true_0, ptr %true_1, align 8
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %1, ptr %t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_310", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.if"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$true_817_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %true_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$true_817_0", ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_309", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_311"(ptr %0, ptr %1) {
  %str_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  ret ptr %str_0
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_312"(ptr %0, ptr %1) {
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %lengthString_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %lengthString_0 = load ptr, ptr %lengthString_addr_0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %substring_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %substring_0 = load ptr, ptr %substring_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %substring_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %substring_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %str_0)
  %8 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i64 1)
  %13 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  %18 = getelementptr { ptr, ptr }, ptr %lengthString_0, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %lengthString_0, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %str_0)
  %23 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27
}

define internal ptr @"runtime/malgo/Prelude.mlg.tailString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_838_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %eqString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %eqString_capture_0, align 8
  %eqString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqString", ptr %eqString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_838_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr @str300)
  %14 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %if_capture_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr null, ptr %if_capture_0, align 8
  %if_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %18)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %str_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$str_838_0", ptr %str_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_311", ptr %fun_func_0, align 8
  %26 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %str_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$str_838_0", ptr %str_1, align 8
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %lengthString_capture_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr null, ptr %lengthString_capture_0, align 8
  %lengthString_func_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.lengthString", ptr %lengthString_func_0, align 8
  %lengthString_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %32, ptr %lengthString_0, align 8
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %"Int64#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 2
  store ptr %33, ptr %"Int64#_0", align 8
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %substring_capture_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr null, ptr %substring_capture_0, align 8
  %substring_func_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.substring", ptr %substring_func_0, align 8
  %substring_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 3
  store ptr %34, ptr %substring_0, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_312", ptr %fun_func_1, align 8
  %35 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call ptr %38(ptr %36, ptr %31)
  ret ptr %39
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_314"(ptr %0, ptr %1) {
  %tValue_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %tValue_0 = load ptr, ptr %tValue_addr_0, align 8
  %c_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %c_0 = load ptr, ptr %c_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$unless_curry_876"(ptr null, ptr %c_0, ptr %tValue_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_313"(ptr %0, ptr %1) {
  %c_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %c_0 = load ptr, ptr %c_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %tValue_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %1, ptr %tValue_0, align 8
  %c_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %c_0, ptr %c_1, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_314", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.unless"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$c_865_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %c_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$c_865_0", ptr %c_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_313", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.identity"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_890_0") {
  ret ptr %"runtime/malgo/Prelude.mlg.$x_890_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_315"(ptr %0, ptr %1) {
  %3 = call ptr @"runtime/malgo/Prelude.mlg.Nothing"(ptr null)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_316"(ptr %0, ptr %1) {
  %Just_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %Just_0 = load ptr, ptr %Just_addr_0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %str_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %atString_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %atString_0 = load ptr, ptr %atString_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 0)
  %8 = getelementptr { ptr, ptr }, ptr %atString_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %atString_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %str_0)
  %18 = getelementptr { ptr, ptr }, ptr %Just_0, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %Just_0, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define internal ptr @"runtime/malgo/Prelude.mlg.headString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_891_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %eqString_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %eqString_capture_0, align 8
  %eqString_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqString", ptr %eqString_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"runtime/malgo/Prelude.mlg.$str_891_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr @str300)
  %14 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %if_capture_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr null, ptr %if_capture_0, align 8
  %if_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.if", ptr %if_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %18)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_315", ptr %fun_func_0, align 8
  %26 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Just_capture_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr null, ptr %Just_capture_0, align 8
  %Just_func_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Just", ptr %Just_func_0, align 8
  %Just_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %32, ptr %Just_0, align 8
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int64#_capture_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr null, ptr %"Int64#_capture_0", align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %"Int64#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %33, ptr %"Int64#_0", align 8
  %str_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 2
  store ptr %"runtime/malgo/Prelude.mlg.$str_891_0", ptr %str_0, align 8
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %atString_capture_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr null, ptr %atString_capture_0, align 8
  %atString_func_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.atString", ptr %atString_func_0, align 8
  %atString_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i32 0, i32 3
  store ptr %34, ptr %atString_0, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_316", ptr %fun_func_1, align 8
  %35 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call ptr %38(ptr %36, ptr %31)
  ret ptr %39
}

define internal ptr @"runtime/malgo/Prelude.mlg.head"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %exitFailure_capture_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %exitFailure_capture_0, align 8
  %exitFailure_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.exitFailure", ptr %exitFailure_func_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %5)
  ret ptr %12

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0", i32 0, i32 1
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  ret ptr %15

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.fst"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$tuple_922_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$tuple_922_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$tuple_922_0", i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  ret ptr %6

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_318"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %z_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %z_0 = load ptr, ptr %z_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$foldl_curry_939"(ptr null, ptr %__0, ptr %z_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_317"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %__1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %__0, ptr %__1, align 8
  %z_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %1, ptr %z_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_318", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.foldl"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$__925_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$__925_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_317", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_319"(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$const_curry_958"(ptr null, ptr %a_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.const"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$a_956_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %a_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$a_956_0", ptr %a_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_319", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.cond"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$nil_962_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_962_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_962_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str320)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %panic_capture_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr null, ptr %panic_capture_0, align 8
  %panic_func_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.panic", ptr %panic_func_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %10)
  ret ptr %16

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_962_0", i32 0, i32 1
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <16 x i8> }, ptr %19, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1
  %25 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { i8, {} }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %"switch_branch_Tuple#_0"
  %31 = getelementptr { i8, {} }, ptr %26, i32 0, i32 1
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %cond_capture_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr null, ptr %cond_capture_0, align 8
  %cond_func_0 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.cond", ptr %cond_func_0, align 8
  %33 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  %34 = load ptr, ptr %33, align 8
  %35 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = call ptr %36(ptr %34, ptr %21)
  ret ptr %37

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %"switch_branch_Tuple#_0"
  %38 = getelementptr { i8, {} }, ptr %26, i32 0, i32 1
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %40 = getelementptr { i8, {} }, ptr %39, i32 0, i32 0
  store i8 0, ptr %40, align 1
  %41 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  %42 = load ptr, ptr %41, align 8
  %43 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  %44 = load ptr, ptr %43, align 8
  %45 = call ptr %44(ptr %42, ptr %39)
  ret ptr %45

switch_default_0:                                 ; preds = %"switch_branch_Tuple#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.concatString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str300)
  ret ptr %10

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$nil_974_0", i32 0, i32 1
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %appendString_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %appendString_capture_0, align 8
  %appendString_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString", ptr %appendString_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %13)
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %concatString_capture_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr null, ptr %concatString_capture_0, align 8
  %concatString_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.concatString", ptr %concatString_func_0, align 8
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %15)
  %28 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %27)
  ret ptr %32

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$case_curry_986"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.case"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_983_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$x_983_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_321", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_322"(ptr %0, ptr %1) {
  %pred_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$dropWhileString_curry_1020"(ptr null, ptr %pred_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.dropWhileString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$pred_991_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %pred_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$pred_991_0", ptr %pred_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_322", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_323"(ptr %0, ptr %1) {
  %pred_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %pred_0 = load ptr, ptr %pred_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$takeWhileString_curry_1085"(ptr null, ptr %pred_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.takeWhileString"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$pred_1051_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %pred_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$pred_1051_0", ptr %pred_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_323", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_324"(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$<|_curry_1124"(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.<|"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$f_1121_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$f_1121_0", ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_324", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_325"(ptr %0, ptr %1) {
  %f_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %f_0 = load ptr, ptr %f_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Prelude.mlg.$._curry_1136"(ptr null, ptr %f_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.."(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$f_1129_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %f_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Prelude.mlg.$f_1129_0", ptr %f_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_325", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.constId"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_87_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_87_0")
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.addOne"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addInt32_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %addInt32_capture_0, align 8
  %addInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %8 = call ptr %6(ptr %4, ptr %7)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0")
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_326"(ptr %0, ptr %1) {
  %printString_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str327)
  %11 = getelementptr { ptr, ptr }, ptr %printString_0, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %printString_0, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  ret ptr %15

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %2
  %16 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %17 = getelementptr { ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, {} }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  ret ptr %19

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.main"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$$__104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %printString_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %printString_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %"String#_0" = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %4, ptr %"String#_0", align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_326", ptr %fun_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %10 = call ptr %8(ptr %6, ptr %9)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %constId_capture_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %constId_capture_0, align 8
  %constId_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.constId", ptr %constId_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addOne_capture_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr null, ptr %addOne_capture_0, align 8
  %addOne_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.addOne", ptr %addOne_func_0, align 8
  %19 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %24 = call ptr %22(ptr %20, ptr %23)
  %25 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr %28(ptr %26, ptr %24)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printInt32_capture_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr null, ptr %printInt32_capture_0, align 8
  %printInt32_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr %34(ptr %32, ptr %29)
  ret ptr %35
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/ToplevelVariable.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/ToplevelVariable.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_332"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @"runtime/malgo/Prelude.mlg.Nothing"(ptr null)
  ret ptr %6

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %2
  %7 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @"runtime/malgo/Prelude.mlg.Nothing"(ptr null)
  ret ptr %10

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal void @"malgo_load_test/testcases/malgo/ToplevelVariable.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_unsafe_cast_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_unsafe_cast_capture_0, align 8
  %malgo_unsafe_cast_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast", ptr %malgo_unsafe_cast_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %1)
  store ptr %8, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %10 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr %13(ptr %11, i32 1)
  store ptr %14, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_332", ptr %fun_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Just_capture_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr null, ptr %Just_capture_0, align 8
  %Just_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Just", ptr %Just_func_0, align 8
  %19 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %16)
  %24 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, ptr %23)
  store ptr %28, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  ret void
}
