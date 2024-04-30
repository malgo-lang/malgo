; ModuleID = 'test/testcases/malgo/TestEither.mlg'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str3549 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str3559 = unnamed_addr constant [6 x i8] c"error\00"

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

define internal ptr @Prelude.putStrLn(ptr %0, ptr %"Prelude.$str_716_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Prelude.$str_716_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_716_0", i32 0, i32 1
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

define internal ptr @Prelude.putStr(ptr %0, ptr %"Prelude.$str_723_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Prelude.$str_723_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_723_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.Int32#"(ptr %0, i32 %"Builtin.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"Builtin.String#"(ptr %0, ptr %"Builtin.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_int32_t_to_string(ptr %0, i32 %"Builtin.$p_2155_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"Builtin.$p_2155_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt32#"(ptr %0, i32 %"Builtin.$x_2179_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"Builtin.$x_2179_0")
  ret ptr %2
}

define internal i32 @"TestEither.#let_closure_3545"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.addInt32#"(ptr %0, i32 %"Builtin.$x_4027_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_4027_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3545", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @TestEither.Left(ptr %0, ptr %"TestEither.$p_249_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"TestEither.$p_249_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @TestEither.Right(ptr %0, ptr %"TestEither.$p_251_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"TestEither.$p_251_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @TestEither.id(ptr %0, ptr %"TestEither.$x_253_0") {
  ret ptr %"TestEither.$x_253_0"
}

define internal ptr @"TestEither.#let_closure_3546"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  ret ptr %8

switch_branch_TestEither.Right_0:                 ; preds = %2
  %11 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %__0, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %__0, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %13)
  ret ptr %18

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @TestEither.andThen(ptr %0, ptr %"TestEither.$__254_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"TestEither.$__254_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3546", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"TestEither.#let_closure_3547"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3548"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr @str3549, ptr %10, align 8
  %11 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, {} }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, {} }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = call ptr @malgo_newline(ptr %19)
  ret ptr %21

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %22 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %23 = getelementptr { ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { i8, <4 x i8> }, ptr %24, i32 0, i32 0
  %26 = load i8, ptr %25, align 1
  switch i8 %26, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %27 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1
  %28 = getelementptr { i32 }, ptr %27, i32 0, i32 0
  %29 = load i32, ptr %28, align 4
  %30 = call ptr @malgo_int32_t_to_string(i32 %29)
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %32 = getelementptr { i8, { ptr } }, ptr %31, i32 0, i32 0
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { ptr } }, ptr %31, i32 0, i32 1, i32 0
  store ptr %30, ptr %33, align 8
  %34 = getelementptr { i8, <8 x i8> }, ptr %31, i32 0, i32 0
  %35 = load i8, ptr %34, align 1
  switch i8 %35, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %36 = getelementptr { i8, { ptr } }, ptr %31, i32 0, i32 1
  %37 = getelementptr { ptr }, ptr %36, i32 0, i32 0
  %38 = load ptr, ptr %37, align 8
  %39 = call ptr @malgo_print_string(ptr %38)
  ret ptr %39

switch_default_1:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_2:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3551"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3552"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %cast_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3550"(ptr %0, ptr %1) {
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %putStr_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %putStr_0 = load ptr, ptr %putStr_addr_0, align 8
  %"toStringInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"toStringInt32#_0" = load ptr, ptr %"toStringInt32#_addr_0", align 8
  %Right_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %Right_0 = load ptr, ptr %Right_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  ret ptr %8

switch_branch_TestEither.Right_0:                 ; preds = %2
  %11 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { i8, <4 x i8> }, ptr %13, i32 0, i32 0
  %15 = load i8, ptr %14, align 1
  switch i8 %15, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %16 = getelementptr { i8, { i32 } }, ptr %13, i32 0, i32 1
  %17 = getelementptr { i32 }, ptr %16, i32 0, i32 0
  %18 = load i32, ptr %17, align 4
  %19 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, i32 %18)
  %24 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, ptr %23)
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %28, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3551", ptr %let_func_0, align 8
  %30 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  %31 = load ptr, ptr %30, align 8
  %32 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr %33(ptr %31, ptr %putStr_0)
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %34, ptr %cast_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3552", ptr %let_func_1, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %Right_0)
  ret ptr %40

switch_default_0:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3553"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3555"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3554"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3555", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3556"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3558"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3557"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3558", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3560"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3562"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3561"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3562", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3563"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3565"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3564"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3565", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3567"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3568"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %cast_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3566"(ptr %0, ptr %1) {
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %putStr_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %putStr_0 = load ptr, ptr %putStr_addr_0, align 8
  %"toStringInt32#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"toStringInt32#_0" = load ptr, ptr %"toStringInt32#_addr_0", align 8
  %Right_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %Right_0 = load ptr, ptr %Right_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  ret ptr %8

switch_branch_TestEither.Right_0:                 ; preds = %2
  %11 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { i8, <4 x i8> }, ptr %13, i32 0, i32 0
  %15 = load i8, ptr %14, align 1
  switch i8 %15, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %16 = getelementptr { i8, { i32 } }, ptr %13, i32 0, i32 1
  %17 = getelementptr { i32 }, ptr %16, i32 0, i32 0
  %18 = load i32, ptr %17, align 4
  %19 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, i32 %18)
  %24 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, ptr %23)
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %28, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3567", ptr %let_func_0, align 8
  %30 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  %31 = load ptr, ptr %30, align 8
  %32 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr %33(ptr %31, ptr %putStr_0)
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %34, ptr %cast_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3568", ptr %let_func_1, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %Right_0)
  ret ptr %40

switch_default_0:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3569"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3571"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3570"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3571", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3572"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3574"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3573"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3574", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3575"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3577"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3576"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3577", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @"TestEither.#let_closure_3578"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#let_closure_3580"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"TestEither.#fun_closure_3579"(ptr %0, ptr %1) {
  %putStrLn_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %putStrLn_0 = load ptr, ptr %putStrLn_addr_0, align 8
  %malgo_int32_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int32_t_to_string_0 = load ptr, ptr %malgo_int32_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_TestEither.Left_0
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, {} }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_newline(ptr %16)
  ret ptr %18

switch_default_0:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_0:                 ; preds = %2
  %19 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { i8, <4 x i8> }, ptr %21, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %24 = getelementptr { i8, { i32 } }, ptr %21, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %malgo_int32_t_to_string_0, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr %30(ptr %28, i32 %26)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %31, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %32, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3580", ptr %let_func_0, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %putStrLn_0)
  ret ptr %40

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @TestEither.main(ptr %0, ptr %"TestEither.$$__268_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %5, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3547", ptr %let_func_0, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3548", ptr %fun_func_0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr %13(ptr %11, ptr %9)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 1, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %15, ptr %20, align 8
  %21 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_16 [
    i8 0, label %switch_branch_TestEither.Left_0
    i8 1, label %switch_branch_TestEither.Right_3
  ]

switch_branch_TestEither.Left_0:                  ; preds = %1
  %23 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %"String#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %30, ptr %"String#_0", align 8
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStr_capture_0 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  store ptr null, ptr %putStr_capture_0, align 8
  %putStr_func_0 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  store ptr @Prelude.putStr, ptr %putStr_func_0, align 8
  %putStr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 1
  store ptr %31, ptr %putStr_0, align 8
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr null, ptr %"toStringInt32#_capture_0", align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %"toStringInt32#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 2
  store ptr %32, ptr %"toStringInt32#_0", align 8
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Right_capture_0 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr null, ptr %Right_capture_0, align 8
  %Right_func_0 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @TestEither.Right, ptr %Right_func_0, align 8
  %Right_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 3
  store ptr %33, ptr %Right_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3550", ptr %let_func_1, align 8
  %34 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = getelementptr { ptr, ptr }, ptr %29, i32 0, i32 1
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr %37(ptr %35, ptr %26)
  %39 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %40 = load i8, ptr %39, align 1
  switch i8 %40, label %switch_default_6 [
    i8 0, label %switch_branch_TestEither.Left_1
    i8 1, label %switch_branch_TestEither.Right_1
  ]

switch_branch_TestEither.Left_1:                  ; preds = %switch_branch_TestEither.Left_0
  %41 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %42 = getelementptr { ptr }, ptr %41, i32 0, i32 0
  %43 = load ptr, ptr %42, align 8
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 0
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr %43, ptr %46, align 8
  %47 = getelementptr { i8, <8 x i8> }, ptr %44, i32 0, i32 0
  %48 = load i8, ptr %47, align 1
  switch i8 %48, label %switch_default_2 [
    i8 0, label %switch_branch_TestEither.Left_2
    i8 1, label %switch_branch_TestEither.Right_0
  ]

switch_branch_TestEither.Left_2:                  ; preds = %switch_branch_TestEither.Left_1
  %49 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 1
  %50 = getelementptr { ptr }, ptr %49, i32 0, i32 0
  %51 = load ptr, ptr %50, align 8
  %52 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %53 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 0
  store i8 0, ptr %53, align 1
  %54 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 1, i32 0
  store ptr %51, ptr %54, align 8
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %52, ptr %d_1, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3553", ptr %let_func_2, align 8
  %56 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %57 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0, align 8
  %putStrLn_0 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 0
  store ptr %57, ptr %putStrLn_0, align 8
  %58 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %58, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_0, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %58, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0, align 8
  %malgo_int32_t_to_string_0 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i32 0, i32 1
  store ptr %58, ptr %malgo_int32_t_to_string_0, align 8
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %56, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %56, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3554", ptr %fun_func_1, align 8
  %59 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  %60 = load ptr, ptr %59, align 8
  %61 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  %62 = load ptr, ptr %61, align 8
  %63 = call ptr %62(ptr %60, ptr %56)
  ret ptr %63

switch_branch_TestEither.Right_0:                 ; preds = %switch_branch_TestEither.Left_1
  %64 = getelementptr { i8, { ptr } }, ptr %44, i32 0, i32 1
  %65 = getelementptr { ptr }, ptr %64, i32 0, i32 0
  %66 = load ptr, ptr %65, align 8
  %67 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %68 = getelementptr { i8, { i32 } }, ptr %67, i32 0, i32 0
  store i8 0, ptr %68, align 1
  %69 = getelementptr { i8, { i32 } }, ptr %67, i32 0, i32 1, i32 0
  store i32 1, ptr %69, align 4
  %70 = getelementptr { i8, <4 x i8> }, ptr %66, i32 0, i32 0
  %71 = load i8, ptr %70, align 1
  switch i8 %71, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_TestEither.Right_0
  %72 = getelementptr { i8, { i32 } }, ptr %66, i32 0, i32 1
  %73 = getelementptr { i32 }, ptr %72, i32 0, i32 0
  %74 = load i32, ptr %73, align 4
  %75 = getelementptr { i8, <4 x i8> }, ptr %67, i32 0, i32 0
  %76 = load i8, ptr %75, align 1
  switch i8 %76, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %77 = getelementptr { i8, { i32 } }, ptr %67, i32 0, i32 1
  %78 = getelementptr { i32 }, ptr %77, i32 0, i32 0
  %79 = load i32, ptr %78, align 4
  %80 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_0", align 8
  %"addInt32#_func_0" = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_0", align 8
  %81 = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 0
  %82 = load ptr, ptr %81, align 8
  %83 = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 1
  %84 = load ptr, ptr %83, align 8
  %85 = call ptr %84(ptr %82, i32 %74)
  %86 = getelementptr { ptr, ptr }, ptr %85, i32 0, i32 0
  %87 = load ptr, ptr %86, align 8
  %88 = getelementptr { ptr, ptr }, ptr %85, i32 0, i32 1
  %89 = load ptr, ptr %88, align 8
  %90 = call i32 %89(ptr %87, i32 %79)
  %91 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0", align 8
  %92 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 0
  %93 = load ptr, ptr %92, align 8
  %94 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 1
  %95 = load ptr, ptr %94, align 8
  %96 = call ptr %95(ptr %93, i32 %90)
  %97 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %98 = getelementptr { i8, { ptr } }, ptr %97, i32 0, i32 0
  store i8 1, ptr %98, align 1
  %99 = getelementptr { i8, { ptr } }, ptr %97, i32 0, i32 1, i32 0
  store ptr %96, ptr %99, align 8
  %100 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %97, ptr %d_2, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3556", ptr %let_func_3, align 8
  %101 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %102 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_1 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_1, align 8
  %putStrLn_func_1 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_1, align 8
  %putStrLn_1 = getelementptr { ptr, ptr }, ptr %fun_capture_4, i32 0, i32 0
  store ptr %102, ptr %putStrLn_1, align 8
  %103 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_1 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_1, align 8
  %malgo_int32_t_to_string_func_1 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_1, align 8
  %malgo_int32_t_to_string_1 = getelementptr { ptr, ptr }, ptr %fun_capture_4, i32 0, i32 1
  store ptr %103, ptr %malgo_int32_t_to_string_1, align 8
  %fun_capture_5 = getelementptr { ptr, ptr }, ptr %101, i32 0, i32 0
  store ptr %fun_capture_4, ptr %fun_capture_5, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %101, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3557", ptr %fun_func_2, align 8
  %104 = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 0
  %105 = load ptr, ptr %104, align 8
  %106 = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 1
  %107 = load ptr, ptr %106, align 8
  %108 = call ptr %107(ptr %105, ptr %101)
  ret ptr %108

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_TestEither.Right_0
  unreachable

switch_default_2:                                 ; preds = %switch_branch_TestEither.Left_1
  unreachable

switch_branch_TestEither.Right_1:                 ; preds = %switch_branch_TestEither.Left_0
  %109 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %110 = getelementptr { ptr }, ptr %109, i32 0, i32 0
  %111 = load ptr, ptr %110, align 8
  %112 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %113 = getelementptr { i8, { ptr } }, ptr %112, i32 0, i32 0
  store i8 0, ptr %113, align 1
  %114 = getelementptr { i8, { ptr } }, ptr %112, i32 0, i32 1, i32 0
  store ptr @str3559, ptr %114, align 8
  %115 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %116 = getelementptr { i8, { ptr } }, ptr %115, i32 0, i32 0
  store i8 0, ptr %116, align 1
  %117 = getelementptr { i8, { ptr } }, ptr %115, i32 0, i32 1, i32 0
  store ptr %112, ptr %117, align 8
  %118 = getelementptr { i8, <8 x i8> }, ptr %115, i32 0, i32 0
  %119 = load i8, ptr %118, align 1
  switch i8 %119, label %switch_default_5 [
    i8 0, label %switch_branch_TestEither.Left_3
    i8 1, label %switch_branch_TestEither.Right_2
  ]

switch_branch_TestEither.Left_3:                  ; preds = %switch_branch_TestEither.Right_1
  %120 = getelementptr { i8, { ptr } }, ptr %115, i32 0, i32 1
  %121 = getelementptr { ptr }, ptr %120, i32 0, i32 0
  %122 = load ptr, ptr %121, align 8
  %123 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %124 = getelementptr { i8, { ptr } }, ptr %123, i32 0, i32 0
  store i8 0, ptr %124, align 1
  %125 = getelementptr { i8, { ptr } }, ptr %123, i32 0, i32 1, i32 0
  store ptr %122, ptr %125, align 8
  %126 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr }, ptr %let_capture_8, i32 0, i32 0
  store ptr %123, ptr %d_3, align 8
  %let_capture_9 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 0
  store ptr %let_capture_8, ptr %let_capture_9, align 8
  %let_func_4 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3560", ptr %let_func_4, align 8
  %127 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %128 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_2 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_2, align 8
  %putStrLn_func_2 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_2, align 8
  %putStrLn_2 = getelementptr { ptr, ptr }, ptr %fun_capture_6, i32 0, i32 0
  store ptr %128, ptr %putStrLn_2, align 8
  %129 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_2 = getelementptr { ptr, ptr }, ptr %129, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_2, align 8
  %malgo_int32_t_to_string_func_2 = getelementptr { ptr, ptr }, ptr %129, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_2, align 8
  %malgo_int32_t_to_string_2 = getelementptr { ptr, ptr }, ptr %fun_capture_6, i32 0, i32 1
  store ptr %129, ptr %malgo_int32_t_to_string_2, align 8
  %fun_capture_7 = getelementptr { ptr, ptr }, ptr %127, i32 0, i32 0
  store ptr %fun_capture_6, ptr %fun_capture_7, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %127, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3561", ptr %fun_func_3, align 8
  %130 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 0
  %131 = load ptr, ptr %130, align 8
  %132 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 1
  %133 = load ptr, ptr %132, align 8
  %134 = call ptr %133(ptr %131, ptr %127)
  ret ptr %134

switch_branch_TestEither.Right_2:                 ; preds = %switch_branch_TestEither.Right_1
  %135 = getelementptr { i8, { ptr } }, ptr %115, i32 0, i32 1
  %136 = getelementptr { ptr }, ptr %135, i32 0, i32 0
  %137 = load ptr, ptr %136, align 8
  %138 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %139 = getelementptr { i8, { i32 } }, ptr %138, i32 0, i32 0
  store i8 0, ptr %139, align 1
  %140 = getelementptr { i8, { i32 } }, ptr %138, i32 0, i32 1, i32 0
  store i32 1, ptr %140, align 4
  %141 = getelementptr { i8, <4 x i8> }, ptr %137, i32 0, i32 0
  %142 = load i8, ptr %141, align 1
  switch i8 %142, label %switch_default_4 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %switch_branch_TestEither.Right_2
  %143 = getelementptr { i8, { i32 } }, ptr %137, i32 0, i32 1
  %144 = getelementptr { i32 }, ptr %143, i32 0, i32 0
  %145 = load i32, ptr %144, align 4
  %146 = getelementptr { i8, <4 x i8> }, ptr %138, i32 0, i32 0
  %147 = load i8, ptr %146, align 1
  switch i8 %147, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.Int32#_3"
  ]

"switch_branch_Builtin.Int32#_3":                 ; preds = %"switch_branch_Builtin.Int32#_2"
  %148 = getelementptr { i8, { i32 } }, ptr %138, i32 0, i32 1
  %149 = getelementptr { i32 }, ptr %148, i32 0, i32 0
  %150 = load i32, ptr %149, align 4
  %151 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_1" = getelementptr { ptr, ptr }, ptr %151, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_1", align 8
  %"addInt32#_func_1" = getelementptr { ptr, ptr }, ptr %151, i32 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_1", align 8
  %152 = getelementptr { ptr, ptr }, ptr %151, i32 0, i32 0
  %153 = load ptr, ptr %152, align 8
  %154 = getelementptr { ptr, ptr }, ptr %151, i32 0, i32 1
  %155 = load ptr, ptr %154, align 8
  %156 = call ptr %155(ptr %153, i32 %145)
  %157 = getelementptr { ptr, ptr }, ptr %156, i32 0, i32 0
  %158 = load ptr, ptr %157, align 8
  %159 = getelementptr { ptr, ptr }, ptr %156, i32 0, i32 1
  %160 = load ptr, ptr %159, align 8
  %161 = call i32 %160(ptr %158, i32 %150)
  %162 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_1" = getelementptr { ptr, ptr }, ptr %162, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_1", align 8
  %"Int32#_func_1" = getelementptr { ptr, ptr }, ptr %162, i32 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_1", align 8
  %163 = getelementptr { ptr, ptr }, ptr %162, i32 0, i32 0
  %164 = load ptr, ptr %163, align 8
  %165 = getelementptr { ptr, ptr }, ptr %162, i32 0, i32 1
  %166 = load ptr, ptr %165, align 8
  %167 = call ptr %166(ptr %164, i32 %161)
  %168 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %169 = getelementptr { i8, { ptr } }, ptr %168, i32 0, i32 0
  store i8 1, ptr %169, align 1
  %170 = getelementptr { i8, { ptr } }, ptr %168, i32 0, i32 1, i32 0
  store ptr %167, ptr %170, align 8
  %171 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_4 = getelementptr { ptr }, ptr %let_capture_10, i32 0, i32 0
  store ptr %168, ptr %d_4, align 8
  %let_capture_11 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 0
  store ptr %let_capture_10, ptr %let_capture_11, align 8
  %let_func_5 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3563", ptr %let_func_5, align 8
  %172 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %173 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_3 = getelementptr { ptr, ptr }, ptr %173, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_3, align 8
  %putStrLn_func_3 = getelementptr { ptr, ptr }, ptr %173, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_3, align 8
  %putStrLn_3 = getelementptr { ptr, ptr }, ptr %fun_capture_8, i32 0, i32 0
  store ptr %173, ptr %putStrLn_3, align 8
  %174 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_3 = getelementptr { ptr, ptr }, ptr %174, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_3, align 8
  %malgo_int32_t_to_string_func_3 = getelementptr { ptr, ptr }, ptr %174, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_3, align 8
  %malgo_int32_t_to_string_3 = getelementptr { ptr, ptr }, ptr %fun_capture_8, i32 0, i32 1
  store ptr %174, ptr %malgo_int32_t_to_string_3, align 8
  %fun_capture_9 = getelementptr { ptr, ptr }, ptr %172, i32 0, i32 0
  store ptr %fun_capture_8, ptr %fun_capture_9, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %172, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3564", ptr %fun_func_4, align 8
  %175 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 0
  %176 = load ptr, ptr %175, align 8
  %177 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 1
  %178 = load ptr, ptr %177, align 8
  %179 = call ptr %178(ptr %176, ptr %172)
  ret ptr %179

switch_default_3:                                 ; preds = %"switch_branch_Builtin.Int32#_2"
  unreachable

switch_default_4:                                 ; preds = %switch_branch_TestEither.Right_2
  unreachable

switch_default_5:                                 ; preds = %switch_branch_TestEither.Right_1
  unreachable

switch_default_6:                                 ; preds = %switch_branch_TestEither.Left_0
  unreachable

switch_branch_TestEither.Right_3:                 ; preds = %1
  %180 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %181 = getelementptr { ptr }, ptr %180, i32 0, i32 0
  %182 = load ptr, ptr %181, align 8
  %183 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %184 = getelementptr { i8, { i32 } }, ptr %183, i32 0, i32 0
  store i8 0, ptr %184, align 1
  %185 = getelementptr { i8, { i32 } }, ptr %183, i32 0, i32 1, i32 0
  store i32 1, ptr %185, align 4
  %186 = getelementptr { i8, <4 x i8> }, ptr %182, i32 0, i32 0
  %187 = load i8, ptr %186, align 1
  switch i8 %187, label %switch_default_15 [
    i8 0, label %"switch_branch_Builtin.Int32#_4"
  ]

"switch_branch_Builtin.Int32#_4":                 ; preds = %switch_branch_TestEither.Right_3
  %188 = getelementptr { i8, { i32 } }, ptr %182, i32 0, i32 1
  %189 = getelementptr { i32 }, ptr %188, i32 0, i32 0
  %190 = load i32, ptr %189, align 4
  %191 = getelementptr { i8, <4 x i8> }, ptr %183, i32 0, i32 0
  %192 = load i8, ptr %191, align 1
  switch i8 %192, label %switch_default_14 [
    i8 0, label %"switch_branch_Builtin.Int32#_5"
  ]

"switch_branch_Builtin.Int32#_5":                 ; preds = %"switch_branch_Builtin.Int32#_4"
  %193 = getelementptr { i8, { i32 } }, ptr %183, i32 0, i32 1
  %194 = getelementptr { i32 }, ptr %193, i32 0, i32 0
  %195 = load i32, ptr %194, align 4
  %196 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_2" = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_2", align 8
  %"addInt32#_func_2" = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_2", align 8
  %197 = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 0
  %198 = load ptr, ptr %197, align 8
  %199 = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 1
  %200 = load ptr, ptr %199, align 8
  %201 = call ptr %200(ptr %198, i32 %190)
  %202 = getelementptr { ptr, ptr }, ptr %201, i32 0, i32 0
  %203 = load ptr, ptr %202, align 8
  %204 = getelementptr { ptr, ptr }, ptr %201, i32 0, i32 1
  %205 = load ptr, ptr %204, align 8
  %206 = call i32 %205(ptr %203, i32 %195)
  %207 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_2" = getelementptr { ptr, ptr }, ptr %207, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_2", align 8
  %"Int32#_func_2" = getelementptr { ptr, ptr }, ptr %207, i32 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_2", align 8
  %208 = getelementptr { ptr, ptr }, ptr %207, i32 0, i32 0
  %209 = load ptr, ptr %208, align 8
  %210 = getelementptr { ptr, ptr }, ptr %207, i32 0, i32 1
  %211 = load ptr, ptr %210, align 8
  %212 = call ptr %211(ptr %209, i32 %206)
  %213 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %214 = getelementptr { i8, { ptr } }, ptr %213, i32 0, i32 0
  store i8 1, ptr %214, align 1
  %215 = getelementptr { i8, { ptr } }, ptr %213, i32 0, i32 1, i32 0
  store ptr %212, ptr %215, align 8
  %216 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %217 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_1" = getelementptr { ptr, ptr }, ptr %217, i32 0, i32 0
  store ptr null, ptr %"String#_capture_1", align 8
  %"String#_func_1" = getelementptr { ptr, ptr }, ptr %217, i32 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_1", align 8
  %"String#_1" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12, i32 0, i32 0
  store ptr %217, ptr %"String#_1", align 8
  %218 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStr_capture_1 = getelementptr { ptr, ptr }, ptr %218, i32 0, i32 0
  store ptr null, ptr %putStr_capture_1, align 8
  %putStr_func_1 = getelementptr { ptr, ptr }, ptr %218, i32 0, i32 1
  store ptr @Prelude.putStr, ptr %putStr_func_1, align 8
  %putStr_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12, i32 0, i32 1
  store ptr %218, ptr %putStr_1, align 8
  %219 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringInt32#_capture_1" = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 0
  store ptr null, ptr %"toStringInt32#_capture_1", align 8
  %"toStringInt32#_func_1" = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_1", align 8
  %"toStringInt32#_1" = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12, i32 0, i32 2
  store ptr %219, ptr %"toStringInt32#_1", align 8
  %220 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Right_capture_1 = getelementptr { ptr, ptr }, ptr %220, i32 0, i32 0
  store ptr null, ptr %Right_capture_1, align 8
  %Right_func_1 = getelementptr { ptr, ptr }, ptr %220, i32 0, i32 1
  store ptr @TestEither.Right, ptr %Right_func_1, align 8
  %Right_1 = getelementptr { ptr, ptr, ptr, ptr }, ptr %let_capture_12, i32 0, i32 3
  store ptr %220, ptr %Right_1, align 8
  %let_capture_13 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 0
  store ptr %let_capture_12, ptr %let_capture_13, align 8
  %let_func_6 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3566", ptr %let_func_6, align 8
  %221 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 0
  %222 = load ptr, ptr %221, align 8
  %223 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 1
  %224 = load ptr, ptr %223, align 8
  %225 = call ptr %224(ptr %222, ptr %213)
  %226 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %227 = load i8, ptr %226, align 1
  switch i8 %227, label %switch_default_13 [
    i8 0, label %switch_branch_TestEither.Left_4
    i8 1, label %switch_branch_TestEither.Right_5
  ]

switch_branch_TestEither.Left_4:                  ; preds = %"switch_branch_Builtin.Int32#_5"
  %228 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %229 = getelementptr { ptr }, ptr %228, i32 0, i32 0
  %230 = load ptr, ptr %229, align 8
  %231 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %232 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 0
  store i8 0, ptr %232, align 1
  %233 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 1, i32 0
  store ptr %230, ptr %233, align 8
  %234 = getelementptr { i8, <8 x i8> }, ptr %231, i32 0, i32 0
  %235 = load i8, ptr %234, align 1
  switch i8 %235, label %switch_default_9 [
    i8 0, label %switch_branch_TestEither.Left_5
    i8 1, label %switch_branch_TestEither.Right_4
  ]

switch_branch_TestEither.Left_5:                  ; preds = %switch_branch_TestEither.Left_4
  %236 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 1
  %237 = getelementptr { ptr }, ptr %236, i32 0, i32 0
  %238 = load ptr, ptr %237, align 8
  %239 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %240 = getelementptr { i8, { ptr } }, ptr %239, i32 0, i32 0
  store i8 0, ptr %240, align 1
  %241 = getelementptr { i8, { ptr } }, ptr %239, i32 0, i32 1, i32 0
  store ptr %238, ptr %241, align 8
  %242 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_5 = getelementptr { ptr }, ptr %let_capture_14, i32 0, i32 0
  store ptr %239, ptr %d_5, align 8
  %let_capture_15 = getelementptr { ptr, ptr }, ptr %242, i32 0, i32 0
  store ptr %let_capture_14, ptr %let_capture_15, align 8
  %let_func_7 = getelementptr { ptr, ptr }, ptr %242, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3569", ptr %let_func_7, align 8
  %243 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %244 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_4 = getelementptr { ptr, ptr }, ptr %244, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_4, align 8
  %putStrLn_func_4 = getelementptr { ptr, ptr }, ptr %244, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_4, align 8
  %putStrLn_4 = getelementptr { ptr, ptr }, ptr %fun_capture_10, i32 0, i32 0
  store ptr %244, ptr %putStrLn_4, align 8
  %245 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_4 = getelementptr { ptr, ptr }, ptr %245, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_4, align 8
  %malgo_int32_t_to_string_func_4 = getelementptr { ptr, ptr }, ptr %245, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_4, align 8
  %malgo_int32_t_to_string_4 = getelementptr { ptr, ptr }, ptr %fun_capture_10, i32 0, i32 1
  store ptr %245, ptr %malgo_int32_t_to_string_4, align 8
  %fun_capture_11 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 0
  store ptr %fun_capture_10, ptr %fun_capture_11, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3570", ptr %fun_func_5, align 8
  %246 = getelementptr { ptr, ptr }, ptr %242, i32 0, i32 0
  %247 = load ptr, ptr %246, align 8
  %248 = getelementptr { ptr, ptr }, ptr %242, i32 0, i32 1
  %249 = load ptr, ptr %248, align 8
  %250 = call ptr %249(ptr %247, ptr %243)
  ret ptr %250

switch_branch_TestEither.Right_4:                 ; preds = %switch_branch_TestEither.Left_4
  %251 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 1
  %252 = getelementptr { ptr }, ptr %251, i32 0, i32 0
  %253 = load ptr, ptr %252, align 8
  %254 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %255 = getelementptr { i8, { i32 } }, ptr %254, i32 0, i32 0
  store i8 0, ptr %255, align 1
  %256 = getelementptr { i8, { i32 } }, ptr %254, i32 0, i32 1, i32 0
  store i32 1, ptr %256, align 4
  %257 = getelementptr { i8, <4 x i8> }, ptr %253, i32 0, i32 0
  %258 = load i8, ptr %257, align 1
  switch i8 %258, label %switch_default_8 [
    i8 0, label %"switch_branch_Builtin.Int32#_6"
  ]

"switch_branch_Builtin.Int32#_6":                 ; preds = %switch_branch_TestEither.Right_4
  %259 = getelementptr { i8, { i32 } }, ptr %253, i32 0, i32 1
  %260 = getelementptr { i32 }, ptr %259, i32 0, i32 0
  %261 = load i32, ptr %260, align 4
  %262 = getelementptr { i8, <4 x i8> }, ptr %254, i32 0, i32 0
  %263 = load i8, ptr %262, align 1
  switch i8 %263, label %switch_default_7 [
    i8 0, label %"switch_branch_Builtin.Int32#_7"
  ]

"switch_branch_Builtin.Int32#_7":                 ; preds = %"switch_branch_Builtin.Int32#_6"
  %264 = getelementptr { i8, { i32 } }, ptr %254, i32 0, i32 1
  %265 = getelementptr { i32 }, ptr %264, i32 0, i32 0
  %266 = load i32, ptr %265, align 4
  %267 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_3" = getelementptr { ptr, ptr }, ptr %267, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_3", align 8
  %"addInt32#_func_3" = getelementptr { ptr, ptr }, ptr %267, i32 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_3", align 8
  %268 = getelementptr { ptr, ptr }, ptr %267, i32 0, i32 0
  %269 = load ptr, ptr %268, align 8
  %270 = getelementptr { ptr, ptr }, ptr %267, i32 0, i32 1
  %271 = load ptr, ptr %270, align 8
  %272 = call ptr %271(ptr %269, i32 %261)
  %273 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 0
  %274 = load ptr, ptr %273, align 8
  %275 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 1
  %276 = load ptr, ptr %275, align 8
  %277 = call i32 %276(ptr %274, i32 %266)
  %278 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_3" = getelementptr { ptr, ptr }, ptr %278, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_3", align 8
  %"Int32#_func_3" = getelementptr { ptr, ptr }, ptr %278, i32 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_3", align 8
  %279 = getelementptr { ptr, ptr }, ptr %278, i32 0, i32 0
  %280 = load ptr, ptr %279, align 8
  %281 = getelementptr { ptr, ptr }, ptr %278, i32 0, i32 1
  %282 = load ptr, ptr %281, align 8
  %283 = call ptr %282(ptr %280, i32 %277)
  %284 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %285 = getelementptr { i8, { ptr } }, ptr %284, i32 0, i32 0
  store i8 1, ptr %285, align 1
  %286 = getelementptr { i8, { ptr } }, ptr %284, i32 0, i32 1, i32 0
  store ptr %283, ptr %286, align 8
  %287 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_6 = getelementptr { ptr }, ptr %let_capture_16, i32 0, i32 0
  store ptr %284, ptr %d_6, align 8
  %let_capture_17 = getelementptr { ptr, ptr }, ptr %287, i32 0, i32 0
  store ptr %let_capture_16, ptr %let_capture_17, align 8
  %let_func_8 = getelementptr { ptr, ptr }, ptr %287, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3572", ptr %let_func_8, align 8
  %288 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %289 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_5 = getelementptr { ptr, ptr }, ptr %289, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_5, align 8
  %putStrLn_func_5 = getelementptr { ptr, ptr }, ptr %289, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_5, align 8
  %putStrLn_5 = getelementptr { ptr, ptr }, ptr %fun_capture_12, i32 0, i32 0
  store ptr %289, ptr %putStrLn_5, align 8
  %290 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_5 = getelementptr { ptr, ptr }, ptr %290, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_5, align 8
  %malgo_int32_t_to_string_func_5 = getelementptr { ptr, ptr }, ptr %290, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_5, align 8
  %malgo_int32_t_to_string_5 = getelementptr { ptr, ptr }, ptr %fun_capture_12, i32 0, i32 1
  store ptr %290, ptr %malgo_int32_t_to_string_5, align 8
  %fun_capture_13 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 0
  store ptr %fun_capture_12, ptr %fun_capture_13, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3573", ptr %fun_func_6, align 8
  %291 = getelementptr { ptr, ptr }, ptr %287, i32 0, i32 0
  %292 = load ptr, ptr %291, align 8
  %293 = getelementptr { ptr, ptr }, ptr %287, i32 0, i32 1
  %294 = load ptr, ptr %293, align 8
  %295 = call ptr %294(ptr %292, ptr %288)
  ret ptr %295

switch_default_7:                                 ; preds = %"switch_branch_Builtin.Int32#_6"
  unreachable

switch_default_8:                                 ; preds = %switch_branch_TestEither.Right_4
  unreachable

switch_default_9:                                 ; preds = %switch_branch_TestEither.Left_4
  unreachable

switch_branch_TestEither.Right_5:                 ; preds = %"switch_branch_Builtin.Int32#_5"
  %296 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %297 = getelementptr { ptr }, ptr %296, i32 0, i32 0
  %298 = load ptr, ptr %297, align 8
  %299 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %300 = getelementptr { i8, { ptr } }, ptr %299, i32 0, i32 0
  store i8 0, ptr %300, align 1
  %301 = getelementptr { i8, { ptr } }, ptr %299, i32 0, i32 1, i32 0
  store ptr @str3559, ptr %301, align 8
  %302 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %303 = getelementptr { i8, { ptr } }, ptr %302, i32 0, i32 0
  store i8 0, ptr %303, align 1
  %304 = getelementptr { i8, { ptr } }, ptr %302, i32 0, i32 1, i32 0
  store ptr %299, ptr %304, align 8
  %305 = getelementptr { i8, <8 x i8> }, ptr %302, i32 0, i32 0
  %306 = load i8, ptr %305, align 1
  switch i8 %306, label %switch_default_12 [
    i8 0, label %switch_branch_TestEither.Left_6
    i8 1, label %switch_branch_TestEither.Right_6
  ]

switch_branch_TestEither.Left_6:                  ; preds = %switch_branch_TestEither.Right_5
  %307 = getelementptr { i8, { ptr } }, ptr %302, i32 0, i32 1
  %308 = getelementptr { ptr }, ptr %307, i32 0, i32 0
  %309 = load ptr, ptr %308, align 8
  %310 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %311 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 0
  store i8 0, ptr %311, align 1
  %312 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 1, i32 0
  store ptr %309, ptr %312, align 8
  %313 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_7 = getelementptr { ptr }, ptr %let_capture_18, i32 0, i32 0
  store ptr %310, ptr %d_7, align 8
  %let_capture_19 = getelementptr { ptr, ptr }, ptr %313, i32 0, i32 0
  store ptr %let_capture_18, ptr %let_capture_19, align 8
  %let_func_9 = getelementptr { ptr, ptr }, ptr %313, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3575", ptr %let_func_9, align 8
  %314 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %315 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_6 = getelementptr { ptr, ptr }, ptr %315, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_6, align 8
  %putStrLn_func_6 = getelementptr { ptr, ptr }, ptr %315, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_6, align 8
  %putStrLn_6 = getelementptr { ptr, ptr }, ptr %fun_capture_14, i32 0, i32 0
  store ptr %315, ptr %putStrLn_6, align 8
  %316 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_6 = getelementptr { ptr, ptr }, ptr %316, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_6, align 8
  %malgo_int32_t_to_string_func_6 = getelementptr { ptr, ptr }, ptr %316, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_6, align 8
  %malgo_int32_t_to_string_6 = getelementptr { ptr, ptr }, ptr %fun_capture_14, i32 0, i32 1
  store ptr %316, ptr %malgo_int32_t_to_string_6, align 8
  %fun_capture_15 = getelementptr { ptr, ptr }, ptr %314, i32 0, i32 0
  store ptr %fun_capture_14, ptr %fun_capture_15, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %314, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3576", ptr %fun_func_7, align 8
  %317 = getelementptr { ptr, ptr }, ptr %313, i32 0, i32 0
  %318 = load ptr, ptr %317, align 8
  %319 = getelementptr { ptr, ptr }, ptr %313, i32 0, i32 1
  %320 = load ptr, ptr %319, align 8
  %321 = call ptr %320(ptr %318, ptr %314)
  ret ptr %321

switch_branch_TestEither.Right_6:                 ; preds = %switch_branch_TestEither.Right_5
  %322 = getelementptr { i8, { ptr } }, ptr %302, i32 0, i32 1
  %323 = getelementptr { ptr }, ptr %322, i32 0, i32 0
  %324 = load ptr, ptr %323, align 8
  %325 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %326 = getelementptr { i8, { i32 } }, ptr %325, i32 0, i32 0
  store i8 0, ptr %326, align 1
  %327 = getelementptr { i8, { i32 } }, ptr %325, i32 0, i32 1, i32 0
  store i32 1, ptr %327, align 4
  %328 = getelementptr { i8, <4 x i8> }, ptr %324, i32 0, i32 0
  %329 = load i8, ptr %328, align 1
  switch i8 %329, label %switch_default_11 [
    i8 0, label %"switch_branch_Builtin.Int32#_8"
  ]

"switch_branch_Builtin.Int32#_8":                 ; preds = %switch_branch_TestEither.Right_6
  %330 = getelementptr { i8, { i32 } }, ptr %324, i32 0, i32 1
  %331 = getelementptr { i32 }, ptr %330, i32 0, i32 0
  %332 = load i32, ptr %331, align 4
  %333 = getelementptr { i8, <4 x i8> }, ptr %325, i32 0, i32 0
  %334 = load i8, ptr %333, align 1
  switch i8 %334, label %switch_default_10 [
    i8 0, label %"switch_branch_Builtin.Int32#_9"
  ]

"switch_branch_Builtin.Int32#_9":                 ; preds = %"switch_branch_Builtin.Int32#_8"
  %335 = getelementptr { i8, { i32 } }, ptr %325, i32 0, i32 1
  %336 = getelementptr { i32 }, ptr %335, i32 0, i32 0
  %337 = load i32, ptr %336, align 4
  %338 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_4" = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_4", align 8
  %"addInt32#_func_4" = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_4", align 8
  %339 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 0
  %340 = load ptr, ptr %339, align 8
  %341 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 1
  %342 = load ptr, ptr %341, align 8
  %343 = call ptr %342(ptr %340, i32 %332)
  %344 = getelementptr { ptr, ptr }, ptr %343, i32 0, i32 0
  %345 = load ptr, ptr %344, align 8
  %346 = getelementptr { ptr, ptr }, ptr %343, i32 0, i32 1
  %347 = load ptr, ptr %346, align 8
  %348 = call i32 %347(ptr %345, i32 %337)
  %349 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_4" = getelementptr { ptr, ptr }, ptr %349, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_4", align 8
  %"Int32#_func_4" = getelementptr { ptr, ptr }, ptr %349, i32 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_4", align 8
  %350 = getelementptr { ptr, ptr }, ptr %349, i32 0, i32 0
  %351 = load ptr, ptr %350, align 8
  %352 = getelementptr { ptr, ptr }, ptr %349, i32 0, i32 1
  %353 = load ptr, ptr %352, align 8
  %354 = call ptr %353(ptr %351, i32 %348)
  %355 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %356 = getelementptr { i8, { ptr } }, ptr %355, i32 0, i32 0
  store i8 1, ptr %356, align 1
  %357 = getelementptr { i8, { ptr } }, ptr %355, i32 0, i32 1, i32 0
  store ptr %354, ptr %357, align 8
  %358 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_8 = getelementptr { ptr }, ptr %let_capture_20, i32 0, i32 0
  store ptr %355, ptr %d_8, align 8
  %let_capture_21 = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 0
  store ptr %let_capture_20, ptr %let_capture_21, align 8
  %let_func_10 = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 1
  store ptr @"TestEither.#let_closure_3578", ptr %let_func_10, align 8
  %359 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %360 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_7 = getelementptr { ptr, ptr }, ptr %360, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_7, align 8
  %putStrLn_func_7 = getelementptr { ptr, ptr }, ptr %360, i32 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_7, align 8
  %putStrLn_7 = getelementptr { ptr, ptr }, ptr %fun_capture_16, i32 0, i32 0
  store ptr %360, ptr %putStrLn_7, align 8
  %361 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_7 = getelementptr { ptr, ptr }, ptr %361, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_7, align 8
  %malgo_int32_t_to_string_func_7 = getelementptr { ptr, ptr }, ptr %361, i32 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_7, align 8
  %malgo_int32_t_to_string_7 = getelementptr { ptr, ptr }, ptr %fun_capture_16, i32 0, i32 1
  store ptr %361, ptr %malgo_int32_t_to_string_7, align 8
  %fun_capture_17 = getelementptr { ptr, ptr }, ptr %359, i32 0, i32 0
  store ptr %fun_capture_16, ptr %fun_capture_17, align 8
  %fun_func_8 = getelementptr { ptr, ptr }, ptr %359, i32 0, i32 1
  store ptr @"TestEither.#fun_closure_3579", ptr %fun_func_8, align 8
  %362 = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 0
  %363 = load ptr, ptr %362, align 8
  %364 = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 1
  %365 = load ptr, ptr %364, align 8
  %366 = call ptr %365(ptr %363, ptr %359)
  ret ptr %366

switch_default_10:                                ; preds = %"switch_branch_Builtin.Int32#_8"
  unreachable

switch_default_11:                                ; preds = %switch_branch_TestEither.Right_6
  unreachable

switch_default_12:                                ; preds = %switch_branch_TestEither.Right_5
  unreachable

switch_default_13:                                ; preds = %"switch_branch_Builtin.Int32#_5"
  unreachable

switch_default_14:                                ; preds = %"switch_branch_Builtin.Int32#_4"
  unreachable

switch_default_15:                                ; preds = %switch_branch_TestEither.Right_3
  unreachable

switch_default_16:                                ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_TestEither()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @TestEither.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_TestEither() {
  ret void
}
