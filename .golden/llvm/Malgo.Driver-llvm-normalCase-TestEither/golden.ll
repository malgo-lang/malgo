; ModuleID = 'test/testcases/malgo/TestEither.mlg'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str3744 = unnamed_addr constant [6 x i8] c"error\00"
@str3771 = unnamed_addr constant [12 x i8] c"unreachable\00"

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

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3697"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3666"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3697", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3698"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3634"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3698", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3699"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3667"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3699", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3700"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3635"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3700", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3701"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_let_3545"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.andThen"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$__254_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$__254_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3701", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3702"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3664"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3702", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3703"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3696"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3703", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3704"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3632"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3704", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3705"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3665"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3705", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3706"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3633"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3706", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3707"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3670"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3707", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3708"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3638"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3708", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3709"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3710"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %cast_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$let_3192"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_3193_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_3193_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_3193_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  ret ptr %7

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_3193_0", i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { i8, <4 x i8> }, ptr %12, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %15 = getelementptr { i8, { i32 } }, ptr %12, i32 0, i32 1
  %16 = getelementptr { i32 }, ptr %15, i32 0, i32 0
  %17 = load i32, ptr %16, align 4
  %18 = call ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr null, i32 %17)
  %19 = call ptr @"runtime/malgo/Builtin.mlg.String#"(ptr null, ptr %18)
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %19, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3709", ptr %let_func_0, align 8
  %21 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStr_capture_0 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr null, ptr %putStr_capture_0, align 8
  %putStr_func_0 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_0, align 8
  %26 = call ptr %24(ptr %22, ptr %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %26, ptr %cast_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3710", ptr %let_func_1, align 8
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Right_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %Right_capture_0, align 8
  %Right_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_0, align 8
  %29 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %28)
  ret ptr %33

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3711"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3671"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3711", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3712"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3639"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3712", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.Left"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$p_249_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_249_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3713"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3668"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3713", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_let_3545"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$__254_0", ptr %"test/testcases/malgo/TestEither.mlg.$left_255_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_255_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_255_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  ret ptr %7

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_255_0", i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"test/testcases/malgo/TestEither.mlg.$__254_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"test/testcases/malgo/TestEither.mlg.$__254_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3714"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3636"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3714", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3715"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3669"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3715", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3716"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3637"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3716", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.Right"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$p_251_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_251_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3717"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3674"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3717", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3718"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3642"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3718", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$x_2179_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3719"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3675"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3719", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3720"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3643"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3720", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/TestEither.mlg.#let_closure_3721"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3721", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3722"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3672"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3722", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3723"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3640"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3723", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3724"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3673"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3724", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3725"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3641"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3725", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i32 0, i32 1
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

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3726"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3678"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3726", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3727"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3646"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3727", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3728"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3679"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3728", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3729"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3647"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3729", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3730"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3676"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3730", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3731"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3644"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3731", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3732"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3677"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3732", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3733"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3645"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3733", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3734"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3650"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3734", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3735"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3682"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3735", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3736"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3651"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3736", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3737"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3683"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3737", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3738"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3739"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3621"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3740"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3741"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3622"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3742"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3743"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3623"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3745"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3746"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3627"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3747"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3748"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3628"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3749"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3750"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3637"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3751"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3752"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3638"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3753"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3754"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3642"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3755"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3756"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3643"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.main"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$$__268_0") {
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
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3738", ptr %let_func_0, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3739", ptr %fun_func_0, align 8
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
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_3"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %23 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = call ptr @"test/testcases/malgo/TestEither.mlg.$let_3192"(ptr null, ptr %26)
  %30 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %31 = load i8, ptr %30, align 1
  switch i8 %31, label %switch_default_6 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_1"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_1"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_1": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %32 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %33 = getelementptr { ptr }, ptr %32, i32 0, i32 0
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %36 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 0
  store i8 0, ptr %36, align 1
  %37 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 1, i32 0
  store ptr %34, ptr %37, align 8
  %38 = getelementptr { i8, <8 x i8> }, ptr %35, i32 0, i32 0
  %39 = load i8, ptr %38, align 1
  switch i8 %39, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_2"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_2": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_1"
  %40 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 1
  %41 = getelementptr { ptr }, ptr %40, i32 0, i32 0
  %42 = load ptr, ptr %41, align 8
  %43 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %44 = getelementptr { i8, { ptr } }, ptr %43, i32 0, i32 0
  store i8 0, ptr %44, align 1
  %45 = getelementptr { i8, { ptr } }, ptr %43, i32 0, i32 1, i32 0
  store ptr %42, ptr %45, align 8
  %46 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %43, ptr %d_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %46, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %46, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3740", ptr %let_func_1, align 8
  %47 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3741", ptr %fun_func_1, align 8
  %48 = getelementptr { ptr, ptr }, ptr %46, i32 0, i32 0
  %49 = load ptr, ptr %48, align 8
  %50 = getelementptr { ptr, ptr }, ptr %46, i32 0, i32 1
  %51 = load ptr, ptr %50, align 8
  %52 = call ptr %51(ptr %49, ptr %47)
  ret ptr %52

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_1"
  %53 = getelementptr { i8, { ptr } }, ptr %35, i32 0, i32 1
  %54 = getelementptr { ptr }, ptr %53, i32 0, i32 0
  %55 = load ptr, ptr %54, align 8
  %56 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %57 = getelementptr { i8, { i32 } }, ptr %56, i32 0, i32 0
  store i8 0, ptr %57, align 1
  %58 = getelementptr { i8, { i32 } }, ptr %56, i32 0, i32 1, i32 0
  store i32 1, ptr %58, align 4
  %59 = getelementptr { i8, <4 x i8> }, ptr %55, i32 0, i32 0
  %60 = load i8, ptr %59, align 1
  switch i8 %60, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %61 = getelementptr { i8, { i32 } }, ptr %55, i32 0, i32 1
  %62 = getelementptr { i32 }, ptr %61, i32 0, i32 0
  %63 = load i32, ptr %62, align 4
  %64 = getelementptr { i8, <4 x i8> }, ptr %56, i32 0, i32 0
  %65 = load i8, ptr %64, align 1
  switch i8 %65, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %66 = getelementptr { i8, { i32 } }, ptr %56, i32 0, i32 1
  %67 = getelementptr { i32 }, ptr %66, i32 0, i32 0
  %68 = load i32, ptr %67, align 4
  %69 = call ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr null, i32 %63)
  %70 = getelementptr { ptr, ptr }, ptr %69, i32 0, i32 0
  %71 = load ptr, ptr %70, align 8
  %72 = getelementptr { ptr, ptr }, ptr %69, i32 0, i32 1
  %73 = load ptr, ptr %72, align 8
  %74 = call i32 %73(ptr %71, i32 %68)
  %75 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %74)
  %76 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %77 = getelementptr { i8, { ptr } }, ptr %76, i32 0, i32 0
  store i8 1, ptr %77, align 1
  %78 = getelementptr { i8, { ptr } }, ptr %76, i32 0, i32 1, i32 0
  store ptr %75, ptr %78, align 8
  %79 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %76, ptr %d_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3742", ptr %let_func_2, align 8
  %80 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_5 = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 0
  store ptr %fun_capture_4, ptr %fun_capture_5, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %80, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3743", ptr %fun_func_2, align 8
  %81 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 0
  %82 = load ptr, ptr %81, align 8
  %83 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 1
  %84 = load ptr, ptr %83, align 8
  %85 = call ptr %84(ptr %82, ptr %80)
  ret ptr %85

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_1"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_1": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %86 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %87 = getelementptr { ptr }, ptr %86, i32 0, i32 0
  %88 = load ptr, ptr %87, align 8
  %89 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %90 = getelementptr { i8, { ptr } }, ptr %89, i32 0, i32 0
  store i8 0, ptr %90, align 1
  %91 = getelementptr { i8, { ptr } }, ptr %89, i32 0, i32 1, i32 0
  store ptr @str3744, ptr %91, align 8
  %92 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %93 = getelementptr { i8, { ptr } }, ptr %92, i32 0, i32 0
  store i8 0, ptr %93, align 1
  %94 = getelementptr { i8, { ptr } }, ptr %92, i32 0, i32 1, i32 0
  store ptr %89, ptr %94, align 8
  %95 = getelementptr { i8, <8 x i8> }, ptr %92, i32 0, i32 0
  %96 = load i8, ptr %95, align 1
  switch i8 %96, label %switch_default_5 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_3"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_2"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_3": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_1"
  %97 = getelementptr { i8, { ptr } }, ptr %92, i32 0, i32 1
  %98 = getelementptr { ptr }, ptr %97, i32 0, i32 0
  %99 = load ptr, ptr %98, align 8
  %100 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %101 = getelementptr { i8, { ptr } }, ptr %100, i32 0, i32 0
  store i8 0, ptr %101, align 1
  %102 = getelementptr { i8, { ptr } }, ptr %100, i32 0, i32 1, i32 0
  store ptr %99, ptr %102, align 8
  %103 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %100, ptr %d_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3745", ptr %let_func_3, align 8
  %104 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_7 = getelementptr { ptr, ptr }, ptr %104, i32 0, i32 0
  store ptr %fun_capture_6, ptr %fun_capture_7, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %104, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3746", ptr %fun_func_3, align 8
  %105 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 0
  %106 = load ptr, ptr %105, align 8
  %107 = getelementptr { ptr, ptr }, ptr %103, i32 0, i32 1
  %108 = load ptr, ptr %107, align 8
  %109 = call ptr %108(ptr %106, ptr %104)
  ret ptr %109

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_2": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_1"
  %110 = getelementptr { i8, { ptr } }, ptr %92, i32 0, i32 1
  %111 = getelementptr { ptr }, ptr %110, i32 0, i32 0
  %112 = load ptr, ptr %111, align 8
  %113 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %114 = getelementptr { i8, { i32 } }, ptr %113, i32 0, i32 0
  store i8 0, ptr %114, align 1
  %115 = getelementptr { i8, { i32 } }, ptr %113, i32 0, i32 1, i32 0
  store i32 1, ptr %115, align 4
  %116 = getelementptr { i8, <4 x i8> }, ptr %112, i32 0, i32 0
  %117 = load i8, ptr %116, align 1
  switch i8 %117, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_2"
  %118 = getelementptr { i8, { i32 } }, ptr %112, i32 0, i32 1
  %119 = getelementptr { i32 }, ptr %118, i32 0, i32 0
  %120 = load i32, ptr %119, align 4
  %121 = getelementptr { i8, <4 x i8> }, ptr %113, i32 0, i32 0
  %122 = load i8, ptr %121, align 1
  switch i8 %122, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  %123 = getelementptr { i8, { i32 } }, ptr %113, i32 0, i32 1
  %124 = getelementptr { i32 }, ptr %123, i32 0, i32 0
  %125 = load i32, ptr %124, align 4
  %126 = call ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr null, i32 %120)
  %127 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 0
  %128 = load ptr, ptr %127, align 8
  %129 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 1
  %130 = load ptr, ptr %129, align 8
  %131 = call i32 %130(ptr %128, i32 %125)
  %132 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %131)
  %133 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %134 = getelementptr { i8, { ptr } }, ptr %133, i32 0, i32 0
  store i8 1, ptr %134, align 1
  %135 = getelementptr { i8, { ptr } }, ptr %133, i32 0, i32 1, i32 0
  store ptr %132, ptr %135, align 8
  %136 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_4 = getelementptr { ptr }, ptr %let_capture_8, i32 0, i32 0
  store ptr %133, ptr %d_4, align 8
  %let_capture_9 = getelementptr { ptr, ptr }, ptr %136, i32 0, i32 0
  store ptr %let_capture_8, ptr %let_capture_9, align 8
  %let_func_4 = getelementptr { ptr, ptr }, ptr %136, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3747", ptr %let_func_4, align 8
  %137 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_9 = getelementptr { ptr, ptr }, ptr %137, i32 0, i32 0
  store ptr %fun_capture_8, ptr %fun_capture_9, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %137, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3748", ptr %fun_func_4, align 8
  %138 = getelementptr { ptr, ptr }, ptr %136, i32 0, i32 0
  %139 = load ptr, ptr %138, align 8
  %140 = getelementptr { ptr, ptr }, ptr %136, i32 0, i32 1
  %141 = load ptr, ptr %140, align 8
  %142 = call ptr %141(ptr %139, ptr %137)
  ret ptr %142

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  unreachable

switch_default_4:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_2"
  unreachable

switch_default_5:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_1"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_3": ; preds = %1
  %143 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %144 = getelementptr { ptr }, ptr %143, i32 0, i32 0
  %145 = load ptr, ptr %144, align 8
  %146 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %147 = getelementptr { i8, { i32 } }, ptr %146, i32 0, i32 0
  store i8 0, ptr %147, align 1
  %148 = getelementptr { i8, { i32 } }, ptr %146, i32 0, i32 1, i32 0
  store i32 1, ptr %148, align 4
  %149 = getelementptr { i8, <4 x i8> }, ptr %145, i32 0, i32 0
  %150 = load i8, ptr %149, align 1
  switch i8 %150, label %switch_default_15 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_3"
  %151 = getelementptr { i8, { i32 } }, ptr %145, i32 0, i32 1
  %152 = getelementptr { i32 }, ptr %151, i32 0, i32 0
  %153 = load i32, ptr %152, align 4
  %154 = getelementptr { i8, <4 x i8> }, ptr %146, i32 0, i32 0
  %155 = load i8, ptr %154, align 1
  switch i8 %155, label %switch_default_14 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  %156 = getelementptr { i8, { i32 } }, ptr %146, i32 0, i32 1
  %157 = getelementptr { i32 }, ptr %156, i32 0, i32 0
  %158 = load i32, ptr %157, align 4
  %159 = call ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr null, i32 %153)
  %160 = getelementptr { ptr, ptr }, ptr %159, i32 0, i32 0
  %161 = load ptr, ptr %160, align 8
  %162 = getelementptr { ptr, ptr }, ptr %159, i32 0, i32 1
  %163 = load ptr, ptr %162, align 8
  %164 = call i32 %163(ptr %161, i32 %158)
  %165 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %164)
  %166 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %167 = getelementptr { i8, { ptr } }, ptr %166, i32 0, i32 0
  store i8 1, ptr %167, align 1
  %168 = getelementptr { i8, { ptr } }, ptr %166, i32 0, i32 1, i32 0
  store ptr %165, ptr %168, align 8
  %169 = call ptr @"test/testcases/malgo/TestEither.mlg.$let_3192"(ptr null, ptr %166)
  %170 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %171 = load i8, ptr %170, align 1
  switch i8 %171, label %switch_default_13 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %172 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %173 = getelementptr { ptr }, ptr %172, i32 0, i32 0
  %174 = load ptr, ptr %173, align 8
  %175 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %176 = getelementptr { i8, { ptr } }, ptr %175, i32 0, i32 0
  store i8 0, ptr %176, align 1
  %177 = getelementptr { i8, { ptr } }, ptr %175, i32 0, i32 1, i32 0
  store ptr %174, ptr %177, align 8
  %178 = getelementptr { i8, <8 x i8> }, ptr %175, i32 0, i32 0
  %179 = load i8, ptr %178, align 1
  switch i8 %179, label %switch_default_9 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_5"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_4"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_5": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4"
  %180 = getelementptr { i8, { ptr } }, ptr %175, i32 0, i32 1
  %181 = getelementptr { ptr }, ptr %180, i32 0, i32 0
  %182 = load ptr, ptr %181, align 8
  %183 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %184 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 0
  store i8 0, ptr %184, align 1
  %185 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 1, i32 0
  store ptr %182, ptr %185, align 8
  %186 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_5 = getelementptr { ptr }, ptr %let_capture_10, i32 0, i32 0
  store ptr %183, ptr %d_5, align 8
  %let_capture_11 = getelementptr { ptr, ptr }, ptr %186, i32 0, i32 0
  store ptr %let_capture_10, ptr %let_capture_11, align 8
  %let_func_5 = getelementptr { ptr, ptr }, ptr %186, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3749", ptr %let_func_5, align 8
  %187 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_11 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 0
  store ptr %fun_capture_10, ptr %fun_capture_11, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3750", ptr %fun_func_5, align 8
  %188 = getelementptr { ptr, ptr }, ptr %186, i32 0, i32 0
  %189 = load ptr, ptr %188, align 8
  %190 = getelementptr { ptr, ptr }, ptr %186, i32 0, i32 1
  %191 = load ptr, ptr %190, align 8
  %192 = call ptr %191(ptr %189, ptr %187)
  ret ptr %192

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_4": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4"
  %193 = getelementptr { i8, { ptr } }, ptr %175, i32 0, i32 1
  %194 = getelementptr { ptr }, ptr %193, i32 0, i32 0
  %195 = load ptr, ptr %194, align 8
  %196 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %197 = getelementptr { i8, { i32 } }, ptr %196, i32 0, i32 0
  store i8 0, ptr %197, align 1
  %198 = getelementptr { i8, { i32 } }, ptr %196, i32 0, i32 1, i32 0
  store i32 1, ptr %198, align 4
  %199 = getelementptr { i8, <4 x i8> }, ptr %195, i32 0, i32 0
  %200 = load i8, ptr %199, align 1
  switch i8 %200, label %switch_default_8 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_4"
  %201 = getelementptr { i8, { i32 } }, ptr %195, i32 0, i32 1
  %202 = getelementptr { i32 }, ptr %201, i32 0, i32 0
  %203 = load i32, ptr %202, align 4
  %204 = getelementptr { i8, <4 x i8> }, ptr %196, i32 0, i32 0
  %205 = load i8, ptr %204, align 1
  switch i8 %205, label %switch_default_7 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %206 = getelementptr { i8, { i32 } }, ptr %196, i32 0, i32 1
  %207 = getelementptr { i32 }, ptr %206, i32 0, i32 0
  %208 = load i32, ptr %207, align 4
  %209 = call ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr null, i32 %203)
  %210 = getelementptr { ptr, ptr }, ptr %209, i32 0, i32 0
  %211 = load ptr, ptr %210, align 8
  %212 = getelementptr { ptr, ptr }, ptr %209, i32 0, i32 1
  %213 = load ptr, ptr %212, align 8
  %214 = call i32 %213(ptr %211, i32 %208)
  %215 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %214)
  %216 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %217 = getelementptr { i8, { ptr } }, ptr %216, i32 0, i32 0
  store i8 1, ptr %217, align 1
  %218 = getelementptr { i8, { ptr } }, ptr %216, i32 0, i32 1, i32 0
  store ptr %215, ptr %218, align 8
  %219 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_6 = getelementptr { ptr }, ptr %let_capture_12, i32 0, i32 0
  store ptr %216, ptr %d_6, align 8
  %let_capture_13 = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 0
  store ptr %let_capture_12, ptr %let_capture_13, align 8
  %let_func_6 = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3751", ptr %let_func_6, align 8
  %220 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_13 = getelementptr { ptr, ptr }, ptr %220, i32 0, i32 0
  store ptr %fun_capture_12, ptr %fun_capture_13, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %220, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3752", ptr %fun_func_6, align 8
  %221 = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 0
  %222 = load ptr, ptr %221, align 8
  %223 = getelementptr { ptr, ptr }, ptr %219, i32 0, i32 1
  %224 = load ptr, ptr %223, align 8
  %225 = call ptr %224(ptr %222, ptr %220)
  ret ptr %225

switch_default_7:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  unreachable

switch_default_8:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_4"
  unreachable

switch_default_9:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_4"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %226 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %227 = getelementptr { ptr }, ptr %226, i32 0, i32 0
  %228 = load ptr, ptr %227, align 8
  %229 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %230 = getelementptr { i8, { ptr } }, ptr %229, i32 0, i32 0
  store i8 0, ptr %230, align 1
  %231 = getelementptr { i8, { ptr } }, ptr %229, i32 0, i32 1, i32 0
  store ptr @str3744, ptr %231, align 8
  %232 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %233 = getelementptr { i8, { ptr } }, ptr %232, i32 0, i32 0
  store i8 0, ptr %233, align 1
  %234 = getelementptr { i8, { ptr } }, ptr %232, i32 0, i32 1, i32 0
  store ptr %229, ptr %234, align 8
  %235 = getelementptr { i8, <8 x i8> }, ptr %232, i32 0, i32 0
  %236 = load i8, ptr %235, align 1
  switch i8 %236, label %switch_default_12 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_6"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_6"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_6": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5"
  %237 = getelementptr { i8, { ptr } }, ptr %232, i32 0, i32 1
  %238 = getelementptr { ptr }, ptr %237, i32 0, i32 0
  %239 = load ptr, ptr %238, align 8
  %240 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %241 = getelementptr { i8, { ptr } }, ptr %240, i32 0, i32 0
  store i8 0, ptr %241, align 1
  %242 = getelementptr { i8, { ptr } }, ptr %240, i32 0, i32 1, i32 0
  store ptr %239, ptr %242, align 8
  %243 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_7 = getelementptr { ptr }, ptr %let_capture_14, i32 0, i32 0
  store ptr %240, ptr %d_7, align 8
  %let_capture_15 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 0
  store ptr %let_capture_14, ptr %let_capture_15, align 8
  %let_func_7 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3753", ptr %let_func_7, align 8
  %244 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_15 = getelementptr { ptr, ptr }, ptr %244, i32 0, i32 0
  store ptr %fun_capture_14, ptr %fun_capture_15, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %244, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3754", ptr %fun_func_7, align 8
  %245 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 0
  %246 = load ptr, ptr %245, align 8
  %247 = getelementptr { ptr, ptr }, ptr %243, i32 0, i32 1
  %248 = load ptr, ptr %247, align 8
  %249 = call ptr %248(ptr %246, ptr %244)
  ret ptr %249

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_6": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5"
  %250 = getelementptr { i8, { ptr } }, ptr %232, i32 0, i32 1
  %251 = getelementptr { ptr }, ptr %250, i32 0, i32 0
  %252 = load ptr, ptr %251, align 8
  %253 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %254 = getelementptr { i8, { i32 } }, ptr %253, i32 0, i32 0
  store i8 0, ptr %254, align 1
  %255 = getelementptr { i8, { i32 } }, ptr %253, i32 0, i32 1, i32 0
  store i32 1, ptr %255, align 4
  %256 = getelementptr { i8, <4 x i8> }, ptr %252, i32 0, i32 0
  %257 = load i8, ptr %256, align 1
  switch i8 %257, label %switch_default_11 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_6"
  %258 = getelementptr { i8, { i32 } }, ptr %252, i32 0, i32 1
  %259 = getelementptr { i32 }, ptr %258, i32 0, i32 0
  %260 = load i32, ptr %259, align 4
  %261 = getelementptr { i8, <4 x i8> }, ptr %253, i32 0, i32 0
  %262 = load i8, ptr %261, align 1
  switch i8 %262, label %switch_default_10 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  %263 = getelementptr { i8, { i32 } }, ptr %253, i32 0, i32 1
  %264 = getelementptr { i32 }, ptr %263, i32 0, i32 0
  %265 = load i32, ptr %264, align 4
  %266 = call ptr @"runtime/malgo/Builtin.mlg.addInt32#"(ptr null, i32 %260)
  %267 = getelementptr { ptr, ptr }, ptr %266, i32 0, i32 0
  %268 = load ptr, ptr %267, align 8
  %269 = getelementptr { ptr, ptr }, ptr %266, i32 0, i32 1
  %270 = load ptr, ptr %269, align 8
  %271 = call i32 %270(ptr %268, i32 %265)
  %272 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %271)
  %273 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %274 = getelementptr { i8, { ptr } }, ptr %273, i32 0, i32 0
  store i8 1, ptr %274, align 1
  %275 = getelementptr { i8, { ptr } }, ptr %273, i32 0, i32 1, i32 0
  store ptr %272, ptr %275, align 8
  %276 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_8 = getelementptr { ptr }, ptr %let_capture_16, i32 0, i32 0
  store ptr %273, ptr %d_8, align 8
  %let_capture_17 = getelementptr { ptr, ptr }, ptr %276, i32 0, i32 0
  store ptr %let_capture_16, ptr %let_capture_17, align 8
  %let_func_8 = getelementptr { ptr, ptr }, ptr %276, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3755", ptr %let_func_8, align 8
  %277 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_17 = getelementptr { ptr, ptr }, ptr %277, i32 0, i32 0
  store ptr %fun_capture_16, ptr %fun_capture_17, align 8
  %fun_func_8 = getelementptr { ptr, ptr }, ptr %277, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_3756", ptr %fun_func_8, align 8
  %278 = getelementptr { ptr, ptr }, ptr %276, i32 0, i32 0
  %279 = load ptr, ptr %278, align 8
  %280 = getelementptr { ptr, ptr }, ptr %276, i32 0, i32 1
  %281 = load ptr, ptr %280, align 8
  %282 = call ptr %281(ptr %279, ptr %277)
  ret ptr %282

switch_default_10:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  unreachable

switch_default_11:                                ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_6"
  unreachable

switch_default_12:                                ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_5"
  unreachable

switch_default_13:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  unreachable

switch_default_14:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  unreachable

switch_default_15:                                ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_3"
  unreachable

switch_default_16:                                ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3757"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3648"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3757", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3758"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3680"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3758", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3759"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3649"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3759", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3760"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3681"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3760", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3761"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3654"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3761", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3762"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3686"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3762", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3763"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3622"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3763", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3764"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3655"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3764", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3765"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3687"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3765", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3766"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3623"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3766", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3767"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3652"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3767", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3768"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3684"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3768", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3769"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3653"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3769", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3770"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3685"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3770", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3621"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$right_275_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$right_275_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_3 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$right_275_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr @str3771, ptr %9, align 8
  %10 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %12 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %13 = getelementptr { ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr @malgo_print_string(ptr %14)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = call ptr @malgo_newline(ptr %18)
  ret ptr %20

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %21 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$right_275_0", i32 0, i32 1
  %22 = getelementptr { ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { i8, <4 x i8> }, ptr %23, i32 0, i32 0
  %25 = load i8, ptr %24, align 1
  switch i8 %25, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %26 = getelementptr { i8, { i32 } }, ptr %23, i32 0, i32 1
  %27 = getelementptr { i32 }, ptr %26, i32 0, i32 0
  %28 = load i32, ptr %27, align 4
  %29 = call ptr @malgo_int32_t_to_string(i32 %28)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 1, i32 0
  store ptr %29, ptr %32, align 8
  %33 = getelementptr { i8, <8 x i8> }, ptr %30, i32 0, i32 0
  %34 = load i8, ptr %33, align 1
  switch i8 %34, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %35 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 1
  %36 = getelementptr { ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr @malgo_print_string(ptr %37)
  ret ptr %38

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$str_723_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3772"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3658"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3772", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3773"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3690"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3773", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3774"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3626"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3774", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3775"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3659"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3775", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3776"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3691"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3776", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3777"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3627"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3777", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3778"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3656"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3778", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3779"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3688"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3779", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3780"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3624"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3780", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3781"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3657"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3781", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3782"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3689"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3782", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3783"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3625"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3783", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3784"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3662"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3784", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3785"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3694"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3785", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3786"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3630"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3786", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3787"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3663"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3787", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3788"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3695"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3788", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3789"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3631"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3789", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3790"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3660"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3790", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3791"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3692"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3791", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3792"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3628"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3792", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.String#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3793"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3661"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3793", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3794"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3693"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3794", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.id"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$x_253_0") {
  ret ptr %"test/testcases/malgo/TestEither.mlg.$x_253_0"
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3795"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.$raw_fun_3629"(ptr %0, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  ]

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_print_string(ptr %11)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = call ptr @malgo_newline(ptr %15)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0"
  unreachable

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0": ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/TestEither.mlg.$left_360_0", i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr null, i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %27, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_3795", ptr %let_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestEither.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TestEither.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestEither.mlg"() {
  ret void
}
