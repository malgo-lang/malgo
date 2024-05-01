; ModuleID = 'test/testcases/malgo/ToplevelVariable.mlg'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@"test/testcases/malgo/ToplevelVariable.mlg.one" = global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.comp" = global ptr undef
@str3103 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3095"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariable.mlg.$p_2920_0", i32 %"test/testcases/malgo/ToplevelVariable.mlg.$y_2929_0") {
  %2 = call ptr @"runtime/malgo/Builtin.mlg.malgo_add_int32_t"(ptr null, i32 %"test/testcases/malgo/ToplevelVariable.mlg.$p_2920_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"test/testcases/malgo/ToplevelVariable.mlg.$y_2929_0")
  ret i32 %7
}

declare ptr @malgo_malloc(i64)

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3100"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3095"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.addOne"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0") {
  %2 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %5 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0", i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %10 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_102_0", i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3100", ptr %let_func_0, align 8
  %14 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %7, i32 %12)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 1, i32 0
  store i32 %14, ptr %17, align 4
  ret ptr %15

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Prelude.mlg.identity"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$x_890_0") {
  ret ptr %"runtime/malgo/Prelude.mlg.$x_890_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3101"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3094"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.constId"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_87_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %2, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3101", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$eta_87_0")
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3096"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0", ptr %"test/testcases/malgo/ToplevelVariable.mlg.$__2957_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0"
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3102"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3102", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3104"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3096"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3105"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3106"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3098"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3107"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.main"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$$__104_0") {
  %2 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_9 [
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr @str3103, ptr %7, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %5, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
  %10 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0, align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %15, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3104", ptr %let_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %14)
  %22 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %23 = getelementptr { i8, <4 x i8> }, ptr %22, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %25 = getelementptr { i8, { i32 } }, ptr %22, i32 0, i32 1
  %26 = getelementptr { i32 }, ptr %25, i32 0, i32 0
  %27 = load i32, ptr %26, align 4
  %28 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %29 = getelementptr { i8, <4 x i8> }, ptr %28, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %31 = getelementptr { i8, { i32 } }, ptr %28, i32 0, i32 1
  %32 = getelementptr { i32 }, ptr %31, i32 0, i32 0
  %33 = load i32, ptr %32, align 4
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_2, i32 0, i32 0
  store i32 %27, ptr %p_0, align 4
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3105", ptr %let_func_1, align 8
  %35 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call i32 %38(ptr %36, i32 %33)
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %41 = getelementptr { i8, { i32 } }, ptr %40, i32 0, i32 0
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { i32 } }, ptr %40, i32 0, i32 1, i32 0
  store i32 %39, ptr %42, align 4
  %43 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %44 = load ptr, ptr %43, align 8
  %45 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %46 = load ptr, ptr %45, align 8
  %47 = call ptr %46(ptr %44, ptr %40)
  %48 = getelementptr { i8, <4 x i8> }, ptr %47, i32 0, i32 0
  %49 = load i8, ptr %48, align 1
  switch i8 %49, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %50 = getelementptr { i8, { i32 } }, ptr %47, i32 0, i32 1
  %51 = getelementptr { i32 }, ptr %50, i32 0, i32 0
  %52 = load i32, ptr %51, align 4
  %53 = call ptr @malgo_int32_t_to_string(i32 %52)
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %55 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 0
  store i8 0, ptr %55, align 1
  %56 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 1, i32 0
  store ptr %53, ptr %56, align 8
  %57 = getelementptr { i8, <8 x i8> }, ptr %54, i32 0, i32 0
  %58 = load i8, ptr %57, align 1
  switch i8 %58, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  %59 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 1
  %60 = getelementptr { ptr }, ptr %59, i32 0, i32 0
  %61 = load ptr, ptr %60, align 8
  %62 = call ptr @malgo_print_string(ptr %61)
  ret ptr %62

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_4:                                 ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
  unreachable

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %1
  %63 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %64 = getelementptr { ptr }, ptr %63, i32 0, i32 0
  %65 = load ptr, ptr %64, align 8
  %66 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_1 = getelementptr { ptr, ptr }, ptr %66, i32 0, i32 0
  store ptr null, ptr %const_capture_1, align 8
  %const_func_1 = getelementptr { ptr, ptr }, ptr %66, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_1, align 8
  %67 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_1 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 0
  store ptr null, ptr %identity_capture_1, align 8
  %identity_func_1 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_1, align 8
  %68 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_1 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %67, ptr %cast_1, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3106", ptr %let_func_2, align 8
  %69 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 1
  %72 = load ptr, ptr %71, align 8
  %73 = call ptr %72(ptr %70, ptr %66)
  %74 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %75 = getelementptr { i8, <4 x i8> }, ptr %74, i32 0, i32 0
  %76 = load i8, ptr %75, align 1
  switch i8 %76, label %switch_default_8 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  %77 = getelementptr { i8, { i32 } }, ptr %74, i32 0, i32 1
  %78 = getelementptr { i32 }, ptr %77, i32 0, i32 0
  %79 = load i32, ptr %78, align 4
  %80 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %81 = getelementptr { i8, <4 x i8> }, ptr %80, i32 0, i32 0
  %82 = load i8, ptr %81, align 1
  switch i8 %82, label %switch_default_7 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %83 = getelementptr { i8, { i32 } }, ptr %80, i32 0, i32 1
  %84 = getelementptr { i32 }, ptr %83, i32 0, i32 0
  %85 = load i32, ptr %84, align 4
  %86 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32 }, ptr %let_capture_6, i32 0, i32 0
  store i32 %79, ptr %p_1, align 4
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3107", ptr %let_func_3, align 8
  %87 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 0
  %88 = load ptr, ptr %87, align 8
  %89 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 1
  %90 = load ptr, ptr %89, align 8
  %91 = call i32 %90(ptr %88, i32 %85)
  %92 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %93 = getelementptr { i8, { i32 } }, ptr %92, i32 0, i32 0
  store i8 0, ptr %93, align 1
  %94 = getelementptr { i8, { i32 } }, ptr %92, i32 0, i32 1, i32 0
  store i32 %91, ptr %94, align 4
  %95 = getelementptr { ptr, ptr }, ptr %73, i32 0, i32 0
  %96 = load ptr, ptr %95, align 8
  %97 = getelementptr { ptr, ptr }, ptr %73, i32 0, i32 1
  %98 = load ptr, ptr %97, align 8
  %99 = call ptr %98(ptr %96, ptr %92)
  %100 = getelementptr { i8, <4 x i8> }, ptr %99, i32 0, i32 0
  %101 = load i8, ptr %100, align 1
  switch i8 %101, label %switch_default_6 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  %102 = getelementptr { i8, { i32 } }, ptr %99, i32 0, i32 1
  %103 = getelementptr { i32 }, ptr %102, i32 0, i32 0
  %104 = load i32, ptr %103, align 4
  %105 = call ptr @malgo_int32_t_to_string(i32 %104)
  %106 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %107 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 0
  store i8 0, ptr %107, align 1
  %108 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 1, i32 0
  store ptr %105, ptr %108, align 8
  %109 = getelementptr { i8, <8 x i8> }, ptr %106, i32 0, i32 0
  %110 = load i8, ptr %109, align 1
  switch i8 %110, label %switch_default_5 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %111 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 1
  %112 = getelementptr { ptr }, ptr %111, i32 0, i32 0
  %113 = load ptr, ptr %112, align 8
  %114 = call ptr @malgo_print_string(ptr %113)
  ret ptr %114

switch_default_5:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  unreachable

switch_default_7:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  unreachable

switch_default_8:                                 ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  unreachable

switch_default_9:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3097"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0", ptr %"test/testcases/malgo/ToplevelVariable.mlg.$__2957_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3098"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0", ptr %"test/testcases/malgo/ToplevelVariable.mlg.$__2969_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3099"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0", ptr %"test/testcases/malgo/ToplevelVariable.mlg.$__2981_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_2944_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3093"(ptr %0, ptr %"runtime/malgo/Prelude.mlg.$a_956_0", ptr %"runtime/malgo/Prelude.mlg.$__957_0") {
  ret ptr %"runtime/malgo/Prelude.mlg.$a_956_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3094"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_84_0", ptr %"test/testcases/malgo/ToplevelVariable.mlg.$__2907_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariable.mlg.$cast_84_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3108"(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariable.mlg.$raw_let_3093"(ptr null, ptr %a_0, ptr %1)
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3108", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$addInt32#_curry_4032"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_4033_0", i32 %"runtime/malgo/Builtin.mlg.$y_4034_0")
  ret i32 %2
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

define internal void @"malgo_load_test/testcases/malgo/ToplevelVariable.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %1, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
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
    i8 0, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0"
    i8 1, label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0"
  ]

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0": ; preds = %0
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  store ptr %11, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  ret void

"switch_branch_runtime/malgo/Prelude.mlg.Just_0": ; preds = %0
  %13 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  store ptr %16, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  ret void

switch_default_0:                                 ; preds = %0
  unreachable
}
