; ModuleID = 'test/testcases/malgo/RightAssoc.mlg'
source_filename = "test/testcases/malgo/RightAssoc.mlg"

@str3914 = unnamed_addr constant [3 x i8] c"OK\00"

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

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3906"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__138_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3910"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__148_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.main"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__186_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  %5 = call ptr @"test/testcases/malgo/RightAssoc.mlg.f"(ptr null, ptr %2)
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1, i32 0
  store ptr @str3914, ptr %8, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %11 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr @malgo_print_string(ptr %13)
  ret ptr %14

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_3915"(ptr %0, ptr %1) {
  %eta_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %eta_0 = load ptr, ptr %eta_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_let_3658"(ptr null, ptr %eta_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.<|>"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %eta_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0", ptr %eta_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_3915", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3850"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__99_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3882"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__109_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3786"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__89_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3912"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__158_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3913"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__168_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3916"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3786"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3917"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3850"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3918"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3882"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3919"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3898"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3920"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3906"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3921"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3910"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3922"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3912"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3923"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3913"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.f"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$n_87_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3916", ptr %fun_func_0, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %5, i32 0, i32 1, i32 1
  store ptr %4, ptr %8, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, {} }, ptr %9, i32 0, i32 0
  store i8 1, ptr %10, align 1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3917", ptr %fun_func_1, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %9, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 1
  store ptr %11, ptr %15, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 1, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_5 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %fun_capture_4, ptr %fun_capture_5, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3918", ptr %fun_func_2, align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr %16, ptr %21, align 8
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1, i32 1
  store ptr %18, ptr %22, align 8
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %24 = getelementptr { i8, {} }, ptr %23, i32 0, i32 0
  store i8 1, ptr %24, align 1
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_7 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %fun_capture_6, ptr %fun_capture_7, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3919", ptr %fun_func_3, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %23, ptr %28, align 8
  %29 = getelementptr { i8, { ptr, ptr } }, ptr %26, i32 0, i32 1, i32 1
  store ptr %25, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 0
  store i8 1, ptr %33, align 1
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr %26, ptr %34, align 8
  %35 = getelementptr { i8, { ptr, ptr } }, ptr %32, i32 0, i32 1, i32 1
  store ptr %30, ptr %35, align 8
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 0
  store i8 1, ptr %37, align 1
  %38 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 1, i32 0
  store ptr %19, ptr %38, align 8
  %39 = getelementptr { i8, { ptr, ptr } }, ptr %36, i32 0, i32 1, i32 1
  store ptr %32, ptr %39, align 8
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %41 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 0
  store i8 1, ptr %41, align 1
  %42 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 1, i32 0
  store ptr %12, ptr %42, align 8
  %43 = getelementptr { i8, { ptr, ptr } }, ptr %40, i32 0, i32 1, i32 1
  store ptr %36, ptr %43, align 8
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 0
  store i8 1, ptr %45, align 1
  %46 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr %5, ptr %46, align 8
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 1
  store ptr %40, ptr %47, align 8
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %49 = getelementptr { i8, {} }, ptr %48, i32 0, i32 0
  store i8 1, ptr %49, align 1
  %50 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_9 = getelementptr { ptr, ptr }, ptr %50, i32 0, i32 0
  store ptr %fun_capture_8, ptr %fun_capture_9, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %50, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3920", ptr %fun_func_4, align 8
  %51 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %52 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 0
  store i8 0, ptr %52, align 1
  %53 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 1, i32 0
  store ptr %48, ptr %53, align 8
  %54 = getelementptr { i8, { ptr, ptr } }, ptr %51, i32 0, i32 1, i32 1
  store ptr %50, ptr %54, align 8
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %56 = getelementptr { i8, {} }, ptr %55, i32 0, i32 0
  store i8 1, ptr %56, align 1
  %57 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_11 = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 0
  store ptr %fun_capture_10, ptr %fun_capture_11, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %57, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3921", ptr %fun_func_5, align 8
  %58 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %59 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 0
  store i8 0, ptr %59, align 1
  %60 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 1, i32 0
  store ptr %55, ptr %60, align 8
  %61 = getelementptr { i8, { ptr, ptr } }, ptr %58, i32 0, i32 1, i32 1
  store ptr %57, ptr %61, align 8
  %62 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %63 = getelementptr { i8, {} }, ptr %62, i32 0, i32 0
  store i8 1, ptr %63, align 1
  %64 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_13 = getelementptr { ptr, ptr }, ptr %64, i32 0, i32 0
  store ptr %fun_capture_12, ptr %fun_capture_13, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %64, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3922", ptr %fun_func_6, align 8
  %65 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %66 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 0
  store i8 0, ptr %66, align 1
  %67 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 1, i32 0
  store ptr %62, ptr %67, align 8
  %68 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 1, i32 1
  store ptr %64, ptr %68, align 8
  %69 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %70 = getelementptr { i8, {} }, ptr %69, i32 0, i32 0
  store i8 1, ptr %70, align 1
  %71 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_15 = getelementptr { ptr, ptr }, ptr %71, i32 0, i32 0
  store ptr %fun_capture_14, ptr %fun_capture_15, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %71, i32 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_3923", ptr %fun_func_7, align 8
  %72 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %73 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 0
  store i8 0, ptr %73, align 1
  %74 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 1, i32 0
  store ptr %69, ptr %74, align 8
  %75 = getelementptr { i8, { ptr, ptr } }, ptr %72, i32 0, i32 1, i32 1
  store ptr %71, ptr %75, align 8
  %76 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %77 = getelementptr { i8, {} }, ptr %76, i32 0, i32 0
  store i8 0, ptr %77, align 1
  %78 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %79 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 0
  store i8 1, ptr %79, align 1
  %80 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 1, i32 0
  store ptr %72, ptr %80, align 8
  %81 = getelementptr { i8, { ptr, ptr } }, ptr %78, i32 0, i32 1, i32 1
  store ptr %76, ptr %81, align 8
  %82 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %83 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 0
  store i8 1, ptr %83, align 1
  %84 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 1, i32 0
  store ptr %65, ptr %84, align 8
  %85 = getelementptr { i8, { ptr, ptr } }, ptr %82, i32 0, i32 1, i32 1
  store ptr %78, ptr %85, align 8
  %86 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %87 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 0
  store i8 1, ptr %87, align 1
  %88 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 1, i32 0
  store ptr %58, ptr %88, align 8
  %89 = getelementptr { i8, { ptr, ptr } }, ptr %86, i32 0, i32 1, i32 1
  store ptr %82, ptr %89, align 8
  %90 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %91 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 0
  store i8 1, ptr %91, align 1
  %92 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 1, i32 0
  store ptr %51, ptr %92, align 8
  %93 = getelementptr { i8, { ptr, ptr } }, ptr %90, i32 0, i32 1, i32 1
  store ptr %86, ptr %93, align 8
  ret ptr %90
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_fun_3898"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$$__119_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.$raw_let_3658"(ptr %0, ptr %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0", ptr %"test/testcases/malgo/RightAssoc.mlg.$p_3600_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"test/testcases/malgo/RightAssoc.mlg.$p_3600_0", ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/RightAssoc.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/RightAssoc.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/RightAssoc.mlg"() {
  ret void
}
