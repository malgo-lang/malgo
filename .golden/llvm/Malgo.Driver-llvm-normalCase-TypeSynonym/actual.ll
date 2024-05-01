; ModuleID = 'test/testcases/malgo/TypeSynonym.mlg'
source_filename = "test/testcases/malgo/TypeSynonym.mlg"

@"test/testcases/malgo/TypeSynonym.mlg.hello" = global ptr undef
@str2861 = unnamed_addr constant [2 x i8] c" \00"
@str2868 = unnamed_addr constant [6 x i8] c"hello\00"
@str2869 = unnamed_addr constant [6 x i8] c"world\00"

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

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.$raw_fun_2860"(ptr %0, ptr %"test/testcases/malgo/TypeSynonym.mlg.$tuple_42_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"test/testcases/malgo/TypeSynonym.mlg.$tuple_42_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_3 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/TypeSynonym.mlg.$tuple_42_0", i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_Tuple#_0"
  %11 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr @malgo_print_string(ptr %13)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 1, i32 0
  store ptr @str2861, ptr %17, align 8
  %18 = getelementptr { i8, <8 x i8> }, ptr %15, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %20 = getelementptr { i8, { ptr } }, ptr %15, i32 0, i32 1
  %21 = getelementptr { ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr @malgo_print_string(ptr %22)
  %24 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %25 = load i8, ptr %24, align 1
  switch i8 %25, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  %26 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %27 = getelementptr { ptr }, ptr %26, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr @malgo_print_string(ptr %28)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, {} }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = call ptr @malgo_newline(ptr %32)
  ret ptr %34

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_Tuple#_0"
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.idOverS"(ptr %0, ptr %"test/testcases/malgo/TypeSynonym.mlg.$x_31_0") {
  ret ptr %"test/testcases/malgo/TypeSynonym.mlg.$x_31_0"
}

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.#let_closure_2862"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %cast_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.#fun_closure_2863"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/TypeSynonym.mlg.$raw_fun_2860"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.main"(ptr %0, ptr %"test/testcases/malgo/TypeSynonym.mlg.$$__38_0") {
  %2 = load ptr, ptr @"test/testcases/malgo/TypeSynonym.mlg.hello", align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %2, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/TypeSynonym.mlg.#let_closure_2862", ptr %let_func_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"test/testcases/malgo/TypeSynonym.mlg.#fun_closure_2863", ptr %fun_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %4)
  ret ptr %9
}

define internal ptr @"test/testcases/malgo/TypeSynonym.mlg.TCon"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TypeSynonym.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TypeSynonym.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TypeSynonym.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1, i32 0
  store ptr @str2868, ptr %3, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1, i32 0
  store ptr @str2869, ptr %6, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %1, ptr %9, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %7, i32 0, i32 1, i32 1
  store ptr %4, ptr %10, align 8
  store ptr %7, ptr @"test/testcases/malgo/TypeSynonym.mlg.hello", align 8
  ret void
}
