; ModuleID = './test/testcases/malgo/RecordTest.mlg'
source_filename = "./test/testcases/malgo/RecordTest.mlg"

@str3534 = unnamed_addr constant [2 x i8] c"a\00"
@str3535 = unnamed_addr constant [2 x i8] c"b\00"

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

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @RecordTest.f(ptr %0, ptr %"RecordTest.$record_56_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"RecordTest.$record_56_0", ptr @str3534)
  %3 = call ptr @malgo_hash_table_get(ptr %"RecordTest.$record_56_0", ptr @str3535)
  ret ptr %2
}

define internal ptr @RecordTest.g(ptr %0, ptr %"RecordTest.$b_52_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"RecordTest.$b_52_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_RecordTest.B_0
  ]

switch_branch_RecordTest.B_0:                     ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"RecordTest.$b_52_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_hash_table_get(ptr %6, ptr @str3534)
  %8 = call ptr @malgo_hash_table_get(ptr %6, ptr @str3535)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"RecordTest.$raw_let_3533"(ptr %0, i32 %"RecordTest.$p_3519_0", i32 %"RecordTest.$y_3528_0") {
  %2 = call ptr @Builtin.malgo_add_int32_t(ptr null, i32 %"RecordTest.$p_3519_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"RecordTest.$y_3528_0")
  ret i32 %7
}

declare ptr @malgo_malloc(i64)

define internal ptr @RecordTest.B(ptr %0, ptr %"RecordTest.$p_50_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"RecordTest.$p_50_0", ptr %4, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$addInt32#_curry_4032"(ptr %0, i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0")
  ret i32 %2
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal i32 @"RecordTest.#let_closure_3536"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"RecordTest.$raw_let_3533"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @RecordTest.main(ptr %0, ptr %"RecordTest.$$__59_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 32, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 1, i32 0
  store i32 10, ptr %7, align 4
  %8 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %8, ptr @str3534, ptr %2)
  call void @malgo_hash_table_insert(ptr %8, ptr @str3535, ptr %5)
  %9 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3534)
  %10 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3535)
  %11 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3534)
  %12 = call ptr @malgo_hash_table_get(ptr %8, ptr @str3535)
  %13 = getelementptr { i8, <4 x i8> }, ptr %9, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %15 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1
  %16 = getelementptr { i32 }, ptr %15, i32 0, i32 0
  %17 = load i32, ptr %16, align 4
  %18 = getelementptr { i8, <4 x i8> }, ptr %12, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %20 = getelementptr { i8, { i32 } }, ptr %12, i32 0, i32 1
  %21 = getelementptr { i32 }, ptr %20, i32 0, i32 0
  %22 = load i32, ptr %21, align 4
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %17, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"RecordTest.#let_closure_3536", ptr %let_func_0, align 8
  %24 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %17, i32 %22)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 1, i32 0
  store i32 %24, ptr %27, align 4
  %28 = getelementptr { i8, <4 x i8> }, ptr %25, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %"switch_branch_Builtin.Int32#_1"
  %30 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 1
  %31 = getelementptr { i32 }, ptr %30, i32 0, i32 0
  %32 = load i32, ptr %31, align 4
  %33 = call ptr @malgo_int32_t_to_string(i32 %32)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1, i32 0
  store ptr %33, ptr %36, align 8
  %37 = getelementptr { i8, <8 x i8> }, ptr %34, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_2"
  %39 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1
  %40 = getelementptr { ptr }, ptr %39, i32 0, i32 0
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr @malgo_print_string(ptr %41)
  ret ptr %42

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_2"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal i32 @"RecordTest.#let_closure_3537"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_add_int32_t(ptr %0, i32 %"Builtin.$p_1808_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1808_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"RecordTest.#let_closure_3537", ptr %let_func_0, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_RecordTest()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @RecordTest.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_RecordTest() {
  ret void
}
