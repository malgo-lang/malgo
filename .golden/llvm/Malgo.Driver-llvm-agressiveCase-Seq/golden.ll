; ModuleID = './test/testcases/malgo/Seq.mlg'
source_filename = "./test/testcases/malgo/Seq.mlg"

@Seq.executeWhenLoaded = global ptr undef

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

define internal ptr @Seq.main(ptr %0, ptr %"Seq.$$__43_0") {
  %2 = load ptr, ptr @Seq.executeWhenLoaded, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %5 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr %8, ptr %11, align 8
  %12 = getelementptr { i8, <8 x i8> }, ptr %9, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %14 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1
  %15 = getelementptr { ptr }, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr @malgo_print_string(ptr %16)
  ret ptr %17

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$addInt32#_curry_4032"(ptr %0, i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0")
  ret i32 %2
}

define internal i32 @"Seq.#let_closure_3649"(ptr %0, i32 %1) {
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
  store ptr @"Seq.#let_closure_3649", ptr %let_func_0, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Seq()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Seq.main(ptr null, ptr %2)
  ret i32 0
}

define internal i32 @"Seq.#let_closure_3654"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call ptr @Builtin.malgo_add_int32_t(ptr null, i32 %p_0)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call i32 %7(ptr %5, i32 %1)
  ret i32 %8
}

define internal i32 @"Seq.#let_closure_3655"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call ptr @Builtin.malgo_add_int32_t(ptr null, i32 %p_0)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call i32 %7(ptr %5, i32 %1)
  ret i32 %8
}

define internal void @malgo_load_Seq() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1, i32 0
  store i32 2, ptr %6, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_5 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %0
  %9 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = getelementptr { i8, <4 x i8> }, ptr %4, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_4 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %14 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %11, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"Seq.#let_closure_3654", ptr %let_func_0, align 8
  %18 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %11, i32 %16)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 1, i32 0
  store i32 %18, ptr %21, align 4
  %22 = getelementptr { i8, <4 x i8> }, ptr %19, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %"switch_branch_Builtin.Int32#_1"
  %24 = getelementptr { i8, { i32 } }, ptr %19, i32 0, i32 1
  %25 = getelementptr { i32 }, ptr %24, i32 0, i32 0
  %26 = load i32, ptr %25, align 4
  %27 = call ptr @malgo_int32_t_to_string(i32 %26)
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %29 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 0
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 1, i32 0
  store ptr %27, ptr %30, align 8
  %31 = getelementptr { i8, <8 x i8> }, ptr %28, i32 0, i32 0
  %32 = load i8, ptr %31, align 1
  switch i8 %32, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_2"
  %33 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 1
  %34 = getelementptr { ptr }, ptr %33, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = call ptr @malgo_print_string(ptr %35)
  %37 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_3"
  ]

"switch_branch_Builtin.Int32#_3":                 ; preds = %"switch_branch_Builtin.String#_0"
  %39 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %40 = getelementptr { i32 }, ptr %39, i32 0, i32 0
  %41 = load i32, ptr %40, align 4
  %42 = getelementptr { i8, <4 x i8> }, ptr %4, i32 0, i32 0
  %43 = load i8, ptr %42, align 1
  switch i8 %43, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_4"
  ]

"switch_branch_Builtin.Int32#_4":                 ; preds = %"switch_branch_Builtin.Int32#_3"
  %44 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1
  %45 = getelementptr { i32 }, ptr %44, i32 0, i32 0
  %46 = load i32, ptr %45, align 4
  %47 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32 }, ptr %let_capture_2, i32 0, i32 0
  store i32 %41, ptr %p_1, align 4
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  store ptr @"Seq.#let_closure_3655", ptr %let_func_1, align 8
  %48 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %41, i32 %46)
  %49 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %50 = getelementptr { i8, { i32 } }, ptr %49, i32 0, i32 0
  store i8 0, ptr %50, align 1
  %51 = getelementptr { i8, { i32 } }, ptr %49, i32 0, i32 1, i32 0
  store i32 %48, ptr %51, align 4
  store ptr %49, ptr @Seq.executeWhenLoaded, align 8
  ret void

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_3"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_Builtin.Int32#_2"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_4:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_5:                                 ; preds = %0
  unreachable
}
