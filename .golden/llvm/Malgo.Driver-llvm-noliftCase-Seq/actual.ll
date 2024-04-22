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

define internal i32 @"Builtin.$addInt32#_curry_4032"(ptr %0, i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0")
  ret i32 %2
}

declare ptr @malgo_malloc(i64)

define internal i32 @"Seq.#let_closure_3020"(ptr %0, i32 %1) {
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
  store ptr @"Seq.#let_closure_3020", ptr %let_func_0, align 8
  ret ptr %2
}

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

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Seq()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Seq.main(ptr null, ptr %2)
  ret i32 0
}

define internal i32 @"Seq.#let_closure_3025"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %malgo_add_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 1
  %malgo_add_int32_t_0 = load ptr, ptr %malgo_add_int32_t_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %1)
  ret i32 %12
}

define internal i32 @"Seq.#let_closure_3026"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %malgo_add_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 1
  %malgo_add_int32_t_0 = load ptr, ptr %malgo_add_int32_t_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %1)
  ret i32 %12
}

define internal void @koriel_load_Seq() {
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
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 0
  store i32 %11, ptr %p_0, align 4
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr null, ptr %malgo_add_int32_t_capture_0, align 8
  %malgo_add_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0, align 8
  %malgo_add_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %18, ptr %malgo_add_int32_t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"Seq.#let_closure_3025", ptr %let_func_0, align 8
  %19 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %11, i32 %16)
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 0
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1, i32 0
  store i32 %19, ptr %22, align 4
  %23 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %"switch_branch_Builtin.Int32#_1"
  %25 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %26 = getelementptr { i32 }, ptr %25, i32 0, i32 0
  %27 = load i32, ptr %26, align 4
  %28 = call ptr @malgo_int32_t_to_string(i32 %27)
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, { ptr } }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %29, i32 0, i32 1, i32 0
  store ptr %28, ptr %31, align 8
  %32 = getelementptr { i8, <8 x i8> }, ptr %29, i32 0, i32 0
  %33 = load i8, ptr %32, align 1
  switch i8 %33, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_2"
  %34 = getelementptr { i8, { ptr } }, ptr %29, i32 0, i32 1
  %35 = getelementptr { ptr }, ptr %34, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = call ptr @malgo_print_string(ptr %36)
  %38 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %39 = load i8, ptr %38, align 1
  switch i8 %39, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_3"
  ]

"switch_branch_Builtin.Int32#_3":                 ; preds = %"switch_branch_Builtin.String#_0"
  %40 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %41 = getelementptr { i32 }, ptr %40, i32 0, i32 0
  %42 = load i32, ptr %41, align 4
  %43 = getelementptr { i8, <4 x i8> }, ptr %4, i32 0, i32 0
  %44 = load i8, ptr %43, align 1
  switch i8 %44, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_4"
  ]

"switch_branch_Builtin.Int32#_4":                 ; preds = %"switch_branch_Builtin.Int32#_3"
  %45 = getelementptr { i8, { i32 } }, ptr %4, i32 0, i32 1
  %46 = getelementptr { i32 }, ptr %45, i32 0, i32 0
  %47 = load i32, ptr %46, align 4
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32, ptr }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32, ptr }, ptr %let_capture_2, i32 0, i32 0
  store i32 %42, ptr %p_1, align 4
  %49 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int32_t_capture_1 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 0
  store ptr null, ptr %malgo_add_int32_t_capture_1, align 8
  %malgo_add_int32_t_func_1 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_1, align 8
  %malgo_add_int32_t_1 = getelementptr { i32, ptr }, ptr %let_capture_2, i32 0, i32 1
  store ptr %49, ptr %malgo_add_int32_t_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 1
  store ptr @"Seq.#let_closure_3026", ptr %let_func_1, align 8
  %50 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %42, i32 %47)
  %51 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %52 = getelementptr { i8, { i32 } }, ptr %51, i32 0, i32 0
  store i8 0, ptr %52, align 1
  %53 = getelementptr { i8, { i32 } }, ptr %51, i32 0, i32 1, i32 0
  store i32 %50, ptr %53, align 4
  store ptr %51, ptr @Seq.executeWhenLoaded, align 8
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
