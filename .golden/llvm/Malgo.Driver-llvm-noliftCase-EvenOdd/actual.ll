; ModuleID = 'test/testcases/malgo/EvenOdd.mlg'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str2949 = unnamed_addr constant [6 x i8] c"False\00"
@str2950 = unnamed_addr constant [5 x i8] c"True\00"

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

define internal i32 @"Builtin.$subInt32#_curry_2292"(ptr %0, i32 %"Builtin.$x_2293_0", i32 %"Builtin.$y_2294_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"Builtin.$x_2293_0", i32 %"Builtin.$y_2294_0")
  ret i32 %2
}

declare ptr @malgo_malloc(i64)

define internal i32 @"EvenOdd.#let_closure_2945"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_sub_int32_t(ptr %0, i32 %"Builtin.$p_1814_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1814_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"EvenOdd.#let_closure_2945", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"EvenOdd.#let_closure_2946"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %malgo_sub_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 1
  %malgo_sub_int32_t_0 = load ptr, ptr %malgo_sub_int32_t_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %1)
  ret i32 %12
}

define internal ptr @EvenOdd.even(ptr %0, ptr %"EvenOdd.$int32#_51_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"EvenOdd.$int32#_51_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_51_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  switch i32 %6, label %switch-unboxed_default_0 [
    i32 0, label %switch-unboxed_branch_0_i32_0
  ]

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_Builtin.Int32#_0"
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 1, ptr %8, align 1
  ret ptr %7

switch-unboxed_default_0:                         ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 1, ptr %11, align 4
  %12 = getelementptr { i8, <4 x i8> }, ptr %"EvenOdd.$int32#_51_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %switch-unboxed_default_0
  %14 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_51_0", i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = getelementptr { i8, <4 x i8> }, ptr %9, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %"switch_branch_Builtin.Int32#_1"
  %19 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1
  %20 = getelementptr { i32 }, ptr %19, i32 0, i32 0
  %21 = load i32, ptr %20, align 4
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 0
  store i32 %16, ptr %p_0, align 4
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %malgo_sub_int32_t_capture_0, align 8
  %malgo_sub_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @Builtin.malgo_sub_int32_t, ptr %malgo_sub_int32_t_func_0, align 8
  %malgo_sub_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %23, ptr %malgo_sub_int32_t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"EvenOdd.#let_closure_2946", ptr %let_func_0, align 8
  %24 = call i32 @"Builtin.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 1, i32 0
  store i32 %24, ptr %27, align 4
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %odd_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %odd_capture_0, align 8
  %odd_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @EvenOdd.odd, ptr %odd_func_0, align 8
  %29 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %25)
  ret ptr %33

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal i32 @"EvenOdd.#let_closure_2947"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %malgo_sub_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i32 0, i32 1
  %malgo_sub_int32_t_0 = load ptr, ptr %malgo_sub_int32_t_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_sub_int32_t_0, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %p_0)
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %1)
  ret i32 %12
}

define internal ptr @EvenOdd.odd(ptr %0, ptr %"EvenOdd.$int32#_61_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"EvenOdd.$int32#_61_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_61_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  switch i32 %6, label %switch-unboxed_default_0 [
    i32 0, label %switch-unboxed_branch_0_i32_0
  ]

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_Builtin.Int32#_0"
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  ret ptr %7

switch-unboxed_default_0:                         ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 1, ptr %11, align 4
  %12 = getelementptr { i8, <4 x i8> }, ptr %"EvenOdd.$int32#_61_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %switch-unboxed_default_0
  %14 = getelementptr { i8, { i32 } }, ptr %"EvenOdd.$int32#_61_0", i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = getelementptr { i8, <4 x i8> }, ptr %9, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_2"
  ]

"switch_branch_Builtin.Int32#_2":                 ; preds = %"switch_branch_Builtin.Int32#_1"
  %19 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1
  %20 = getelementptr { i32 }, ptr %19, i32 0, i32 0
  %21 = load i32, ptr %20, align 4
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 0
  store i32 %16, ptr %p_0, align 4
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_sub_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %malgo_sub_int32_t_capture_0, align 8
  %malgo_sub_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @Builtin.malgo_sub_int32_t, ptr %malgo_sub_int32_t_func_0, align 8
  %malgo_sub_int32_t_0 = getelementptr { i32, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %23, ptr %malgo_sub_int32_t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"EvenOdd.#let_closure_2947", ptr %let_func_0, align 8
  %24 = call i32 @"Builtin.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { i32 } }, ptr %25, i32 0, i32 1, i32 0
  store i32 %24, ptr %27, align 4
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %even_capture_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  store ptr null, ptr %even_capture_0, align 8
  %even_func_0 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  store ptr @EvenOdd.even, ptr %even_func_0, align 8
  %29 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %28, i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %25)
  ret ptr %33

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @"EvenOdd.#fun_closure_2948"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_2 [
    i8 0, label %switch_branch_Builtin.False_0
    i8 1, label %switch_branch_Builtin.True_0
  ]

switch_branch_Builtin.False_0:                    ; preds = %2
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr @str2949, ptr %7, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %5, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_Builtin.False_0
  %10 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Builtin.False_0
  unreachable

switch_branch_Builtin.True_0:                     ; preds = %2
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr @str2950, ptr %21, align 8
  %22 = getelementptr { i8, <8 x i8> }, ptr %19, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Builtin.True_0
  %24 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1
  %25 = getelementptr { ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr @malgo_print_string(ptr %26)
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %29 = getelementptr { i8, {} }, ptr %28, i32 0, i32 0
  store i8 0, ptr %29, align 1
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = call ptr @malgo_newline(ptr %30)
  ret ptr %32

switch_default_1:                                 ; preds = %switch_branch_Builtin.True_0
  unreachable

switch_default_2:                                 ; preds = %2
  unreachable
}

define internal ptr @EvenOdd.main(ptr %0, ptr %"EvenOdd.$$__71_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"EvenOdd.#fun_closure_2948", ptr %fun_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1, i32 0
  store i32 10, ptr %5, align 4
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %even_capture_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr null, ptr %even_capture_0, align 8
  %even_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @EvenOdd.even, ptr %even_func_0, align 8
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %3)
  %12 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %11)
  ret ptr %16
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_EvenOdd()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @EvenOdd.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_EvenOdd() {
  ret void
}
