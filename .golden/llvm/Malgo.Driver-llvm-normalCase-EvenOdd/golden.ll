; ModuleID = './test/testcases/malgo/EvenOdd.mlg'
source_filename = "./test/testcases/malgo/EvenOdd.mlg"

@str2947 = unnamed_addr constant [6 x i8] c"False\00"
@str2948 = unnamed_addr constant [5 x i8] c"True\00"

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

define internal ptr @"EvenOdd.$fun_79"(ptr %0, ptr %"EvenOdd.$false_72_0") {
  %2 = getelementptr { i8, {} }, ptr %"EvenOdd.$false_72_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Builtin.False_0
    i8 1, label %switch_branch_Builtin.True_0
  ]

switch_branch_Builtin.False_0:                    ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1, i32 0
  store ptr @str2947, ptr %6, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %4, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_Builtin.False_0
  %9 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Builtin.False_0
  unreachable

switch_branch_Builtin.True_0:                     ; preds = %1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr @str2948, ptr %20, align 8
  %21 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Builtin.True_0
  %23 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_print_string(ptr %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, {} }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = call ptr @malgo_newline(ptr %29)
  ret ptr %31

switch_default_1:                                 ; preds = %switch_branch_Builtin.True_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal i32 @"EvenOdd.#let_closure_2949"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"EvenOdd.$raw_let_2945"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
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
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %16, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"EvenOdd.#let_closure_2949", ptr %let_func_0, align 8
  %23 = call i32 @"Builtin.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %25 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 0
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1, i32 0
  store i32 %23, ptr %26, align 4
  %27 = call ptr @EvenOdd.odd(ptr null, ptr %24)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @EvenOdd.main(ptr %0, ptr %"EvenOdd.$$__71_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 10, ptr %4, align 4
  %5 = call ptr @EvenOdd.even(ptr null, ptr %2)
  %6 = call ptr @"EvenOdd.$fun_79"(ptr null, ptr %5)
  ret ptr %6
}

define internal i32 @"EvenOdd.#let_closure_2950"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"EvenOdd.$raw_let_2946"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
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
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %16, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  store ptr @"EvenOdd.#let_closure_2950", ptr %let_func_0, align 8
  %23 = call i32 @"Builtin.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %25 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 0
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1, i32 0
  store i32 %23, ptr %26, align 4
  %27 = call ptr @EvenOdd.even(ptr null, ptr %24)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal i32 @"EvenOdd.$raw_let_2945"(ptr %0, i32 %"EvenOdd.$p_2878_0", i32 %"EvenOdd.$y_2887_0") {
  %2 = call ptr @Builtin.malgo_sub_int32_t(ptr null, i32 %"EvenOdd.$p_2878_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"EvenOdd.$y_2887_0")
  ret i32 %7
}

define internal i32 @"EvenOdd.#let_closure_2951"(ptr %0, i32 %1) {
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
  store ptr @"EvenOdd.#let_closure_2951", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"EvenOdd.$raw_let_2946"(ptr %0, i32 %"EvenOdd.$p_2906_0", i32 %"EvenOdd.$y_2915_0") {
  %2 = call ptr @Builtin.malgo_sub_int32_t(ptr null, i32 %"EvenOdd.$p_2906_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"EvenOdd.$y_2915_0")
  ret i32 %7
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
