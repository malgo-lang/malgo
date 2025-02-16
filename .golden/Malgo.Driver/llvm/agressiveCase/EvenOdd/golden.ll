; ModuleID = 'test/testcases/malgo/EvenOdd.mlg'
source_filename = "test/testcases/malgo/EvenOdd.mlg"

@str4776 = unnamed_addr constant [6 x i8] c"False\00"
@str4777 = unnamed_addr constant [5 x i8] c"True\00"

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

define internal ptr @"runtime/malgo/Builtin.mlg.False"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.True"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4774"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1814_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1814_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4774", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2287_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$x_2287_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4775", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"runtime/malgo/Builtin.mlg.$p_1817_0", i32 %"runtime/malgo/Builtin.mlg.$p_1818_0")
  ret i32 %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4700"(ptr %0, i32 %"test/testcases/malgo/EvenOdd.mlg.$p_3529_0", i32 %"test/testcases/malgo/EvenOdd.mlg.$y_3560_0") {
  %2 = call ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t"(ptr null, i32 %"test/testcases/malgo/EvenOdd.mlg.$p_3529_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"test/testcases/malgo/EvenOdd.mlg.$y_3560_0")
  ret i32 %7
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4737"(ptr %0, i32 %"test/testcases/malgo/EvenOdd.mlg.$p_3982_0", i32 %"test/testcases/malgo/EvenOdd.mlg.$y_4013_0") {
  %2 = call ptr @"runtime/malgo/Builtin.mlg.malgo_sub_int32_t"(ptr null, i32 %"test/testcases/malgo/EvenOdd.mlg.$p_3982_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"test/testcases/malgo/EvenOdd.mlg.$y_4013_0")
  ret i32 %7
}

define internal i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr %0, i32 %"runtime/malgo/Builtin.mlg.$x_2293_0", i32 %"runtime/malgo/Builtin.mlg.$y_2294_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"runtime/malgo/Builtin.mlg.$x_2293_0", i32 %"runtime/malgo/Builtin.mlg.$y_2294_0")
  ret i32 %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4778"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4779"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4765"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4780"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.main"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$$__71_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 10, ptr %4, align 4
  %5 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_26 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %7 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i32 }, ptr %7, i32 0, i32 0
  %9 = load i32, ptr %8, align 4
  switch i32 %9, label %switch-unboxed_default_0 [
    i32 0, label %switch-unboxed_branch_0_i32_0
  ]

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 1, ptr %11, align 1
  %12 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %switch-unboxed_branch_0_i32_0
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %24 = getelementptr { i8, {} }, ptr %23, i32 0, i32 0
  store i8 0, ptr %24, align 1
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, {} }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = call ptr @malgo_newline(ptr %25)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_0"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %switch-unboxed_branch_0_i32_0
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %29 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 0
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %30, align 8
  %31 = getelementptr { i8, <8 x i8> }, ptr %28, i32 0, i32 0
  %32 = load i8, ptr %31, align 1
  switch i8 %32, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  %33 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 1
  %34 = getelementptr { ptr }, ptr %33, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = call ptr @malgo_print_string(ptr %35)
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %38 = getelementptr { i8, {} }, ptr %37, i32 0, i32 0
  store i8 0, ptr %38, align 1
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %40 = getelementptr { i8, {} }, ptr %39, i32 0, i32 0
  store i8 0, ptr %40, align 1
  %41 = call ptr @malgo_newline(ptr %39)
  ret ptr %41

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_0"
  unreachable

switch_default_2:                                 ; preds = %switch-unboxed_branch_0_i32_0
  unreachable

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %43 = getelementptr { i8, { i32 } }, ptr %42, i32 0, i32 0
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { i32 } }, ptr %42, i32 0, i32 1, i32 0
  store i32 1, ptr %44, align 4
  %45 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %46 = load i8, ptr %45, align 1
  switch i8 %46, label %switch_default_25 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %switch-unboxed_default_0
  %47 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %48 = getelementptr { i32 }, ptr %47, i32 0, i32 0
  %49 = load i32, ptr %48, align 4
  %50 = getelementptr { i8, <4 x i8> }, ptr %42, i32 0, i32 0
  %51 = load i8, ptr %50, align 1
  switch i8 %51, label %switch_default_24 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %52 = getelementptr { i8, { i32 } }, ptr %42, i32 0, i32 1
  %53 = getelementptr { i32 }, ptr %52, i32 0, i32 0
  %54 = load i32, ptr %53, align 4
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %49, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4778", ptr %let_func_0, align 8
  %56 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  %57 = load ptr, ptr %56, align 8
  %58 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  %59 = load ptr, ptr %58, align 8
  %60 = call i32 %59(ptr %57, i32 %54)
  %61 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %62 = getelementptr { i8, { i32 } }, ptr %61, i32 0, i32 0
  store i8 0, ptr %62, align 1
  %63 = getelementptr { i8, { i32 } }, ptr %61, i32 0, i32 1, i32 0
  store i32 %60, ptr %63, align 4
  %64 = getelementptr { i8, <4 x i8> }, ptr %61, i32 0, i32 0
  %65 = load i8, ptr %64, align 1
  switch i8 %65, label %switch_default_23 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  %66 = getelementptr { i8, { i32 } }, ptr %61, i32 0, i32 1
  %67 = getelementptr { i32 }, ptr %66, i32 0, i32 0
  %68 = load i32, ptr %67, align 4
  switch i32 %68, label %switch-unboxed_default_1 [
    i32 0, label %switch-unboxed_branch_0_i32_1
  ]

switch-unboxed_branch_0_i32_1:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %69 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %70 = getelementptr { i8, {} }, ptr %69, i32 0, i32 0
  store i8 0, ptr %70, align 1
  %71 = getelementptr { i8, {} }, ptr %69, i32 0, i32 0
  %72 = load i8, ptr %71, align 1
  switch i8 %72, label %switch_default_5 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_1"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_1": ; preds = %switch-unboxed_branch_0_i32_1
  %73 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %74 = getelementptr { i8, { ptr } }, ptr %73, i32 0, i32 0
  store i8 0, ptr %74, align 1
  %75 = getelementptr { i8, { ptr } }, ptr %73, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %75, align 8
  %76 = getelementptr { i8, <8 x i8> }, ptr %73, i32 0, i32 0
  %77 = load i8, ptr %76, align 1
  switch i8 %77, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_1"
  %78 = getelementptr { i8, { ptr } }, ptr %73, i32 0, i32 1
  %79 = getelementptr { ptr }, ptr %78, i32 0, i32 0
  %80 = load ptr, ptr %79, align 8
  %81 = call ptr @malgo_print_string(ptr %80)
  %82 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %83 = getelementptr { i8, {} }, ptr %82, i32 0, i32 0
  store i8 0, ptr %83, align 1
  %84 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %85 = getelementptr { i8, {} }, ptr %84, i32 0, i32 0
  store i8 0, ptr %85, align 1
  %86 = call ptr @malgo_newline(ptr %84)
  ret ptr %86

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_1"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_1": ; preds = %switch-unboxed_branch_0_i32_1
  %87 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %88 = getelementptr { i8, { ptr } }, ptr %87, i32 0, i32 0
  store i8 0, ptr %88, align 1
  %89 = getelementptr { i8, { ptr } }, ptr %87, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %89, align 8
  %90 = getelementptr { i8, <8 x i8> }, ptr %87, i32 0, i32 0
  %91 = load i8, ptr %90, align 1
  switch i8 %91, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_3": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_1"
  %92 = getelementptr { i8, { ptr } }, ptr %87, i32 0, i32 1
  %93 = getelementptr { ptr }, ptr %92, i32 0, i32 0
  %94 = load ptr, ptr %93, align 8
  %95 = call ptr @malgo_print_string(ptr %94)
  %96 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %97 = getelementptr { i8, {} }, ptr %96, i32 0, i32 0
  store i8 0, ptr %97, align 1
  %98 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %99 = getelementptr { i8, {} }, ptr %98, i32 0, i32 0
  store i8 0, ptr %99, align 1
  %100 = call ptr @malgo_newline(ptr %98)
  ret ptr %100

switch_default_4:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_1"
  unreachable

switch_default_5:                                 ; preds = %switch-unboxed_branch_0_i32_1
  unreachable

switch-unboxed_default_1:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %101 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %102 = getelementptr { i8, { i32 } }, ptr %101, i32 0, i32 0
  store i8 0, ptr %102, align 1
  %103 = getelementptr { i8, { i32 } }, ptr %101, i32 0, i32 1, i32 0
  store i32 1, ptr %103, align 4
  %104 = getelementptr { i8, <4 x i8> }, ptr %61, i32 0, i32 0
  %105 = load i8, ptr %104, align 1
  switch i8 %105, label %switch_default_22 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4": ; preds = %switch-unboxed_default_1
  %106 = getelementptr { i8, { i32 } }, ptr %61, i32 0, i32 1
  %107 = getelementptr { i32 }, ptr %106, i32 0, i32 0
  %108 = load i32, ptr %107, align 4
  %109 = getelementptr { i8, <4 x i8> }, ptr %101, i32 0, i32 0
  %110 = load i8, ptr %109, align 1
  switch i8 %110, label %switch_default_21 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  %111 = getelementptr { i8, { i32 } }, ptr %101, i32 0, i32 1
  %112 = getelementptr { i32 }, ptr %111, i32 0, i32 0
  %113 = load i32, ptr %112, align 4
  %114 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %108)
  %115 = getelementptr { ptr, ptr }, ptr %114, i32 0, i32 0
  %116 = load ptr, ptr %115, align 8
  %117 = getelementptr { ptr, ptr }, ptr %114, i32 0, i32 1
  %118 = load ptr, ptr %117, align 8
  %119 = call i32 %118(ptr %116, i32 %113)
  %120 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %119)
  %121 = getelementptr { i8, <4 x i8> }, ptr %120, i32 0, i32 0
  %122 = load i8, ptr %121, align 1
  switch i8 %122, label %switch_default_20 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %123 = getelementptr { i8, { i32 } }, ptr %120, i32 0, i32 1
  %124 = getelementptr { i32 }, ptr %123, i32 0, i32 0
  %125 = load i32, ptr %124, align 4
  switch i32 %125, label %switch-unboxed_default_2 [
    i32 0, label %switch-unboxed_branch_0_i32_2
  ]

switch-unboxed_branch_0_i32_2:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %126 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %127 = getelementptr { i8, {} }, ptr %126, i32 0, i32 0
  store i8 1, ptr %127, align 1
  %128 = getelementptr { i8, {} }, ptr %126, i32 0, i32 0
  %129 = load i8, ptr %128, align 1
  switch i8 %129, label %switch_default_8 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_2"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_2": ; preds = %switch-unboxed_branch_0_i32_2
  %130 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %131 = getelementptr { i8, { ptr } }, ptr %130, i32 0, i32 0
  store i8 0, ptr %131, align 1
  %132 = getelementptr { i8, { ptr } }, ptr %130, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %132, align 8
  %133 = getelementptr { i8, <8 x i8> }, ptr %130, i32 0, i32 0
  %134 = load i8, ptr %133, align 1
  switch i8 %134, label %switch_default_6 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_4": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_2"
  %135 = getelementptr { i8, { ptr } }, ptr %130, i32 0, i32 1
  %136 = getelementptr { ptr }, ptr %135, i32 0, i32 0
  %137 = load ptr, ptr %136, align 8
  %138 = call ptr @malgo_print_string(ptr %137)
  %139 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %140 = getelementptr { i8, {} }, ptr %139, i32 0, i32 0
  store i8 0, ptr %140, align 1
  %141 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %142 = getelementptr { i8, {} }, ptr %141, i32 0, i32 0
  store i8 0, ptr %142, align 1
  %143 = call ptr @malgo_newline(ptr %141)
  ret ptr %143

switch_default_6:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_2"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_2": ; preds = %switch-unboxed_branch_0_i32_2
  %144 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %145 = getelementptr { i8, { ptr } }, ptr %144, i32 0, i32 0
  store i8 0, ptr %145, align 1
  %146 = getelementptr { i8, { ptr } }, ptr %144, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %146, align 8
  %147 = getelementptr { i8, <8 x i8> }, ptr %144, i32 0, i32 0
  %148 = load i8, ptr %147, align 1
  switch i8 %148, label %switch_default_7 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_2"
  %149 = getelementptr { i8, { ptr } }, ptr %144, i32 0, i32 1
  %150 = getelementptr { ptr }, ptr %149, i32 0, i32 0
  %151 = load ptr, ptr %150, align 8
  %152 = call ptr @malgo_print_string(ptr %151)
  %153 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %154 = getelementptr { i8, {} }, ptr %153, i32 0, i32 0
  store i8 0, ptr %154, align 1
  %155 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %156 = getelementptr { i8, {} }, ptr %155, i32 0, i32 0
  store i8 0, ptr %156, align 1
  %157 = call ptr @malgo_newline(ptr %155)
  ret ptr %157

switch_default_7:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_2"
  unreachable

switch_default_8:                                 ; preds = %switch-unboxed_branch_0_i32_2
  unreachable

switch-unboxed_default_2:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %158 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %120, ptr %d_0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %158, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %158, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4779", ptr %let_func_1, align 8
  %159 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %160 = getelementptr { i8, { i32 } }, ptr %159, i32 0, i32 0
  store i8 0, ptr %160, align 1
  %161 = getelementptr { i8, { i32 } }, ptr %159, i32 0, i32 1, i32 0
  store i32 1, ptr %161, align 4
  %162 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %120, ptr %159)
  %163 = getelementptr { i8, <4 x i8> }, ptr %162, i32 0, i32 0
  %164 = load i8, ptr %163, align 1
  switch i8 %164, label %switch_default_19 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7": ; preds = %switch-unboxed_default_2
  %165 = getelementptr { i8, { i32 } }, ptr %162, i32 0, i32 1
  %166 = getelementptr { i32 }, ptr %165, i32 0, i32 0
  %167 = load i32, ptr %166, align 4
  switch i32 %167, label %switch-unboxed_default_3 [
    i32 0, label %switch-unboxed_branch_0_i32_3
  ]

switch-unboxed_branch_0_i32_3:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  %168 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %169 = getelementptr { i8, {} }, ptr %168, i32 0, i32 0
  store i8 0, ptr %169, align 1
  %170 = getelementptr { i8, {} }, ptr %168, i32 0, i32 0
  %171 = load i8, ptr %170, align 1
  switch i8 %171, label %switch_default_11 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_3"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_3": ; preds = %switch-unboxed_branch_0_i32_3
  %172 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %173 = getelementptr { i8, { ptr } }, ptr %172, i32 0, i32 0
  store i8 0, ptr %173, align 1
  %174 = getelementptr { i8, { ptr } }, ptr %172, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %174, align 8
  %175 = getelementptr { i8, <8 x i8> }, ptr %172, i32 0, i32 0
  %176 = load i8, ptr %175, align 1
  switch i8 %176, label %switch_default_9 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_6"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_6": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_3"
  %177 = getelementptr { i8, { ptr } }, ptr %172, i32 0, i32 1
  %178 = getelementptr { ptr }, ptr %177, i32 0, i32 0
  %179 = load ptr, ptr %178, align 8
  %180 = call ptr @malgo_print_string(ptr %179)
  %181 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %182 = getelementptr { i8, {} }, ptr %181, i32 0, i32 0
  store i8 0, ptr %182, align 1
  %183 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %184 = getelementptr { i8, {} }, ptr %183, i32 0, i32 0
  store i8 0, ptr %184, align 1
  %185 = call ptr @malgo_newline(ptr %183)
  ret ptr %185

switch_default_9:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_3"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_3": ; preds = %switch-unboxed_branch_0_i32_3
  %186 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %187 = getelementptr { i8, { ptr } }, ptr %186, i32 0, i32 0
  store i8 0, ptr %187, align 1
  %188 = getelementptr { i8, { ptr } }, ptr %186, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %188, align 8
  %189 = getelementptr { i8, <8 x i8> }, ptr %186, i32 0, i32 0
  %190 = load i8, ptr %189, align 1
  switch i8 %190, label %switch_default_10 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_7"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_7": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_3"
  %191 = getelementptr { i8, { ptr } }, ptr %186, i32 0, i32 1
  %192 = getelementptr { ptr }, ptr %191, i32 0, i32 0
  %193 = load ptr, ptr %192, align 8
  %194 = call ptr @malgo_print_string(ptr %193)
  %195 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %196 = getelementptr { i8, {} }, ptr %195, i32 0, i32 0
  store i8 0, ptr %196, align 1
  %197 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %198 = getelementptr { i8, {} }, ptr %197, i32 0, i32 0
  store i8 0, ptr %198, align 1
  %199 = call ptr @malgo_newline(ptr %197)
  ret ptr %199

switch_default_10:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_3"
  unreachable

switch_default_11:                                ; preds = %switch-unboxed_branch_0_i32_3
  unreachable

switch-unboxed_default_3:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  %200 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %162, ptr %d_1, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %200, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %200, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4780", ptr %let_func_2, align 8
  %201 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %202 = getelementptr { i8, { i32 } }, ptr %201, i32 0, i32 0
  store i8 0, ptr %202, align 1
  %203 = getelementptr { i8, { i32 } }, ptr %201, i32 0, i32 1, i32 0
  store i32 1, ptr %203, align 4
  %204 = getelementptr { ptr, ptr }, ptr %200, i32 0, i32 0
  %205 = load ptr, ptr %204, align 8
  %206 = getelementptr { ptr, ptr }, ptr %200, i32 0, i32 1
  %207 = load ptr, ptr %206, align 8
  %208 = call ptr %207(ptr %205, ptr %201)
  %209 = getelementptr { i8, <4 x i8> }, ptr %208, i32 0, i32 0
  %210 = load i8, ptr %209, align 1
  switch i8 %210, label %switch_default_18 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8": ; preds = %switch-unboxed_default_3
  %211 = getelementptr { i8, { i32 } }, ptr %208, i32 0, i32 1
  %212 = getelementptr { i32 }, ptr %211, i32 0, i32 0
  %213 = load i32, ptr %212, align 4
  switch i32 %213, label %switch-unboxed_default_4 [
    i32 0, label %switch-unboxed_branch_0_i32_4
  ]

switch-unboxed_branch_0_i32_4:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  %214 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  %215 = getelementptr { i8, {} }, ptr %214, i32 0, i32 0
  %216 = load i8, ptr %215, align 1
  switch i8 %216, label %switch_default_14 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_4"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_4": ; preds = %switch-unboxed_branch_0_i32_4
  %217 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %218 = getelementptr { i8, { ptr } }, ptr %217, i32 0, i32 0
  store i8 0, ptr %218, align 1
  %219 = getelementptr { i8, { ptr } }, ptr %217, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %219, align 8
  %220 = getelementptr { i8, <8 x i8> }, ptr %217, i32 0, i32 0
  %221 = load i8, ptr %220, align 1
  switch i8 %221, label %switch_default_12 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_8"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_8": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_4"
  %222 = getelementptr { i8, { ptr } }, ptr %217, i32 0, i32 1
  %223 = getelementptr { ptr }, ptr %222, i32 0, i32 0
  %224 = load ptr, ptr %223, align 8
  %225 = call ptr @malgo_print_string(ptr %224)
  %226 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %227 = getelementptr { i8, {} }, ptr %226, i32 0, i32 0
  store i8 0, ptr %227, align 1
  %228 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %229 = getelementptr { i8, {} }, ptr %228, i32 0, i32 0
  store i8 0, ptr %229, align 1
  %230 = call ptr @malgo_newline(ptr %228)
  ret ptr %230

switch_default_12:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_4"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_4": ; preds = %switch-unboxed_branch_0_i32_4
  %231 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %232 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 0
  store i8 0, ptr %232, align 1
  %233 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %233, align 8
  %234 = getelementptr { i8, <8 x i8> }, ptr %231, i32 0, i32 0
  %235 = load i8, ptr %234, align 1
  switch i8 %235, label %switch_default_13 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_9"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_9": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_4"
  %236 = getelementptr { i8, { ptr } }, ptr %231, i32 0, i32 1
  %237 = getelementptr { ptr }, ptr %236, i32 0, i32 0
  %238 = load ptr, ptr %237, align 8
  %239 = call ptr @malgo_print_string(ptr %238)
  %240 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %241 = getelementptr { i8, {} }, ptr %240, i32 0, i32 0
  store i8 0, ptr %241, align 1
  %242 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %243 = getelementptr { i8, {} }, ptr %242, i32 0, i32 0
  store i8 0, ptr %243, align 1
  %244 = call ptr @malgo_newline(ptr %242)
  ret ptr %244

switch_default_13:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_4"
  unreachable

switch_default_14:                                ; preds = %switch-unboxed_branch_0_i32_4
  unreachable

switch-unboxed_default_4:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  %245 = call ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr null, ptr %208)
  %246 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 1)
  %247 = getelementptr { ptr, ptr }, ptr %245, i32 0, i32 0
  %248 = load ptr, ptr %247, align 8
  %249 = getelementptr { ptr, ptr }, ptr %245, i32 0, i32 1
  %250 = load ptr, ptr %249, align 8
  %251 = call ptr %250(ptr %248, ptr %246)
  %252 = call ptr @"test/testcases/malgo/EvenOdd.mlg.odd"(ptr null, ptr %251)
  %253 = getelementptr { i8, {} }, ptr %252, i32 0, i32 0
  %254 = load i8, ptr %253, align 1
  switch i8 %254, label %switch_default_17 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.False_5"
    i8 1, label %"switch_branch_runtime/malgo/Builtin.mlg.True_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.False_5": ; preds = %switch-unboxed_default_4
  %255 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %256 = getelementptr { i8, { ptr } }, ptr %255, i32 0, i32 0
  store i8 0, ptr %256, align 1
  %257 = getelementptr { i8, { ptr } }, ptr %255, i32 0, i32 1, i32 0
  store ptr @str4776, ptr %257, align 8
  %258 = getelementptr { i8, <8 x i8> }, ptr %255, i32 0, i32 0
  %259 = load i8, ptr %258, align 1
  switch i8 %259, label %switch_default_15 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_10"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_10": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_5"
  %260 = getelementptr { i8, { ptr } }, ptr %255, i32 0, i32 1
  %261 = getelementptr { ptr }, ptr %260, i32 0, i32 0
  %262 = load ptr, ptr %261, align 8
  %263 = call ptr @malgo_print_string(ptr %262)
  %264 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %265 = getelementptr { i8, {} }, ptr %264, i32 0, i32 0
  store i8 0, ptr %265, align 1
  %266 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %267 = getelementptr { i8, {} }, ptr %266, i32 0, i32 0
  store i8 0, ptr %267, align 1
  %268 = call ptr @malgo_newline(ptr %266)
  ret ptr %268

switch_default_15:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_5"
  unreachable

"switch_branch_runtime/malgo/Builtin.mlg.True_5": ; preds = %switch-unboxed_default_4
  %269 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %270 = getelementptr { i8, { ptr } }, ptr %269, i32 0, i32 0
  store i8 0, ptr %270, align 1
  %271 = getelementptr { i8, { ptr } }, ptr %269, i32 0, i32 1, i32 0
  store ptr @str4777, ptr %271, align 8
  %272 = getelementptr { i8, <8 x i8> }, ptr %269, i32 0, i32 0
  %273 = load i8, ptr %272, align 1
  switch i8 %273, label %switch_default_16 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_11"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_11": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_5"
  %274 = getelementptr { i8, { ptr } }, ptr %269, i32 0, i32 1
  %275 = getelementptr { ptr }, ptr %274, i32 0, i32 0
  %276 = load ptr, ptr %275, align 8
  %277 = call ptr @malgo_print_string(ptr %276)
  %278 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %279 = getelementptr { i8, {} }, ptr %278, i32 0, i32 0
  store i8 0, ptr %279, align 1
  %280 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %281 = getelementptr { i8, {} }, ptr %280, i32 0, i32 0
  store i8 0, ptr %281, align 1
  %282 = call ptr @malgo_newline(ptr %280)
  ret ptr %282

switch_default_16:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.True_5"
  unreachable

switch_default_17:                                ; preds = %switch-unboxed_default_4
  unreachable

switch_default_18:                                ; preds = %switch-unboxed_default_3
  unreachable

switch_default_19:                                ; preds = %switch-unboxed_default_2
  unreachable

switch_default_20:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  unreachable

switch_default_21:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  unreachable

switch_default_22:                                ; preds = %switch-unboxed_default_1
  unreachable

switch_default_23:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  unreachable

switch_default_24:                                ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  unreachable

switch_default_25:                                ; preds = %switch-unboxed_default_0
  unreachable

switch_default_26:                                ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4781"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4690"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4781", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4782"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4700"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4783"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4784"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4701"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4785"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.even"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_11 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  switch i32 %6, label %switch-unboxed_default_0 [
    i32 0, label %switch-unboxed_branch_0_i32_0
  ]

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 1, ptr %8, align 1
  ret ptr %7

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 1, ptr %11, align 4
  %12 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_10 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %switch-unboxed_default_0
  %14 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_51_0", i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = getelementptr { i8, <4 x i8> }, ptr %9, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_9 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4782", ptr %let_func_0, align 8
  %23 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %25 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 0
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1, i32 0
  store i32 %23, ptr %26, align 4
  %27 = getelementptr { i8, <4 x i8> }, ptr %24, i32 0, i32 0
  %28 = load i8, ptr %27, align 1
  switch i8 %28, label %switch_default_8 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  %29 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1
  %30 = getelementptr { i32 }, ptr %29, i32 0, i32 0
  %31 = load i32, ptr %30, align 4
  switch i32 %31, label %switch-unboxed_default_1 [
    i32 0, label %switch-unboxed_branch_0_i32_1
  ]

switch-unboxed_branch_0_i32_1:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, {} }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  ret ptr %32

switch-unboxed_default_1:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1, i32 0
  store i32 1, ptr %36, align 4
  %37 = getelementptr { i8, <4 x i8> }, ptr %24, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_7 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4": ; preds = %switch-unboxed_default_1
  %39 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1
  %40 = getelementptr { i32 }, ptr %39, i32 0, i32 0
  %41 = load i32, ptr %40, align 4
  %42 = getelementptr { i8, <4 x i8> }, ptr %34, i32 0, i32 0
  %43 = load i8, ptr %42, align 1
  switch i8 %43, label %switch_default_6 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  %44 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1
  %45 = getelementptr { i32 }, ptr %44, i32 0, i32 0
  %46 = load i32, ptr %45, align 4
  %47 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32 }, ptr %let_capture_2, i32 0, i32 0
  store i32 %41, ptr %p_1, align 4
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4783", ptr %let_func_1, align 8
  %48 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  %49 = load ptr, ptr %48, align 8
  %50 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  %51 = load ptr, ptr %50, align 8
  %52 = call i32 %51(ptr %49, i32 %46)
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %54 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 0
  store i8 0, ptr %54, align 1
  %55 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1, i32 0
  store i32 %52, ptr %55, align 4
  %56 = getelementptr { i8, <4 x i8> }, ptr %53, i32 0, i32 0
  %57 = load i8, ptr %56, align 1
  switch i8 %57, label %switch_default_5 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %58 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1
  %59 = getelementptr { i32 }, ptr %58, i32 0, i32 0
  %60 = load i32, ptr %59, align 4
  switch i32 %60, label %switch-unboxed_default_2 [
    i32 0, label %switch-unboxed_branch_0_i32_2
  ]

switch-unboxed_branch_0_i32_2:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %61 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %62 = getelementptr { i8, {} }, ptr %61, i32 0, i32 0
  store i8 1, ptr %62, align 1
  ret ptr %61

switch-unboxed_default_2:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %63 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %64 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 0
  store i8 0, ptr %64, align 1
  %65 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 1, i32 0
  store i32 1, ptr %65, align 4
  %66 = getelementptr { i8, <4 x i8> }, ptr %53, i32 0, i32 0
  %67 = load i8, ptr %66, align 1
  switch i8 %67, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7": ; preds = %switch-unboxed_default_2
  %68 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1
  %69 = getelementptr { i32 }, ptr %68, i32 0, i32 0
  %70 = load i32, ptr %69, align 4
  %71 = getelementptr { i8, <4 x i8> }, ptr %63, i32 0, i32 0
  %72 = load i8, ptr %71, align 1
  switch i8 %72, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  %73 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 1
  %74 = getelementptr { i32 }, ptr %73, i32 0, i32 0
  %75 = load i32, ptr %74, align 4
  %76 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %70)
  %77 = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 0
  %78 = load ptr, ptr %77, align 8
  %79 = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 1
  %80 = load ptr, ptr %79, align 8
  %81 = call i32 %80(ptr %78, i32 %75)
  %82 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %81)
  %83 = getelementptr { i8, <4 x i8> }, ptr %82, i32 0, i32 0
  %84 = load i8, ptr %83, align 1
  switch i8 %84, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  %85 = getelementptr { i8, { i32 } }, ptr %82, i32 0, i32 1
  %86 = getelementptr { i32 }, ptr %85, i32 0, i32 0
  %87 = load i32, ptr %86, align 4
  switch i32 %87, label %switch-unboxed_default_3 [
    i32 0, label %switch-unboxed_branch_0_i32_3
  ]

switch-unboxed_branch_0_i32_3:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  %88 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %89 = getelementptr { i8, {} }, ptr %88, i32 0, i32 0
  store i8 0, ptr %89, align 1
  ret ptr %88

switch-unboxed_default_3:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  %90 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %82, ptr %d_0, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %90, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %90, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4784", ptr %let_func_2, align 8
  %91 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %92 = getelementptr { i8, { i32 } }, ptr %91, i32 0, i32 0
  store i8 0, ptr %92, align 1
  %93 = getelementptr { i8, { i32 } }, ptr %91, i32 0, i32 1, i32 0
  store i32 1, ptr %93, align 4
  %94 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %82, ptr %91)
  %95 = getelementptr { i8, <4 x i8> }, ptr %94, i32 0, i32 0
  %96 = load i8, ptr %95, align 1
  switch i8 %96, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10": ; preds = %switch-unboxed_default_3
  %97 = getelementptr { i8, { i32 } }, ptr %94, i32 0, i32 1
  %98 = getelementptr { i32 }, ptr %97, i32 0, i32 0
  %99 = load i32, ptr %98, align 4
  switch i32 %99, label %switch-unboxed_default_4 [
    i32 0, label %switch-unboxed_branch_0_i32_4
  ]

switch-unboxed_branch_0_i32_4:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  %100 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %101 = getelementptr { i8, {} }, ptr %100, i32 0, i32 0
  store i8 1, ptr %101, align 1
  ret ptr %100

switch-unboxed_default_4:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  %102 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %94, ptr %d_1, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4785", ptr %let_func_3, align 8
  %103 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %104 = getelementptr { i8, { i32 } }, ptr %103, i32 0, i32 0
  store i8 0, ptr %104, align 1
  %105 = getelementptr { i8, { i32 } }, ptr %103, i32 0, i32 1, i32 0
  store i32 1, ptr %105, align 4
  %106 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 0
  %107 = load ptr, ptr %106, align 8
  %108 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 1
  %109 = load ptr, ptr %108, align 8
  %110 = call ptr %109(ptr %107, ptr %103)
  %111 = getelementptr { i8, <4 x i8> }, ptr %110, i32 0, i32 0
  %112 = load i8, ptr %111, align 1
  switch i8 %112, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11": ; preds = %switch-unboxed_default_4
  %113 = getelementptr { i8, { i32 } }, ptr %110, i32 0, i32 1
  %114 = getelementptr { i32 }, ptr %113, i32 0, i32 0
  %115 = load i32, ptr %114, align 4
  switch i32 %115, label %switch-unboxed_default_5 [
    i32 0, label %switch-unboxed_branch_0_i32_5
  ]

switch-unboxed_branch_0_i32_5:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  %116 = call ptr @"runtime/malgo/Builtin.mlg.False"(ptr null)
  ret ptr %116

switch-unboxed_default_5:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  %117 = call ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr null, ptr %110)
  %118 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 1)
  %119 = getelementptr { ptr, ptr }, ptr %117, i32 0, i32 0
  %120 = load ptr, ptr %119, align 8
  %121 = getelementptr { ptr, ptr }, ptr %117, i32 0, i32 1
  %122 = load ptr, ptr %121, align 8
  %123 = call ptr %122(ptr %120, ptr %118)
  %124 = call ptr @"test/testcases/malgo/EvenOdd.mlg.even"(ptr null, ptr %123)
  ret ptr %124

switch_default_0:                                 ; preds = %switch-unboxed_default_4
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_3
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  unreachable

switch_default_4:                                 ; preds = %switch-unboxed_default_2
  unreachable

switch_default_5:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  unreachable

switch_default_7:                                 ; preds = %switch-unboxed_default_1
  unreachable

switch_default_8:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  unreachable

switch_default_9:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  unreachable

switch_default_10:                                ; preds = %switch-unboxed_default_0
  unreachable

switch_default_11:                                ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4786"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4737"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4787"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4788"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4738"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4789"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.odd"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_11 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  switch i32 %6, label %switch-unboxed_default_0 [
    i32 0, label %switch-unboxed_branch_0_i32_0
  ]

switch-unboxed_branch_0_i32_0:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  ret ptr %7

switch-unboxed_default_0:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 1, ptr %11, align 4
  %12 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_10 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %switch-unboxed_default_0
  %14 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_61_0", i32 0, i32 1
  %15 = getelementptr { i32 }, ptr %14, i32 0, i32 0
  %16 = load i32, ptr %15, align 4
  %17 = getelementptr { i8, <4 x i8> }, ptr %9, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_9 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
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
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4786", ptr %let_func_0, align 8
  %23 = call i32 @"runtime/malgo/Builtin.mlg.$subInt32#_curry_2292"(ptr null, i32 %16, i32 %21)
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %25 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 0
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1, i32 0
  store i32 %23, ptr %26, align 4
  %27 = getelementptr { i8, <4 x i8> }, ptr %24, i32 0, i32 0
  %28 = load i8, ptr %27, align 1
  switch i8 %28, label %switch_default_8 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  %29 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1
  %30 = getelementptr { i32 }, ptr %29, i32 0, i32 0
  %31 = load i32, ptr %30, align 4
  switch i32 %31, label %switch-unboxed_default_1 [
    i32 0, label %switch-unboxed_branch_0_i32_1
  ]

switch-unboxed_branch_0_i32_1:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, {} }, ptr %32, i32 0, i32 0
  store i8 1, ptr %33, align 1
  ret ptr %32

switch-unboxed_default_1:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_3"
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1, i32 0
  store i32 1, ptr %36, align 4
  %37 = getelementptr { i8, <4 x i8> }, ptr %24, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_7 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4": ; preds = %switch-unboxed_default_1
  %39 = getelementptr { i8, { i32 } }, ptr %24, i32 0, i32 1
  %40 = getelementptr { i32 }, ptr %39, i32 0, i32 0
  %41 = load i32, ptr %40, align 4
  %42 = getelementptr { i8, <4 x i8> }, ptr %34, i32 0, i32 0
  %43 = load i8, ptr %42, align 1
  switch i8 %43, label %switch_default_6 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  %44 = getelementptr { i8, { i32 } }, ptr %34, i32 0, i32 1
  %45 = getelementptr { i32 }, ptr %44, i32 0, i32 0
  %46 = load i32, ptr %45, align 4
  %47 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32 }, ptr %let_capture_2, i32 0, i32 0
  store i32 %41, ptr %p_1, align 4
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4787", ptr %let_func_1, align 8
  %48 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 0
  %49 = load ptr, ptr %48, align 8
  %50 = getelementptr { ptr, ptr }, ptr %47, i32 0, i32 1
  %51 = load ptr, ptr %50, align 8
  %52 = call i32 %51(ptr %49, i32 %46)
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %54 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 0
  store i8 0, ptr %54, align 1
  %55 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1, i32 0
  store i32 %52, ptr %55, align 4
  %56 = getelementptr { i8, <4 x i8> }, ptr %53, i32 0, i32 0
  %57 = load i8, ptr %56, align 1
  switch i8 %57, label %switch_default_5 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  %58 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1
  %59 = getelementptr { i32 }, ptr %58, i32 0, i32 0
  %60 = load i32, ptr %59, align 4
  switch i32 %60, label %switch-unboxed_default_2 [
    i32 0, label %switch-unboxed_branch_0_i32_2
  ]

switch-unboxed_branch_0_i32_2:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %61 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %62 = getelementptr { i8, {} }, ptr %61, i32 0, i32 0
  store i8 0, ptr %62, align 1
  ret ptr %61

switch-unboxed_default_2:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_6"
  %63 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %64 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 0
  store i8 0, ptr %64, align 1
  %65 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 1, i32 0
  store i32 1, ptr %65, align 4
  %66 = getelementptr { i8, <4 x i8> }, ptr %53, i32 0, i32 0
  %67 = load i8, ptr %66, align 1
  switch i8 %67, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7": ; preds = %switch-unboxed_default_2
  %68 = getelementptr { i8, { i32 } }, ptr %53, i32 0, i32 1
  %69 = getelementptr { i32 }, ptr %68, i32 0, i32 0
  %70 = load i32, ptr %69, align 4
  %71 = getelementptr { i8, <4 x i8> }, ptr %63, i32 0, i32 0
  %72 = load i8, ptr %71, align 1
  switch i8 %72, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  %73 = getelementptr { i8, { i32 } }, ptr %63, i32 0, i32 1
  %74 = getelementptr { i32 }, ptr %73, i32 0, i32 0
  %75 = load i32, ptr %74, align 4
  %76 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %70)
  %77 = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 0
  %78 = load ptr, ptr %77, align 8
  %79 = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 1
  %80 = load ptr, ptr %79, align 8
  %81 = call i32 %80(ptr %78, i32 %75)
  %82 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %81)
  %83 = getelementptr { i8, <4 x i8> }, ptr %82, i32 0, i32 0
  %84 = load i8, ptr %83, align 1
  switch i8 %84, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  %85 = getelementptr { i8, { i32 } }, ptr %82, i32 0, i32 1
  %86 = getelementptr { i32 }, ptr %85, i32 0, i32 0
  %87 = load i32, ptr %86, align 4
  switch i32 %87, label %switch-unboxed_default_3 [
    i32 0, label %switch-unboxed_branch_0_i32_3
  ]

switch-unboxed_branch_0_i32_3:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  %88 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %89 = getelementptr { i8, {} }, ptr %88, i32 0, i32 0
  store i8 1, ptr %89, align 1
  ret ptr %88

switch-unboxed_default_3:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_9"
  %90 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %82, ptr %d_0, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %90, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %90, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4788", ptr %let_func_2, align 8
  %91 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %92 = getelementptr { i8, { i32 } }, ptr %91, i32 0, i32 0
  store i8 0, ptr %92, align 1
  %93 = getelementptr { i8, { i32 } }, ptr %91, i32 0, i32 1, i32 0
  store i32 1, ptr %93, align 4
  %94 = call ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr null, ptr %82, ptr %91)
  %95 = getelementptr { i8, <4 x i8> }, ptr %94, i32 0, i32 0
  %96 = load i8, ptr %95, align 1
  switch i8 %96, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10": ; preds = %switch-unboxed_default_3
  %97 = getelementptr { i8, { i32 } }, ptr %94, i32 0, i32 1
  %98 = getelementptr { i32 }, ptr %97, i32 0, i32 0
  %99 = load i32, ptr %98, align 4
  switch i32 %99, label %switch-unboxed_default_4 [
    i32 0, label %switch-unboxed_branch_0_i32_4
  ]

switch-unboxed_branch_0_i32_4:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  %100 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %101 = getelementptr { i8, {} }, ptr %100, i32 0, i32 0
  store i8 0, ptr %101, align 1
  ret ptr %100

switch-unboxed_default_4:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_10"
  %102 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %94, ptr %d_1, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4789", ptr %let_func_3, align 8
  %103 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %104 = getelementptr { i8, { i32 } }, ptr %103, i32 0, i32 0
  store i8 0, ptr %104, align 1
  %105 = getelementptr { i8, { i32 } }, ptr %103, i32 0, i32 1, i32 0
  store i32 1, ptr %105, align 4
  %106 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 0
  %107 = load ptr, ptr %106, align 8
  %108 = getelementptr { ptr, ptr }, ptr %102, i32 0, i32 1
  %109 = load ptr, ptr %108, align 8
  %110 = call ptr %109(ptr %107, ptr %103)
  %111 = getelementptr { i8, <4 x i8> }, ptr %110, i32 0, i32 0
  %112 = load i8, ptr %111, align 1
  switch i8 %112, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11": ; preds = %switch-unboxed_default_4
  %113 = getelementptr { i8, { i32 } }, ptr %110, i32 0, i32 1
  %114 = getelementptr { i32 }, ptr %113, i32 0, i32 0
  %115 = load i32, ptr %114, align 4
  switch i32 %115, label %switch-unboxed_default_5 [
    i32 0, label %switch-unboxed_branch_0_i32_5
  ]

switch-unboxed_branch_0_i32_5:                    ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  %116 = call ptr @"runtime/malgo/Builtin.mlg.True"(ptr null)
  ret ptr %116

switch-unboxed_default_5:                         ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_11"
  %117 = call ptr @"runtime/malgo/Builtin.mlg.subInt32"(ptr null, ptr %110)
  %118 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 1)
  %119 = getelementptr { ptr, ptr }, ptr %117, i32 0, i32 0
  %120 = load ptr, ptr %119, align 8
  %121 = getelementptr { ptr, ptr }, ptr %117, i32 0, i32 1
  %122 = load ptr, ptr %121, align 8
  %123 = call ptr %122(ptr %120, ptr %118)
  %124 = call ptr @"test/testcases/malgo/EvenOdd.mlg.odd"(ptr null, ptr %123)
  ret ptr %124

switch_default_0:                                 ; preds = %switch-unboxed_default_4
  unreachable

switch_default_1:                                 ; preds = %switch-unboxed_default_3
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_8"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_7"
  unreachable

switch_default_4:                                 ; preds = %switch-unboxed_default_2
  unreachable

switch_default_5:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_5"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_4"
  unreachable

switch_default_7:                                 ; preds = %switch-unboxed_default_1
  unreachable

switch_default_8:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_2"
  unreachable

switch_default_9:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  unreachable

switch_default_10:                                ; preds = %switch-unboxed_default_0
  unreachable

switch_default_11:                                ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4790"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4690"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2300_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2299_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2300_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2300_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4790", ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4701"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3669_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3669_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3669_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4702"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4703"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4704"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3700_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3700_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3700_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4705"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4706"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4707"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3731_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3731_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3731_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4708"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4709"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3582_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4710"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3765_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3765_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3765_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4711"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4712"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4713"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3796_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3796_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3796_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4714"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4715"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4716"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3827_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3827_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3827_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4717"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4718"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3613_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4719"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3861_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3861_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3861_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4720"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4721"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4722"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3892_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3892_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3892_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4723"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4724"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4725"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3923_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3923_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3923_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4726"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4727"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3525_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_3644_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4738"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4122_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4122_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4122_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4739"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4740"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4741"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4153_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4153_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4153_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4742"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4743"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4744"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4184_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4184_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4184_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4745"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4746"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4035_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4747"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4218_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4218_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4218_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4748"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4749"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4750"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4249_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4249_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4249_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4751"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4752"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4753"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4280_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4280_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4280_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4754"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4755"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4066_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4756"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4314_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4314_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4314_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4757"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4758"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4759"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4345_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4345_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4345_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4760"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4761"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4762"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4376_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4376_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4376_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4763"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4764"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_3978_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4097_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4765"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4605_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4605_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4605_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4766"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4767"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4768"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4636_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4636_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4636_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4769"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4770"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4771"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4667_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4667_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4667_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4772"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/EvenOdd.mlg.$raw_let_4773"(ptr %0, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$d_4450_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/EvenOdd.mlg.$int32#_4575_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"runtime/malgo/Builtin.mlg.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$subInt32_curry_2308"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2309_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2310_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/EvenOdd.mlg.#let_closure_4791", ptr %let_func_0, align 8
  %13 = call i32 @"runtime/malgo/Builtin.mlg.$malgo_sub_int32_t_curry_1816"(ptr null, i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/EvenOdd.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/EvenOdd.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/EvenOdd.mlg"() {
  ret void
}
