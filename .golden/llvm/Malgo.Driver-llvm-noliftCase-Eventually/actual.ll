; ModuleID = './test/testcases/malgo/Eventually.mlg'
source_filename = "./test/testcases/malgo/Eventually.mlg"

@Eventually.eventually = global ptr undef
@Eventually.comp = global ptr undef
@str3290 = unnamed_addr constant [7 x i8] c"return\00"
@str3291 = unnamed_addr constant [5 x i8] c"bind\00"
@str3295 = unnamed_addr constant [8 x i8] c"not yet\00"
@str3331 = unnamed_addr constant [2 x i8] c"1\00"
@str3334 = unnamed_addr constant [2 x i8] c"2\00"

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

define internal ptr @"Eventually.#fun_closure_3289"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %k_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %k_0 = load ptr, ptr %k_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, {} }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { ptr, ptr }, ptr %p_0, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %p_0, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %3)
  %10 = call ptr @"Eventually.$eventuallyBind_curry_288"(ptr null, ptr %9, ptr %k_0)
  ret ptr %10
}

define internal ptr @"Eventually.$eventuallyBind_curry_288"(ptr %0, ptr %"Eventually.$done_289_0", ptr %"Eventually.$k_290_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Eventually.$done_289_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Eventually.$done_289_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %"Eventually.$k_290_0", i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %"Eventually.$k_290_0", i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %6)
  ret ptr %11

switch_branch_Eventually.NotYetDone_0:            ; preds = %1
  %12 = getelementptr { i8, { ptr } }, ptr %"Eventually.$done_289_0", i32 0, i32 1
  %13 = getelementptr { ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %14, ptr %p_0, align 8
  %k_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"Eventually.$k_290_0", ptr %k_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3289", ptr %fun_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, { ptr } }, ptr %16, i32 0, i32 0
  store i8 1, ptr %17, align 1
  %18 = getelementptr { i8, { ptr } }, ptr %16, i32 0, i32 1, i32 0
  store ptr %15, ptr %18, align 8
  ret ptr %16

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Eventually.Done(ptr %0, ptr %"Eventually.$p_258_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Eventually.$p_258_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @Eventually.NotYetDone(ptr %0, ptr %"Eventually.$p_260_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Eventually.$p_260_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @Eventually.step(ptr %0, ptr %"Eventually.$done_262_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Eventually.$done_262_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Eventually.$done_262_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  ret ptr %7

switch_branch_Eventually.NotYetDone_0:            ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %"Eventually.$done_262_0", i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr %18(ptr %16, ptr %13)
  ret ptr %19

switch_default_0:                                 ; preds = %1
  unreachable
}

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @Eventually.return(ptr %0, ptr %"Eventually.$record_269_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"Eventually.$record_269_0", ptr @str3290)
  %3 = call ptr @malgo_hash_table_get(ptr %"Eventually.$record_269_0", ptr @str3291)
  ret ptr %2
}

define internal ptr @"Eventually.#let_closure_3292"(ptr %0, ptr %1) {
  %done_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %done_0 = load ptr, ptr %done_addr_0, align 8
  %3 = call ptr @"Eventually.$eventuallyBind_curry_288"(ptr null, ptr %done_0, ptr %1)
  ret ptr %3
}

define internal ptr @Eventually.eventuallyBind(ptr %0, ptr %"Eventually.$done_272_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %done_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Eventually.$done_272_0", ptr %done_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3292", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Eventually.bind(ptr %0, ptr %"Eventually.$record_308_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"Eventually.$record_308_0", ptr @str3290)
  %3 = call ptr @malgo_hash_table_get(ptr %"Eventually.$record_308_0", ptr @str3291)
  ret ptr %3
}

define internal ptr @"Eventually.#let_closure_3293"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3294"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3296"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3297"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3298"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3299"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3300"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3301"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3302"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3303"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3304"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3305"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3306"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3307"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3308"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3309"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3310"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3311"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3312"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3313"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3314"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3315"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3316"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3317"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3318"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3319"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3320"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3321"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3322"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3323"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#let_closure_3324"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Eventually.#fun_closure_3325"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_0:                  ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <4 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Eventually.Done_0
  %10 = getelementptr { i8, { i32 } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_int32_t_to_string(i32 %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, <8 x i8> }, ptr %14, i32 0, i32 0
  %18 = load i8, ptr %17, align 1
  switch i8 %18, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %19 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1
  %20 = getelementptr { ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr @malgo_print_string(ptr %21)
  ret ptr %22

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_0:            ; preds = %2
  %23 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr @str3295, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %switch_branch_Eventually.NotYetDone_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_print_string(ptr %33)
  ret ptr %34

switch_default_2:                                 ; preds = %switch_branch_Eventually.NotYetDone_0
  unreachable

switch_default_3:                                 ; preds = %2
  unreachable
}

define internal ptr @Eventually.main(ptr %0, ptr %"Eventually.$$__355_0") {
  %2 = load ptr, ptr @Eventually.comp, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_14 [
    i8 0, label %switch_branch_Eventually.Done_0
    i8 1, label %switch_branch_Eventually.NotYetDone_7
  ]

switch_branch_Eventually.Done_0:                  ; preds = %1
  %5 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = load ptr, ptr @Eventually.comp, align 8
  %12 = getelementptr { i8, <8 x i8> }, ptr %11, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_6 [
    i8 0, label %switch_branch_Eventually.Done_1
    i8 1, label %switch_branch_Eventually.NotYetDone_3
  ]

switch_branch_Eventually.Done_1:                  ; preds = %switch_branch_Eventually.Done_0
  %14 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1
  %15 = getelementptr { ptr }, ptr %14, i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %16, ptr %19, align 8
  %20 = getelementptr { i8, <8 x i8> }, ptr %17, i32 0, i32 0
  %21 = load i8, ptr %20, align 1
  switch i8 %21, label %switch_default_2 [
    i8 0, label %switch_branch_Eventually.Done_2
    i8 1, label %switch_branch_Eventually.NotYetDone_1
  ]

switch_branch_Eventually.Done_2:                  ; preds = %switch_branch_Eventually.Done_1
  %22 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 1
  %23 = getelementptr { ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1, i32 0
  store ptr %24, ptr %27, align 8
  %28 = getelementptr { i8, <8 x i8> }, ptr %25, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_0 [
    i8 0, label %switch_branch_Eventually.Done_3
    i8 1, label %switch_branch_Eventually.NotYetDone_0
  ]

switch_branch_Eventually.Done_3:                  ; preds = %switch_branch_Eventually.Done_2
  %30 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1
  %31 = getelementptr { ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %34 = getelementptr { i8, { ptr } }, ptr %33, i32 0, i32 0
  store i8 0, ptr %34, align 1
  %35 = getelementptr { i8, { ptr } }, ptr %33, i32 0, i32 1, i32 0
  store ptr %32, ptr %35, align 8
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %33, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3293", ptr %let_func_0, align 8
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3294", ptr %fun_func_0, align 8
  %38 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  %39 = load ptr, ptr %38, align 8
  %40 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr %41(ptr %39, ptr %37)
  ret ptr %42

switch_branch_Eventually.NotYetDone_0:            ; preds = %switch_branch_Eventually.Done_2
  %43 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1
  %44 = getelementptr { ptr }, ptr %43, i32 0, i32 0
  %45 = load ptr, ptr %44, align 8
  %46 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %47 = getelementptr { i8, {} }, ptr %46, i32 0, i32 0
  store i8 0, ptr %47, align 1
  %48 = getelementptr { ptr, ptr }, ptr %45, i32 0, i32 0
  %49 = load ptr, ptr %48, align 8
  %50 = getelementptr { ptr, ptr }, ptr %45, i32 0, i32 1
  %51 = load ptr, ptr %50, align 8
  %52 = call ptr %51(ptr %49, ptr %46)
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %52, ptr %d_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3296", ptr %let_func_1, align 8
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3297", ptr %fun_func_1, align 8
  %55 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = call ptr %58(ptr %56, ptr %54)
  ret ptr %59

switch_default_0:                                 ; preds = %switch_branch_Eventually.Done_2
  unreachable

switch_branch_Eventually.NotYetDone_1:            ; preds = %switch_branch_Eventually.Done_1
  %60 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 1
  %61 = getelementptr { ptr }, ptr %60, i32 0, i32 0
  %62 = load ptr, ptr %61, align 8
  %63 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %64 = getelementptr { i8, {} }, ptr %63, i32 0, i32 0
  store i8 0, ptr %64, align 1
  %65 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 0
  %66 = load ptr, ptr %65, align 8
  %67 = getelementptr { ptr, ptr }, ptr %62, i32 0, i32 1
  %68 = load ptr, ptr %67, align 8
  %69 = call ptr %68(ptr %66, ptr %63)
  %70 = getelementptr { i8, <8 x i8> }, ptr %69, i32 0, i32 0
  %71 = load i8, ptr %70, align 1
  switch i8 %71, label %switch_default_1 [
    i8 0, label %switch_branch_Eventually.Done_4
    i8 1, label %switch_branch_Eventually.NotYetDone_2
  ]

switch_branch_Eventually.Done_4:                  ; preds = %switch_branch_Eventually.NotYetDone_1
  %72 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 1
  %73 = getelementptr { ptr }, ptr %72, i32 0, i32 0
  %74 = load ptr, ptr %73, align 8
  %75 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %76 = getelementptr { i8, { ptr } }, ptr %75, i32 0, i32 0
  store i8 0, ptr %76, align 1
  %77 = getelementptr { i8, { ptr } }, ptr %75, i32 0, i32 1, i32 0
  store ptr %74, ptr %77, align 8
  %78 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %75, ptr %d_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %78, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %78, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3298", ptr %let_func_2, align 8
  %79 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_5 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 0
  store ptr %fun_capture_4, ptr %fun_capture_5, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %79, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3299", ptr %fun_func_2, align 8
  %80 = getelementptr { ptr, ptr }, ptr %78, i32 0, i32 0
  %81 = load ptr, ptr %80, align 8
  %82 = getelementptr { ptr, ptr }, ptr %78, i32 0, i32 1
  %83 = load ptr, ptr %82, align 8
  %84 = call ptr %83(ptr %81, ptr %79)
  ret ptr %84

switch_branch_Eventually.NotYetDone_2:            ; preds = %switch_branch_Eventually.NotYetDone_1
  %85 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 1
  %86 = getelementptr { ptr }, ptr %85, i32 0, i32 0
  %87 = load ptr, ptr %86, align 8
  %88 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %89 = getelementptr { i8, {} }, ptr %88, i32 0, i32 0
  store i8 0, ptr %89, align 1
  %90 = getelementptr { ptr, ptr }, ptr %87, i32 0, i32 0
  %91 = load ptr, ptr %90, align 8
  %92 = getelementptr { ptr, ptr }, ptr %87, i32 0, i32 1
  %93 = load ptr, ptr %92, align 8
  %94 = call ptr %93(ptr %91, ptr %88)
  %95 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %94, ptr %d_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %95, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %95, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3300", ptr %let_func_3, align 8
  %96 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_7 = getelementptr { ptr, ptr }, ptr %96, i32 0, i32 0
  store ptr %fun_capture_6, ptr %fun_capture_7, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %96, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3301", ptr %fun_func_3, align 8
  %97 = getelementptr { ptr, ptr }, ptr %95, i32 0, i32 0
  %98 = load ptr, ptr %97, align 8
  %99 = getelementptr { ptr, ptr }, ptr %95, i32 0, i32 1
  %100 = load ptr, ptr %99, align 8
  %101 = call ptr %100(ptr %98, ptr %96)
  ret ptr %101

switch_default_1:                                 ; preds = %switch_branch_Eventually.NotYetDone_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Eventually.Done_1
  unreachable

switch_branch_Eventually.NotYetDone_3:            ; preds = %switch_branch_Eventually.Done_0
  %102 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1
  %103 = getelementptr { ptr }, ptr %102, i32 0, i32 0
  %104 = load ptr, ptr %103, align 8
  %105 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %106 = getelementptr { i8, {} }, ptr %105, i32 0, i32 0
  store i8 0, ptr %106, align 1
  %107 = getelementptr { ptr, ptr }, ptr %104, i32 0, i32 0
  %108 = load ptr, ptr %107, align 8
  %109 = getelementptr { ptr, ptr }, ptr %104, i32 0, i32 1
  %110 = load ptr, ptr %109, align 8
  %111 = call ptr %110(ptr %108, ptr %105)
  %112 = getelementptr { i8, <8 x i8> }, ptr %111, i32 0, i32 0
  %113 = load i8, ptr %112, align 1
  switch i8 %113, label %switch_default_5 [
    i8 0, label %switch_branch_Eventually.Done_5
    i8 1, label %switch_branch_Eventually.NotYetDone_5
  ]

switch_branch_Eventually.Done_5:                  ; preds = %switch_branch_Eventually.NotYetDone_3
  %114 = getelementptr { i8, { ptr } }, ptr %111, i32 0, i32 1
  %115 = getelementptr { ptr }, ptr %114, i32 0, i32 0
  %116 = load ptr, ptr %115, align 8
  %117 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %118 = getelementptr { i8, { ptr } }, ptr %117, i32 0, i32 0
  store i8 0, ptr %118, align 1
  %119 = getelementptr { i8, { ptr } }, ptr %117, i32 0, i32 1, i32 0
  store ptr %116, ptr %119, align 8
  %120 = getelementptr { i8, <8 x i8> }, ptr %117, i32 0, i32 0
  %121 = load i8, ptr %120, align 1
  switch i8 %121, label %switch_default_3 [
    i8 0, label %switch_branch_Eventually.Done_6
    i8 1, label %switch_branch_Eventually.NotYetDone_4
  ]

switch_branch_Eventually.Done_6:                  ; preds = %switch_branch_Eventually.Done_5
  %122 = getelementptr { i8, { ptr } }, ptr %117, i32 0, i32 1
  %123 = getelementptr { ptr }, ptr %122, i32 0, i32 0
  %124 = load ptr, ptr %123, align 8
  %125 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %126 = getelementptr { i8, { ptr } }, ptr %125, i32 0, i32 0
  store i8 0, ptr %126, align 1
  %127 = getelementptr { i8, { ptr } }, ptr %125, i32 0, i32 1, i32 0
  store ptr %124, ptr %127, align 8
  %128 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_4 = getelementptr { ptr }, ptr %let_capture_8, i32 0, i32 0
  store ptr %125, ptr %d_4, align 8
  %let_capture_9 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 0
  store ptr %let_capture_8, ptr %let_capture_9, align 8
  %let_func_4 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3302", ptr %let_func_4, align 8
  %129 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_9 = getelementptr { ptr, ptr }, ptr %129, i32 0, i32 0
  store ptr %fun_capture_8, ptr %fun_capture_9, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %129, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3303", ptr %fun_func_4, align 8
  %130 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 0
  %131 = load ptr, ptr %130, align 8
  %132 = getelementptr { ptr, ptr }, ptr %128, i32 0, i32 1
  %133 = load ptr, ptr %132, align 8
  %134 = call ptr %133(ptr %131, ptr %129)
  ret ptr %134

switch_branch_Eventually.NotYetDone_4:            ; preds = %switch_branch_Eventually.Done_5
  %135 = getelementptr { i8, { ptr } }, ptr %117, i32 0, i32 1
  %136 = getelementptr { ptr }, ptr %135, i32 0, i32 0
  %137 = load ptr, ptr %136, align 8
  %138 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %139 = getelementptr { i8, {} }, ptr %138, i32 0, i32 0
  store i8 0, ptr %139, align 1
  %140 = getelementptr { ptr, ptr }, ptr %137, i32 0, i32 0
  %141 = load ptr, ptr %140, align 8
  %142 = getelementptr { ptr, ptr }, ptr %137, i32 0, i32 1
  %143 = load ptr, ptr %142, align 8
  %144 = call ptr %143(ptr %141, ptr %138)
  %145 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_5 = getelementptr { ptr }, ptr %let_capture_10, i32 0, i32 0
  store ptr %144, ptr %d_5, align 8
  %let_capture_11 = getelementptr { ptr, ptr }, ptr %145, i32 0, i32 0
  store ptr %let_capture_10, ptr %let_capture_11, align 8
  %let_func_5 = getelementptr { ptr, ptr }, ptr %145, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3304", ptr %let_func_5, align 8
  %146 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_11 = getelementptr { ptr, ptr }, ptr %146, i32 0, i32 0
  store ptr %fun_capture_10, ptr %fun_capture_11, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %146, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3305", ptr %fun_func_5, align 8
  %147 = getelementptr { ptr, ptr }, ptr %145, i32 0, i32 0
  %148 = load ptr, ptr %147, align 8
  %149 = getelementptr { ptr, ptr }, ptr %145, i32 0, i32 1
  %150 = load ptr, ptr %149, align 8
  %151 = call ptr %150(ptr %148, ptr %146)
  ret ptr %151

switch_default_3:                                 ; preds = %switch_branch_Eventually.Done_5
  unreachable

switch_branch_Eventually.NotYetDone_5:            ; preds = %switch_branch_Eventually.NotYetDone_3
  %152 = getelementptr { i8, { ptr } }, ptr %111, i32 0, i32 1
  %153 = getelementptr { ptr }, ptr %152, i32 0, i32 0
  %154 = load ptr, ptr %153, align 8
  %155 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %156 = getelementptr { i8, {} }, ptr %155, i32 0, i32 0
  store i8 0, ptr %156, align 1
  %157 = getelementptr { ptr, ptr }, ptr %154, i32 0, i32 0
  %158 = load ptr, ptr %157, align 8
  %159 = getelementptr { ptr, ptr }, ptr %154, i32 0, i32 1
  %160 = load ptr, ptr %159, align 8
  %161 = call ptr %160(ptr %158, ptr %155)
  %162 = getelementptr { i8, <8 x i8> }, ptr %161, i32 0, i32 0
  %163 = load i8, ptr %162, align 1
  switch i8 %163, label %switch_default_4 [
    i8 0, label %switch_branch_Eventually.Done_7
    i8 1, label %switch_branch_Eventually.NotYetDone_6
  ]

switch_branch_Eventually.Done_7:                  ; preds = %switch_branch_Eventually.NotYetDone_5
  %164 = getelementptr { i8, { ptr } }, ptr %161, i32 0, i32 1
  %165 = getelementptr { ptr }, ptr %164, i32 0, i32 0
  %166 = load ptr, ptr %165, align 8
  %167 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %168 = getelementptr { i8, { ptr } }, ptr %167, i32 0, i32 0
  store i8 0, ptr %168, align 1
  %169 = getelementptr { i8, { ptr } }, ptr %167, i32 0, i32 1, i32 0
  store ptr %166, ptr %169, align 8
  %170 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_6 = getelementptr { ptr }, ptr %let_capture_12, i32 0, i32 0
  store ptr %167, ptr %d_6, align 8
  %let_capture_13 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 0
  store ptr %let_capture_12, ptr %let_capture_13, align 8
  %let_func_6 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3306", ptr %let_func_6, align 8
  %171 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_13 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 0
  store ptr %fun_capture_12, ptr %fun_capture_13, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3307", ptr %fun_func_6, align 8
  %172 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 0
  %173 = load ptr, ptr %172, align 8
  %174 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 1
  %175 = load ptr, ptr %174, align 8
  %176 = call ptr %175(ptr %173, ptr %171)
  ret ptr %176

switch_branch_Eventually.NotYetDone_6:            ; preds = %switch_branch_Eventually.NotYetDone_5
  %177 = getelementptr { i8, { ptr } }, ptr %161, i32 0, i32 1
  %178 = getelementptr { ptr }, ptr %177, i32 0, i32 0
  %179 = load ptr, ptr %178, align 8
  %180 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %181 = getelementptr { i8, {} }, ptr %180, i32 0, i32 0
  store i8 0, ptr %181, align 1
  %182 = getelementptr { ptr, ptr }, ptr %179, i32 0, i32 0
  %183 = load ptr, ptr %182, align 8
  %184 = getelementptr { ptr, ptr }, ptr %179, i32 0, i32 1
  %185 = load ptr, ptr %184, align 8
  %186 = call ptr %185(ptr %183, ptr %180)
  %187 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_7 = getelementptr { ptr }, ptr %let_capture_14, i32 0, i32 0
  store ptr %186, ptr %d_7, align 8
  %let_capture_15 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 0
  store ptr %let_capture_14, ptr %let_capture_15, align 8
  %let_func_7 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3308", ptr %let_func_7, align 8
  %188 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_15 = getelementptr { ptr, ptr }, ptr %188, i32 0, i32 0
  store ptr %fun_capture_14, ptr %fun_capture_15, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %188, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3309", ptr %fun_func_7, align 8
  %189 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 0
  %190 = load ptr, ptr %189, align 8
  %191 = getelementptr { ptr, ptr }, ptr %187, i32 0, i32 1
  %192 = load ptr, ptr %191, align 8
  %193 = call ptr %192(ptr %190, ptr %188)
  ret ptr %193

switch_default_4:                                 ; preds = %switch_branch_Eventually.NotYetDone_5
  unreachable

switch_default_5:                                 ; preds = %switch_branch_Eventually.NotYetDone_3
  unreachable

switch_default_6:                                 ; preds = %switch_branch_Eventually.Done_0
  unreachable

switch_branch_Eventually.NotYetDone_7:            ; preds = %1
  %194 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %195 = getelementptr { ptr }, ptr %194, i32 0, i32 0
  %196 = load ptr, ptr %195, align 8
  %197 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %198 = getelementptr { i8, {} }, ptr %197, i32 0, i32 0
  store i8 0, ptr %198, align 1
  %199 = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 0
  %200 = load ptr, ptr %199, align 8
  %201 = getelementptr { ptr, ptr }, ptr %196, i32 0, i32 1
  %202 = load ptr, ptr %201, align 8
  %203 = call ptr %202(ptr %200, ptr %197)
  %204 = load ptr, ptr @Eventually.comp, align 8
  %205 = getelementptr { i8, <8 x i8> }, ptr %204, i32 0, i32 0
  %206 = load i8, ptr %205, align 1
  switch i8 %206, label %switch_default_13 [
    i8 0, label %switch_branch_Eventually.Done_8
    i8 1, label %switch_branch_Eventually.NotYetDone_11
  ]

switch_branch_Eventually.Done_8:                  ; preds = %switch_branch_Eventually.NotYetDone_7
  %207 = getelementptr { i8, { ptr } }, ptr %204, i32 0, i32 1
  %208 = getelementptr { ptr }, ptr %207, i32 0, i32 0
  %209 = load ptr, ptr %208, align 8
  %210 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %211 = getelementptr { i8, { ptr } }, ptr %210, i32 0, i32 0
  store i8 0, ptr %211, align 1
  %212 = getelementptr { i8, { ptr } }, ptr %210, i32 0, i32 1, i32 0
  store ptr %209, ptr %212, align 8
  %213 = getelementptr { i8, <8 x i8> }, ptr %210, i32 0, i32 0
  %214 = load i8, ptr %213, align 1
  switch i8 %214, label %switch_default_9 [
    i8 0, label %switch_branch_Eventually.Done_9
    i8 1, label %switch_branch_Eventually.NotYetDone_9
  ]

switch_branch_Eventually.Done_9:                  ; preds = %switch_branch_Eventually.Done_8
  %215 = getelementptr { i8, { ptr } }, ptr %210, i32 0, i32 1
  %216 = getelementptr { ptr }, ptr %215, i32 0, i32 0
  %217 = load ptr, ptr %216, align 8
  %218 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %219 = getelementptr { i8, { ptr } }, ptr %218, i32 0, i32 0
  store i8 0, ptr %219, align 1
  %220 = getelementptr { i8, { ptr } }, ptr %218, i32 0, i32 1, i32 0
  store ptr %217, ptr %220, align 8
  %221 = getelementptr { i8, <8 x i8> }, ptr %218, i32 0, i32 0
  %222 = load i8, ptr %221, align 1
  switch i8 %222, label %switch_default_7 [
    i8 0, label %switch_branch_Eventually.Done_10
    i8 1, label %switch_branch_Eventually.NotYetDone_8
  ]

switch_branch_Eventually.Done_10:                 ; preds = %switch_branch_Eventually.Done_9
  %223 = getelementptr { i8, { ptr } }, ptr %218, i32 0, i32 1
  %224 = getelementptr { ptr }, ptr %223, i32 0, i32 0
  %225 = load ptr, ptr %224, align 8
  %226 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %227 = getelementptr { i8, { ptr } }, ptr %226, i32 0, i32 0
  store i8 0, ptr %227, align 1
  %228 = getelementptr { i8, { ptr } }, ptr %226, i32 0, i32 1, i32 0
  store ptr %225, ptr %228, align 8
  %229 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_8 = getelementptr { ptr }, ptr %let_capture_16, i32 0, i32 0
  store ptr %226, ptr %d_8, align 8
  %let_capture_17 = getelementptr { ptr, ptr }, ptr %229, i32 0, i32 0
  store ptr %let_capture_16, ptr %let_capture_17, align 8
  %let_func_8 = getelementptr { ptr, ptr }, ptr %229, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3310", ptr %let_func_8, align 8
  %230 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_17 = getelementptr { ptr, ptr }, ptr %230, i32 0, i32 0
  store ptr %fun_capture_16, ptr %fun_capture_17, align 8
  %fun_func_8 = getelementptr { ptr, ptr }, ptr %230, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3311", ptr %fun_func_8, align 8
  %231 = getelementptr { ptr, ptr }, ptr %229, i32 0, i32 0
  %232 = load ptr, ptr %231, align 8
  %233 = getelementptr { ptr, ptr }, ptr %229, i32 0, i32 1
  %234 = load ptr, ptr %233, align 8
  %235 = call ptr %234(ptr %232, ptr %230)
  ret ptr %235

switch_branch_Eventually.NotYetDone_8:            ; preds = %switch_branch_Eventually.Done_9
  %236 = getelementptr { i8, { ptr } }, ptr %218, i32 0, i32 1
  %237 = getelementptr { ptr }, ptr %236, i32 0, i32 0
  %238 = load ptr, ptr %237, align 8
  %239 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %240 = getelementptr { i8, {} }, ptr %239, i32 0, i32 0
  store i8 0, ptr %240, align 1
  %241 = getelementptr { ptr, ptr }, ptr %238, i32 0, i32 0
  %242 = load ptr, ptr %241, align 8
  %243 = getelementptr { ptr, ptr }, ptr %238, i32 0, i32 1
  %244 = load ptr, ptr %243, align 8
  %245 = call ptr %244(ptr %242, ptr %239)
  %246 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_9 = getelementptr { ptr }, ptr %let_capture_18, i32 0, i32 0
  store ptr %245, ptr %d_9, align 8
  %let_capture_19 = getelementptr { ptr, ptr }, ptr %246, i32 0, i32 0
  store ptr %let_capture_18, ptr %let_capture_19, align 8
  %let_func_9 = getelementptr { ptr, ptr }, ptr %246, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3312", ptr %let_func_9, align 8
  %247 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_19 = getelementptr { ptr, ptr }, ptr %247, i32 0, i32 0
  store ptr %fun_capture_18, ptr %fun_capture_19, align 8
  %fun_func_9 = getelementptr { ptr, ptr }, ptr %247, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3313", ptr %fun_func_9, align 8
  %248 = getelementptr { ptr, ptr }, ptr %246, i32 0, i32 0
  %249 = load ptr, ptr %248, align 8
  %250 = getelementptr { ptr, ptr }, ptr %246, i32 0, i32 1
  %251 = load ptr, ptr %250, align 8
  %252 = call ptr %251(ptr %249, ptr %247)
  ret ptr %252

switch_default_7:                                 ; preds = %switch_branch_Eventually.Done_9
  unreachable

switch_branch_Eventually.NotYetDone_9:            ; preds = %switch_branch_Eventually.Done_8
  %253 = getelementptr { i8, { ptr } }, ptr %210, i32 0, i32 1
  %254 = getelementptr { ptr }, ptr %253, i32 0, i32 0
  %255 = load ptr, ptr %254, align 8
  %256 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %257 = getelementptr { i8, {} }, ptr %256, i32 0, i32 0
  store i8 0, ptr %257, align 1
  %258 = getelementptr { ptr, ptr }, ptr %255, i32 0, i32 0
  %259 = load ptr, ptr %258, align 8
  %260 = getelementptr { ptr, ptr }, ptr %255, i32 0, i32 1
  %261 = load ptr, ptr %260, align 8
  %262 = call ptr %261(ptr %259, ptr %256)
  %263 = getelementptr { i8, <8 x i8> }, ptr %262, i32 0, i32 0
  %264 = load i8, ptr %263, align 1
  switch i8 %264, label %switch_default_8 [
    i8 0, label %switch_branch_Eventually.Done_11
    i8 1, label %switch_branch_Eventually.NotYetDone_10
  ]

switch_branch_Eventually.Done_11:                 ; preds = %switch_branch_Eventually.NotYetDone_9
  %265 = getelementptr { i8, { ptr } }, ptr %262, i32 0, i32 1
  %266 = getelementptr { ptr }, ptr %265, i32 0, i32 0
  %267 = load ptr, ptr %266, align 8
  %268 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %269 = getelementptr { i8, { ptr } }, ptr %268, i32 0, i32 0
  store i8 0, ptr %269, align 1
  %270 = getelementptr { i8, { ptr } }, ptr %268, i32 0, i32 1, i32 0
  store ptr %267, ptr %270, align 8
  %271 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_10 = getelementptr { ptr }, ptr %let_capture_20, i32 0, i32 0
  store ptr %268, ptr %d_10, align 8
  %let_capture_21 = getelementptr { ptr, ptr }, ptr %271, i32 0, i32 0
  store ptr %let_capture_20, ptr %let_capture_21, align 8
  %let_func_10 = getelementptr { ptr, ptr }, ptr %271, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3314", ptr %let_func_10, align 8
  %272 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_21 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 0
  store ptr %fun_capture_20, ptr %fun_capture_21, align 8
  %fun_func_10 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3315", ptr %fun_func_10, align 8
  %273 = getelementptr { ptr, ptr }, ptr %271, i32 0, i32 0
  %274 = load ptr, ptr %273, align 8
  %275 = getelementptr { ptr, ptr }, ptr %271, i32 0, i32 1
  %276 = load ptr, ptr %275, align 8
  %277 = call ptr %276(ptr %274, ptr %272)
  ret ptr %277

switch_branch_Eventually.NotYetDone_10:           ; preds = %switch_branch_Eventually.NotYetDone_9
  %278 = getelementptr { i8, { ptr } }, ptr %262, i32 0, i32 1
  %279 = getelementptr { ptr }, ptr %278, i32 0, i32 0
  %280 = load ptr, ptr %279, align 8
  %281 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %282 = getelementptr { i8, {} }, ptr %281, i32 0, i32 0
  store i8 0, ptr %282, align 1
  %283 = getelementptr { ptr, ptr }, ptr %280, i32 0, i32 0
  %284 = load ptr, ptr %283, align 8
  %285 = getelementptr { ptr, ptr }, ptr %280, i32 0, i32 1
  %286 = load ptr, ptr %285, align 8
  %287 = call ptr %286(ptr %284, ptr %281)
  %288 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_11 = getelementptr { ptr }, ptr %let_capture_22, i32 0, i32 0
  store ptr %287, ptr %d_11, align 8
  %let_capture_23 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 0
  store ptr %let_capture_22, ptr %let_capture_23, align 8
  %let_func_11 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3316", ptr %let_func_11, align 8
  %289 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_23 = getelementptr { ptr, ptr }, ptr %289, i32 0, i32 0
  store ptr %fun_capture_22, ptr %fun_capture_23, align 8
  %fun_func_11 = getelementptr { ptr, ptr }, ptr %289, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3317", ptr %fun_func_11, align 8
  %290 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 0
  %291 = load ptr, ptr %290, align 8
  %292 = getelementptr { ptr, ptr }, ptr %288, i32 0, i32 1
  %293 = load ptr, ptr %292, align 8
  %294 = call ptr %293(ptr %291, ptr %289)
  ret ptr %294

switch_default_8:                                 ; preds = %switch_branch_Eventually.NotYetDone_9
  unreachable

switch_default_9:                                 ; preds = %switch_branch_Eventually.Done_8
  unreachable

switch_branch_Eventually.NotYetDone_11:           ; preds = %switch_branch_Eventually.NotYetDone_7
  %295 = getelementptr { i8, { ptr } }, ptr %204, i32 0, i32 1
  %296 = getelementptr { ptr }, ptr %295, i32 0, i32 0
  %297 = load ptr, ptr %296, align 8
  %298 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %299 = getelementptr { i8, {} }, ptr %298, i32 0, i32 0
  store i8 0, ptr %299, align 1
  %300 = getelementptr { ptr, ptr }, ptr %297, i32 0, i32 0
  %301 = load ptr, ptr %300, align 8
  %302 = getelementptr { ptr, ptr }, ptr %297, i32 0, i32 1
  %303 = load ptr, ptr %302, align 8
  %304 = call ptr %303(ptr %301, ptr %298)
  %305 = getelementptr { i8, <8 x i8> }, ptr %304, i32 0, i32 0
  %306 = load i8, ptr %305, align 1
  switch i8 %306, label %switch_default_12 [
    i8 0, label %switch_branch_Eventually.Done_12
    i8 1, label %switch_branch_Eventually.NotYetDone_13
  ]

switch_branch_Eventually.Done_12:                 ; preds = %switch_branch_Eventually.NotYetDone_11
  %307 = getelementptr { i8, { ptr } }, ptr %304, i32 0, i32 1
  %308 = getelementptr { ptr }, ptr %307, i32 0, i32 0
  %309 = load ptr, ptr %308, align 8
  %310 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %311 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 0
  store i8 0, ptr %311, align 1
  %312 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 1, i32 0
  store ptr %309, ptr %312, align 8
  %313 = getelementptr { i8, <8 x i8> }, ptr %310, i32 0, i32 0
  %314 = load i8, ptr %313, align 1
  switch i8 %314, label %switch_default_10 [
    i8 0, label %switch_branch_Eventually.Done_13
    i8 1, label %switch_branch_Eventually.NotYetDone_12
  ]

switch_branch_Eventually.Done_13:                 ; preds = %switch_branch_Eventually.Done_12
  %315 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 1
  %316 = getelementptr { ptr }, ptr %315, i32 0, i32 0
  %317 = load ptr, ptr %316, align 8
  %318 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %319 = getelementptr { i8, { ptr } }, ptr %318, i32 0, i32 0
  store i8 0, ptr %319, align 1
  %320 = getelementptr { i8, { ptr } }, ptr %318, i32 0, i32 1, i32 0
  store ptr %317, ptr %320, align 8
  %321 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_12 = getelementptr { ptr }, ptr %let_capture_24, i32 0, i32 0
  store ptr %318, ptr %d_12, align 8
  %let_capture_25 = getelementptr { ptr, ptr }, ptr %321, i32 0, i32 0
  store ptr %let_capture_24, ptr %let_capture_25, align 8
  %let_func_12 = getelementptr { ptr, ptr }, ptr %321, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3318", ptr %let_func_12, align 8
  %322 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_25 = getelementptr { ptr, ptr }, ptr %322, i32 0, i32 0
  store ptr %fun_capture_24, ptr %fun_capture_25, align 8
  %fun_func_12 = getelementptr { ptr, ptr }, ptr %322, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3319", ptr %fun_func_12, align 8
  %323 = getelementptr { ptr, ptr }, ptr %321, i32 0, i32 0
  %324 = load ptr, ptr %323, align 8
  %325 = getelementptr { ptr, ptr }, ptr %321, i32 0, i32 1
  %326 = load ptr, ptr %325, align 8
  %327 = call ptr %326(ptr %324, ptr %322)
  ret ptr %327

switch_branch_Eventually.NotYetDone_12:           ; preds = %switch_branch_Eventually.Done_12
  %328 = getelementptr { i8, { ptr } }, ptr %310, i32 0, i32 1
  %329 = getelementptr { ptr }, ptr %328, i32 0, i32 0
  %330 = load ptr, ptr %329, align 8
  %331 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %332 = getelementptr { i8, {} }, ptr %331, i32 0, i32 0
  store i8 0, ptr %332, align 1
  %333 = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 0
  %334 = load ptr, ptr %333, align 8
  %335 = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 1
  %336 = load ptr, ptr %335, align 8
  %337 = call ptr %336(ptr %334, ptr %331)
  %338 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_13 = getelementptr { ptr }, ptr %let_capture_26, i32 0, i32 0
  store ptr %337, ptr %d_13, align 8
  %let_capture_27 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 0
  store ptr %let_capture_26, ptr %let_capture_27, align 8
  %let_func_13 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3320", ptr %let_func_13, align 8
  %339 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_27 = getelementptr { ptr, ptr }, ptr %339, i32 0, i32 0
  store ptr %fun_capture_26, ptr %fun_capture_27, align 8
  %fun_func_13 = getelementptr { ptr, ptr }, ptr %339, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3321", ptr %fun_func_13, align 8
  %340 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 0
  %341 = load ptr, ptr %340, align 8
  %342 = getelementptr { ptr, ptr }, ptr %338, i32 0, i32 1
  %343 = load ptr, ptr %342, align 8
  %344 = call ptr %343(ptr %341, ptr %339)
  ret ptr %344

switch_default_10:                                ; preds = %switch_branch_Eventually.Done_12
  unreachable

switch_branch_Eventually.NotYetDone_13:           ; preds = %switch_branch_Eventually.NotYetDone_11
  %345 = getelementptr { i8, { ptr } }, ptr %304, i32 0, i32 1
  %346 = getelementptr { ptr }, ptr %345, i32 0, i32 0
  %347 = load ptr, ptr %346, align 8
  %348 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %349 = getelementptr { i8, {} }, ptr %348, i32 0, i32 0
  store i8 0, ptr %349, align 1
  %350 = getelementptr { ptr, ptr }, ptr %347, i32 0, i32 0
  %351 = load ptr, ptr %350, align 8
  %352 = getelementptr { ptr, ptr }, ptr %347, i32 0, i32 1
  %353 = load ptr, ptr %352, align 8
  %354 = call ptr %353(ptr %351, ptr %348)
  %355 = getelementptr { i8, <8 x i8> }, ptr %354, i32 0, i32 0
  %356 = load i8, ptr %355, align 1
  switch i8 %356, label %switch_default_11 [
    i8 0, label %switch_branch_Eventually.Done_14
    i8 1, label %switch_branch_Eventually.NotYetDone_14
  ]

switch_branch_Eventually.Done_14:                 ; preds = %switch_branch_Eventually.NotYetDone_13
  %357 = getelementptr { i8, { ptr } }, ptr %354, i32 0, i32 1
  %358 = getelementptr { ptr }, ptr %357, i32 0, i32 0
  %359 = load ptr, ptr %358, align 8
  %360 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %361 = getelementptr { i8, { ptr } }, ptr %360, i32 0, i32 0
  store i8 0, ptr %361, align 1
  %362 = getelementptr { i8, { ptr } }, ptr %360, i32 0, i32 1, i32 0
  store ptr %359, ptr %362, align 8
  %363 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_14 = getelementptr { ptr }, ptr %let_capture_28, i32 0, i32 0
  store ptr %360, ptr %d_14, align 8
  %let_capture_29 = getelementptr { ptr, ptr }, ptr %363, i32 0, i32 0
  store ptr %let_capture_28, ptr %let_capture_29, align 8
  %let_func_14 = getelementptr { ptr, ptr }, ptr %363, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3322", ptr %let_func_14, align 8
  %364 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_29 = getelementptr { ptr, ptr }, ptr %364, i32 0, i32 0
  store ptr %fun_capture_28, ptr %fun_capture_29, align 8
  %fun_func_14 = getelementptr { ptr, ptr }, ptr %364, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3323", ptr %fun_func_14, align 8
  %365 = getelementptr { ptr, ptr }, ptr %363, i32 0, i32 0
  %366 = load ptr, ptr %365, align 8
  %367 = getelementptr { ptr, ptr }, ptr %363, i32 0, i32 1
  %368 = load ptr, ptr %367, align 8
  %369 = call ptr %368(ptr %366, ptr %364)
  ret ptr %369

switch_branch_Eventually.NotYetDone_14:           ; preds = %switch_branch_Eventually.NotYetDone_13
  %370 = getelementptr { i8, { ptr } }, ptr %354, i32 0, i32 1
  %371 = getelementptr { ptr }, ptr %370, i32 0, i32 0
  %372 = load ptr, ptr %371, align 8
  %373 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %374 = getelementptr { i8, {} }, ptr %373, i32 0, i32 0
  store i8 0, ptr %374, align 1
  %375 = getelementptr { ptr, ptr }, ptr %372, i32 0, i32 0
  %376 = load ptr, ptr %375, align 8
  %377 = getelementptr { ptr, ptr }, ptr %372, i32 0, i32 1
  %378 = load ptr, ptr %377, align 8
  %379 = call ptr %378(ptr %376, ptr %373)
  %380 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_15 = getelementptr { ptr }, ptr %let_capture_30, i32 0, i32 0
  store ptr %379, ptr %d_15, align 8
  %let_capture_31 = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 0
  store ptr %let_capture_30, ptr %let_capture_31, align 8
  %let_func_15 = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 1
  store ptr @"Eventually.#let_closure_3324", ptr %let_func_15, align 8
  %381 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_31 = getelementptr { ptr, ptr }, ptr %381, i32 0, i32 0
  store ptr %fun_capture_30, ptr %fun_capture_31, align 8
  %fun_func_15 = getelementptr { ptr, ptr }, ptr %381, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3325", ptr %fun_func_15, align 8
  %382 = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 0
  %383 = load ptr, ptr %382, align 8
  %384 = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 1
  %385 = load ptr, ptr %384, align 8
  %386 = call ptr %385(ptr %383, ptr %381)
  ret ptr %386

switch_default_11:                                ; preds = %switch_branch_Eventually.NotYetDone_13
  unreachable

switch_default_12:                                ; preds = %switch_branch_Eventually.NotYetDone_11
  unreachable

switch_default_13:                                ; preds = %switch_branch_Eventually.NotYetDone_7
  unreachable

switch_default_14:                                ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Eventually()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Eventually.main(ptr null, ptr %2)
  ret i32 0
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal ptr @"Eventually.#fun_closure_3330"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr @str3331, ptr %5, align 8
  %6 = getelementptr { i8, <8 x i8> }, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %2
  %8 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1
  %9 = getelementptr { ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @malgo_print_string(ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %11, ptr %14, align 8
  ret ptr %12

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#fun_closure_3333"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr @str3334, ptr %5, align 8
  %6 = getelementptr { i8, <8 x i8> }, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %2
  %8 = getelementptr { i8, { ptr } }, ptr %3, i32 0, i32 1
  %9 = getelementptr { ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @malgo_print_string(ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %11, ptr %14, align 8
  ret ptr %12

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"Eventually.#fun_closure_3335"(ptr %0, ptr %1) {
  %3 = load ptr, ptr @Eventually.eventually, align 8
  %4 = call ptr @malgo_hash_table_get(ptr %3, ptr @str3290)
  %5 = call ptr @malgo_hash_table_get(ptr %3, ptr @str3291)
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 1, i32 0
  store i32 3, ptr %8, align 4
  %9 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %6)
  ret ptr %13
}

define internal ptr @"Eventually.#fun_closure_3332"(ptr %0, ptr %1) {
  %3 = load ptr, ptr @Eventually.eventually, align 8
  %4 = call ptr @malgo_hash_table_get(ptr %3, ptr @str3290)
  %5 = call ptr @malgo_hash_table_get(ptr %3, ptr @str3291)
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3333", ptr %fun_func_0, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  %10 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr %13(ptr %11, ptr %7)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3335", ptr %fun_func_1, align 8
  %16 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %15)
  ret ptr %20
}

define internal void @malgo_load_Eventually() {
  %1 = call ptr @malgo_hash_table_new()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Done_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %Done_capture_0, align 8
  %Done_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @Eventually.Done, ptr %Done_func_0, align 8
  call void @malgo_hash_table_insert(ptr %1, ptr @str3290, ptr %2)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %eventuallyBind_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %eventuallyBind_capture_0, align 8
  %eventuallyBind_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @Eventually.eventuallyBind, ptr %eventuallyBind_func_0, align 8
  call void @malgo_hash_table_insert(ptr %1, ptr @str3291, ptr %3)
  store ptr %1, ptr @Eventually.eventually, align 8
  %4 = load ptr, ptr @Eventually.eventually, align 8
  %5 = call ptr @malgo_hash_table_get(ptr %4, ptr @str3290)
  %6 = call ptr @malgo_hash_table_get(ptr %4, ptr @str3291)
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3330", ptr %fun_func_0, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"Eventually.#fun_closure_3332", ptr %fun_func_1, align 8
  %17 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %16)
  store ptr %21, ptr @Eventually.comp, align 8
  ret void
}
