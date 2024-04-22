; ModuleID = './test/testcases/malgo/Show.mlg'
source_filename = "./test/testcases/malgo/Show.mlg"

@Show.showInt32 = global ptr undef
@str3849 = unnamed_addr constant [2 x i8] c"(\00"
@str3850 = unnamed_addr constant [5 x i8] c"show\00"
@str3851 = unnamed_addr constant [3 x i8] c", \00"
@str3852 = unnamed_addr constant [2 x i8] c")\00"

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

define internal ptr @"Builtin.String#"(ptr %0, ptr %"Builtin.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @"Show.#let_closure_3853"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3837"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.#let_closure_3854"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3841"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.#let_closure_3855"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3843"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.#let_closure_3856"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3844"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.$raw_fun_3845"(ptr %0, ptr %"Show.$showDictB_188_0", ptr %"Show.$showDictA_187_0", ptr %"Show.$tuple_190_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Show.$tuple_190_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"Show.$tuple_190_0", i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr @str3849, ptr %11, align 8
  %12 = call ptr @malgo_hash_table_get(ptr %"Show.$showDictA_187_0", ptr @str3850)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr @str3851, ptr %20, align 8
  %21 = call ptr @malgo_hash_table_get(ptr %"Show.$showDictB_188_0", ptr @str3850)
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %8)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr @str3852, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %26, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"Show.#let_closure_3853", ptr %let_func_0, align 8
  %31 = call ptr @"Builtin.$appendString_curry_3984"(ptr null, ptr %26, ptr %27)
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %18, ptr %d_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  store ptr @"Show.#let_closure_3854", ptr %let_func_1, align 8
  %33 = call ptr @"Builtin.$appendString_curry_3984"(ptr null, ptr %18, ptr %31)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %17, ptr %d_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"Show.#let_closure_3855", ptr %let_func_2, align 8
  %35 = call ptr @"Builtin.$appendString_curry_3984"(ptr null, ptr %17, ptr %33)
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %9, ptr %d_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  store ptr @"Show.#let_closure_3856", ptr %let_func_3, align 8
  %37 = call ptr @"Builtin.$appendString_curry_3984"(ptr null, ptr %9, ptr %35)
  ret ptr %37

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.$raw_let_3841"(ptr %0, ptr %"Show.$d_203_0", ptr %"Show.$string#_3691_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Show.$d_203_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Show.$d_203_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Show.$string#_3691_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Show.$string#_3691_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.appendString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.$raw_let_3843"(ptr %0, ptr %"Show.$d_199_0", ptr %"Show.$string#_3693_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Show.$d_199_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Show.$d_199_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Show.$string#_3693_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Show.$string#_3693_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.appendString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.$raw_let_3844"(ptr %0, ptr %"Show.$d_193_0", ptr %"Show.$string#_3695_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Show.$d_193_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Show.$d_193_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Show.$string#_3695_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Show.$string#_3695_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.appendString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.#let_closure_3857"(ptr %0, ptr %1) {
  %showDictA_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %3 = call ptr @"Show.$showTuple2_curry_186"(ptr null, ptr %showDictA_0, ptr %1)
  ret ptr %3
}

define internal ptr @Show.showTuple2(ptr %0, ptr %"Show.$showDictA_149_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDictA_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Show.$showDictA_149_0", ptr %showDictA_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Show.#let_closure_3857", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Show.#let_closure_3858"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.$raw_let_3847"(ptr %0, ptr %"Show.$x_137_0", ptr %"Show.$y_138_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Show.$x_137_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Show.$x_137_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Show.$y_138_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Show.$y_138_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"Show.#let_closure_3858", ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %17, ptr %20, align 8
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.#let_closure_3859"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3847"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Show.<>"(ptr %0, ptr %"Show.$x_137_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Show.$x_137_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Show.#let_closure_3859", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Show.#let_closure_3860"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Builtin.$appendString_curry_3984"(ptr %0, ptr %"Builtin.$string#_3985_0", ptr %"Builtin.$string#_3986_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3985_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3985_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3986_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3986_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"Show.#let_closure_3860", ptr %let_func_0, align 8
  %13 = call ptr @"Builtin.$malgo_string_append_curry_2142"(ptr null, ptr %6, ptr %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.#let_closure_3861"(ptr %0, ptr %1) {
  %showDict_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDict_0 = load ptr, ptr %showDict_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3846"(ptr null, ptr %showDict_0, ptr %1)
  ret ptr %3
}

define internal ptr @Show.print(ptr %0, ptr %"Show.$showDict_121_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDict_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Show.$showDict_121_0", ptr %showDict_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Show.#let_closure_3861", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$malgo_string_append_curry_2142"(ptr %0, ptr %"Builtin.$p_2143_0", ptr %"Builtin.$p_2144_0") {
  %2 = call ptr @malgo_string_append(ptr %"Builtin.$p_2143_0", ptr %"Builtin.$p_2144_0")
  ret ptr %2
}

define internal ptr @"Show.$raw_let_3848"(ptr %0, ptr %"Show.$cast_230_0", ptr %"Show.$x_3802_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"Show.$cast_230_0", ptr @str3850)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Show.$x_3802_0")
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.#let_closure_3862"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"Show.$raw_let_3848"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @Show.main(ptr %0, ptr %"Show.$$__225_0") {
  %2 = load ptr, ptr @Show.showInt32, align 8
  %3 = load ptr, ptr @Show.showInt32, align 8
  %4 = call ptr @"Show.$showTuple2_curry_186"(ptr null, ptr %2, ptr %3)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %4, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"Show.#let_closure_3862", ptr %let_func_0, align 8
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %6, i32 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %9, i32 0, i32 1, i32 0
  store i32 2, ptr %11, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %6, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 1
  store ptr %9, ptr %15, align 8
  %16 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %12)
  ret ptr %20
}

define internal ptr @"Show.#fun_closure_3863"(ptr %0, ptr %1) {
  %showDictB_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %showDictA_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %3 = call ptr @"Show.$raw_fun_3845"(ptr null, ptr %showDictB_0, ptr %showDictA_0, ptr %1)
  ret ptr %3
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal ptr @"Show.$showTuple2_curry_186"(ptr %0, ptr %"Show.$showDictA_187_0", ptr %"Show.$showDictB_188_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %showDictB_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"Show.$showDictB_188_0", ptr %showDictB_0, align 8
  %showDictA_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"Show.$showDictA_187_0", ptr %showDictA_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Show.#fun_closure_3863", ptr %fun_func_0, align 8
  %3 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %3, ptr @str3850, ptr %2)
  ret ptr %3
}

define internal ptr @Show.show(ptr %0, ptr %"Show.$record_119_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"Show.$record_119_0", ptr @str3850)
  ret ptr %2
}

define internal ptr @"Show.$raw_let_3846"(ptr %0, ptr %"Show.$showDict_121_0", ptr %"Show.$x_122_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"Show.$showDict_121_0", ptr @str3850)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Show.$x_122_0")
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Show.#let_closure_3864"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Builtin.appendString#"(ptr %0, ptr %"Builtin.$x_3963_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_3963_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Show.#let_closure_3864", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Show.$raw_let_3837"(ptr %0, ptr %"Show.$d_209_0", ptr %"Show.$string#_3689_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Show.$d_209_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Show.$d_209_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Show.$string#_3689_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Show.$string#_3689_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.appendString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$appendString#_curry_3968"(ptr %0, ptr %"Builtin.$x_3969_0", ptr %"Builtin.$y_3970_0") {
  %2 = call ptr @malgo_string_append(ptr %"Builtin.$x_3969_0", ptr %"Builtin.$y_3970_0")
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Show()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Show.main(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @"Show.#fun_closure_3869"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %2
  %5 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr %8, ptr %11, align 8
  ret ptr %9

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal void @koriel_load_Show() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr @"Show.#fun_closure_3869", ptr %fun_func_0, align 8
  %2 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %2, ptr @str3850, ptr %1)
  store ptr %2, ptr @Show.showInt32, align 8
  ret void
}
