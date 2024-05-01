; ModuleID = 'test/testcases/malgo/Show.mlg'
source_filename = "test/testcases/malgo/Show.mlg"

@"test/testcases/malgo/Show.mlg.showInt32" = global ptr undef
@str3189 = unnamed_addr constant [2 x i8] c"(\00"
@str3190 = unnamed_addr constant [5 x i8] c"show\00"
@str3191 = unnamed_addr constant [3 x i8] c", \00"
@str3192 = unnamed_addr constant [2 x i8] c")\00"

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

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3985_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_3986_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %6, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3186", ptr %let_func_0, align 8
  %13 = call ptr @"runtime/malgo/Builtin.mlg.$malgo_string_append_curry_2142"(ptr null, ptr %6, ptr %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"runtime/malgo/Builtin.mlg.$appendString#_curry_3968"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3969_0", ptr %"runtime/malgo/Builtin.mlg.$y_3970_0") {
  %2 = call ptr @malgo_string_append(ptr %"runtime/malgo/Builtin.mlg.$x_3969_0", ptr %"runtime/malgo/Builtin.mlg.$y_3970_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.$malgo_string_append_curry_2142"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_2143_0", ptr %"runtime/malgo/Builtin.mlg.$p_2144_0") {
  %2 = call ptr @malgo_string_append(ptr %"runtime/malgo/Builtin.mlg.$p_2143_0", ptr %"runtime/malgo/Builtin.mlg.$p_2144_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.String#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3187"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.appendString#"(ptr %0, ptr %"runtime/malgo/Builtin.mlg.$x_3963_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$x_3963_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3187", ptr %let_func_0, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3193"(ptr %0, ptr %1) {
  %"appendString#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %"appendString#_0" = load ptr, ptr %"appendString#_addr_0", align 8
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %d_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %d_0, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %10 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %12)
  %23 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3194"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %"appendString#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"appendString#_0" = load ptr, ptr %"appendString#_addr_0", align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %d_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %d_0, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %10 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %12)
  %23 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3195"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %"appendString#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"appendString#_0" = load ptr, ptr %"appendString#_addr_0", align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %d_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %d_0, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %10 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %12)
  %23 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3196"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %"appendString#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %"appendString#_0" = load ptr, ptr %"appendString#_addr_0", align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %d_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %d_0, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %10 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"appendString#_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %7)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %12)
  %23 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %22)
  ret ptr %27

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3188"(ptr %0, ptr %1) {
  %showDictB_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 0
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %showDictA_addr_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 1
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %"appendString#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 2
  %"appendString#_0" = load ptr, ptr %"appendString#_addr_0", align 8
  %"String#_addr_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %0, i32 0, i32 3
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <16 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %2
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, { ptr } }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %10, i32 0, i32 1, i32 0
  store ptr @str3189, ptr %12, align 8
  %13 = call ptr @malgo_hash_table_get(ptr %showDictA_0, ptr @str3190)
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %7)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr @str3191, ptr %21, align 8
  %22 = call ptr @malgo_hash_table_get(ptr %showDictB_0, ptr @str3190)
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, ptr %9)
  %28 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %29 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 0
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { ptr } }, ptr %28, i32 0, i32 1, i32 0
  store ptr @str3192, ptr %30, align 8
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %"appendString#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"appendString#_0", ptr %"appendString#_1", align 8
  %d_0 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %27, ptr %d_0, align 8
  %"String#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i32 0, i32 2
  store ptr %"String#_0", ptr %"String#_1", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %31, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3193", ptr %let_func_0, align 8
  %32 = call ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr null, ptr %27, ptr %28)
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %d_1 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %19, ptr %d_1, align 8
  %"appendString#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 1
  store ptr %"appendString#_0", ptr %"appendString#_2", align 8
  %"String#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i32 0, i32 2
  store ptr %"String#_0", ptr %"String#_2", align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3194", ptr %let_func_1, align 8
  %34 = call ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr null, ptr %19, ptr %32)
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %d_2 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %18, ptr %d_2, align 8
  %"appendString#_3" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 1
  store ptr %"appendString#_0", ptr %"appendString#_3", align 8
  %"String#_3" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_4, i32 0, i32 2
  store ptr %"String#_0", ptr %"String#_3", align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3195", ptr %let_func_2, align 8
  %36 = call ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr null, ptr %18, ptr %34)
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %d_3 = getelementptr { ptr, ptr, ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %10, ptr %d_3, align 8
  %"appendString#_4" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_6, i32 0, i32 1
  store ptr %"appendString#_0", ptr %"appendString#_4", align 8
  %"String#_4" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_6, i32 0, i32 2
  store ptr %"String#_0", ptr %"String#_4", align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3196", ptr %let_func_3, align 8
  %38 = call ptr @"runtime/malgo/Builtin.mlg.$appendString_curry_3984"(ptr null, ptr %10, ptr %36)
  ret ptr %38

switch_default_0:                                 ; preds = %2
  unreachable
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal ptr @"test/testcases/malgo/Show.mlg.$showTuple2_curry_186"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$showDictA_187_0", ptr %"test/testcases/malgo/Show.mlg.$showDictB_188_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr, ptr, ptr }, ptr null, i32 1) to i64))
  %showDictB_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Show.mlg.$showDictB_188_0", ptr %showDictB_0, align 8
  %showDictA_0 = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %"test/testcases/malgo/Show.mlg.$showDictA_187_0", ptr %showDictA_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"appendString#_capture_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %"appendString#_capture_0", align 8
  %"appendString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.appendString#", ptr %"appendString#_func_0", align 8
  %"appendString#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 2
  store ptr %3, ptr %"appendString#_0", align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %"String#_0" = getelementptr { ptr, ptr, ptr, ptr }, ptr %fun_capture_0, i32 0, i32 3
  store ptr %4, ptr %"String#_0", align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3188", ptr %fun_func_0, align 8
  %5 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %5, ptr @str3190, ptr %2)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/Show.mlg.show"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$record_119_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/Show.mlg.$record_119_0", ptr @str3190)
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3197"(ptr %0, ptr %1) {
  %showDict_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDict_0 = load ptr, ptr %showDict_addr_0, align 8
  %3 = call ptr @malgo_hash_table_get(ptr %showDict_0, ptr @str3190)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %1)
  %9 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %11 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr @malgo_print_string(ptr %13)
  ret ptr %14

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.print"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$showDict_121_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDict_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Show.mlg.$showDict_121_0", ptr %showDict_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3197", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3199"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"runtime/malgo/Builtin.mlg.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3198"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %x_0, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %5 = getelementptr { i8, { ptr } }, ptr %x_0, i32 0, i32 1
  %6 = getelementptr { ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %10 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %7, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3199", ptr %let_func_0, align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr %18, ptr %21, align 8
  ret ptr %19

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_1:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.<>"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$x_137_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Show.mlg.$x_137_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3198", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3200"(ptr %0, ptr %1) {
  %showDictA_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/Show.mlg.$showTuple2_curry_186"(ptr null, ptr %showDictA_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Show.mlg.showTuple2"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$showDictA_149_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %showDictA_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Show.mlg.$showDictA_149_0", ptr %showDictA_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3200", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Show.mlg.#let_closure_3201"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @malgo_hash_table_get(ptr %cast_0, ptr @str3190)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %1)
  %9 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %2
  %11 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %12 = getelementptr { ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr @malgo_print_string(ptr %13)
  ret ptr %14

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Show.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Show.mlg.$$__225_0") {
  %2 = load ptr, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %3 = load ptr, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  %4 = call ptr @"test/testcases/malgo/Show.mlg.$showTuple2_curry_186"(ptr null, ptr %2, ptr %3)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %4, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#let_closure_3201", ptr %let_func_0, align 8
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

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Show.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Show.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3206"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %2
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

define internal void @"malgo_load_test/testcases/malgo/Show.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr @"test/testcases/malgo/Show.mlg.#fun_closure_3206", ptr %fun_func_0, align 8
  %2 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %2, ptr @str3190, ptr %1)
  store ptr %2, ptr @"test/testcases/malgo/Show.mlg.showInt32", align 8
  ret void
}
