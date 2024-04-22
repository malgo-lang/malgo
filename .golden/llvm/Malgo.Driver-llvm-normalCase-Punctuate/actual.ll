; ModuleID = './test/testcases/malgo/Punctuate.mlg'
source_filename = "./test/testcases/malgo/Punctuate.mlg"

@str3172 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str3174 = unnamed_addr constant [6 x i8] c"SInt \00"
@str3176 = unnamed_addr constant [8 x i8] c"SList [\00"
@str3177 = unnamed_addr constant [3 x i8] c", \00"
@str3178 = unnamed_addr constant [2 x i8] c"]\00"
@str3181 = unnamed_addr constant [2 x i8] c"x\00"
@str3182 = unnamed_addr constant [2 x i8] c"y\00"
@str3183 = unnamed_addr constant [2 x i8] c"z\00"
@str3184 = unnamed_addr constant [1 x i8] zeroinitializer

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

define internal ptr @Punctuate.SList(ptr %0, ptr %"Punctuate.$p_117_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Punctuate.$p_117_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @Punctuate.SInt(ptr %0, ptr %"Punctuate.$p_115_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Punctuate.$p_115_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3170"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.malgo_string_append(ptr %0, ptr %"Builtin.$p_2140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2140_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3170", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Punctuate.Symbol(ptr %0, ptr %"Punctuate.$p_113_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Punctuate.$p_113_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"Prelude.$punctuate_curry_746"(ptr %0, ptr %"Prelude.$__747_0", ptr %"Prelude.$nil_748_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Prelude.$nil_748_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"Prelude.$nil_748_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_1
    i8 1, label %switch_branch_Prelude.Cons_1
  ]

switch_branch_Prelude.Nil_1:                      ; preds = %switch_branch_Prelude.Cons_0
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 0
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 1, i32 0
  store ptr %8, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 1, i32 1
  store ptr %13, ptr %18, align 8
  ret ptr %15

switch_branch_Prelude.Cons_1:                     ; preds = %switch_branch_Prelude.Cons_0
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %20 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr @"Prelude.$punctuate_curry_746"(ptr null, ptr %"Prelude.$__747_0", ptr %10)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %25, i32 0, i32 0
  store i8 1, ptr %26, align 1
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %25, i32 0, i32 1, i32 0
  store ptr %"Prelude.$__747_0", ptr %27, align 8
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %25, i32 0, i32 1, i32 1
  store ptr %24, ptr %28, align 8
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, { ptr, ptr } }, ptr %29, i32 0, i32 0
  store i8 1, ptr %30, align 1
  %31 = getelementptr { i8, { ptr, ptr } }, ptr %29, i32 0, i32 1, i32 0
  store ptr %8, ptr %31, align 8
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %29, i32 0, i32 1, i32 1
  store ptr %25, ptr %32, align 8
  ret ptr %29

switch_default_0:                                 ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.#let_closure_3171"(ptr %0, ptr %1) {
  %eta_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %eta_0 = load ptr, ptr %eta_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3169"(ptr null, ptr %eta_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.<>"(ptr %0, ptr %"Punctuate.$eta_119_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %eta_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Punctuate.$eta_119_0", ptr %eta_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3171", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Prelude.$mapList_curry_790"(ptr %0, ptr %"Prelude.$__791_0", ptr %"Prelude.$nil_792_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Prelude.$nil_792_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"Prelude.$nil_792_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = call ptr @"Prelude.$mapList_curry_790"(ptr null, ptr %"Prelude.$__791_0", ptr %10)
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 0
  store i8 1, ptr %18, align 1
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %15, ptr %19, align 8
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 1
  store ptr %16, ptr %20, align 8
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$raw_let_3168"(ptr %0, ptr %"Punctuate.$p_709_0", ptr %"Punctuate.$y_718_0") {
  %2 = call ptr @Builtin.malgo_string_append(ptr null, ptr %"Punctuate.$p_709_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Punctuate.$y_718_0")
  ret ptr %7
}

define internal ptr @"Punctuate.#let_closure_3173"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3175"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3179"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3180"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Punctuate.show(ptr %0, ptr %"Punctuate.$symbol_120_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$symbol_120_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_9 [
    i8 0, label %switch_branch_Punctuate.Symbol_0
    i8 1, label %switch_branch_Punctuate.SInt_0
    i8 2, label %switch_branch_Punctuate.SList_0
  ]

switch_branch_Punctuate.Symbol_0:                 ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$symbol_120_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr @str3172, ptr %9, align 8
  %10 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_Punctuate.Symbol_0
  %12 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %13 = getelementptr { ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %16 = load i8, ptr %15, align 1
  switch i8 %16, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %17 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %18 = getelementptr { ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %14, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3173", ptr %let_func_0, align 8
  %21 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %20, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr %24(ptr %22, ptr %19)
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  ret ptr %26

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Punctuate.Symbol_0
  unreachable

switch_branch_Punctuate.SInt_0:                   ; preds = %1
  %29 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$symbol_120_0", i32 0, i32 1
  %30 = getelementptr { ptr }, ptr %29, i32 0, i32 0
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %33 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 0
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1, i32 0
  store ptr @str3174, ptr %34, align 8
  %35 = getelementptr { i8, <4 x i8> }, ptr %31, i32 0, i32 0
  %36 = load i8, ptr %35, align 1
  switch i8 %36, label %switch_default_4 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Punctuate.SInt_0
  %37 = getelementptr { i8, { i32 } }, ptr %31, i32 0, i32 1
  %38 = getelementptr { i32 }, ptr %37, i32 0, i32 0
  %39 = load i32, ptr %38, align 4
  %40 = call ptr @malgo_int32_t_to_string(i32 %39)
  %41 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %42 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 0
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1, i32 0
  store ptr %40, ptr %43, align 8
  %44 = getelementptr { i8, <8 x i8> }, ptr %32, i32 0, i32 0
  %45 = load i8, ptr %44, align 1
  switch i8 %45, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.String#_2"
  ]

"switch_branch_Builtin.String#_2":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %46 = getelementptr { i8, { ptr } }, ptr %32, i32 0, i32 1
  %47 = getelementptr { ptr }, ptr %46, i32 0, i32 0
  %48 = load ptr, ptr %47, align 8
  %49 = getelementptr { i8, <8 x i8> }, ptr %41, i32 0, i32 0
  %50 = load i8, ptr %49, align 1
  switch i8 %50, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_3"
  ]

"switch_branch_Builtin.String#_3":                ; preds = %"switch_branch_Builtin.String#_2"
  %51 = getelementptr { i8, { ptr } }, ptr %41, i32 0, i32 1
  %52 = getelementptr { ptr }, ptr %51, i32 0, i32 0
  %53 = load ptr, ptr %52, align 8
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %48, ptr %p_1, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3175", ptr %let_func_1, align 8
  %55 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = call ptr %58(ptr %56, ptr %53)
  %60 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %61 = getelementptr { i8, { ptr } }, ptr %60, i32 0, i32 0
  store i8 0, ptr %61, align 1
  %62 = getelementptr { i8, { ptr } }, ptr %60, i32 0, i32 1, i32 0
  store ptr %59, ptr %62, align 8
  ret ptr %60

switch_default_2:                                 ; preds = %"switch_branch_Builtin.String#_2"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_4:                                 ; preds = %switch_branch_Punctuate.SInt_0
  unreachable

switch_branch_Punctuate.SList_0:                  ; preds = %1
  %63 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$symbol_120_0", i32 0, i32 1
  %64 = getelementptr { ptr }, ptr %63, i32 0, i32 0
  %65 = load ptr, ptr %64, align 8
  %66 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %67 = getelementptr { i8, { ptr } }, ptr %66, i32 0, i32 0
  store i8 0, ptr %67, align 1
  %68 = getelementptr { i8, { ptr } }, ptr %66, i32 0, i32 1, i32 0
  store ptr @str3176, ptr %68, align 8
  %69 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %70 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 0
  store i8 0, ptr %70, align 1
  %71 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 1, i32 0
  store ptr @str3177, ptr %71, align 8
  %72 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %show_capture_0 = getelementptr { ptr, ptr }, ptr %72, i32 0, i32 0
  store ptr null, ptr %show_capture_0, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %72, i32 0, i32 1
  store ptr @Punctuate.show, ptr %show_func_0, align 8
  %73 = call ptr @"Prelude.$mapList_curry_790"(ptr null, ptr %72, ptr %65)
  %74 = call ptr @"Prelude.$punctuate_curry_746"(ptr null, ptr %69, ptr %73)
  %75 = call ptr @Prelude.concatString(ptr null, ptr %74)
  %76 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %77 = getelementptr { i8, { ptr } }, ptr %76, i32 0, i32 0
  store i8 0, ptr %77, align 1
  %78 = getelementptr { i8, { ptr } }, ptr %76, i32 0, i32 1, i32 0
  store ptr @str3178, ptr %78, align 8
  %79 = getelementptr { i8, <8 x i8> }, ptr %75, i32 0, i32 0
  %80 = load i8, ptr %79, align 1
  switch i8 %80, label %switch_default_8 [
    i8 0, label %"switch_branch_Builtin.String#_4"
  ]

"switch_branch_Builtin.String#_4":                ; preds = %switch_branch_Punctuate.SList_0
  %81 = getelementptr { i8, { ptr } }, ptr %75, i32 0, i32 1
  %82 = getelementptr { ptr }, ptr %81, i32 0, i32 0
  %83 = load ptr, ptr %82, align 8
  %84 = getelementptr { i8, <8 x i8> }, ptr %76, i32 0, i32 0
  %85 = load i8, ptr %84, align 1
  switch i8 %85, label %switch_default_7 [
    i8 0, label %"switch_branch_Builtin.String#_5"
  ]

"switch_branch_Builtin.String#_5":                ; preds = %"switch_branch_Builtin.String#_4"
  %86 = getelementptr { i8, { ptr } }, ptr %76, i32 0, i32 1
  %87 = getelementptr { ptr }, ptr %86, i32 0, i32 0
  %88 = load ptr, ptr %87, align 8
  %89 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %83, ptr %p_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %89, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %89, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3179", ptr %let_func_2, align 8
  %90 = getelementptr { ptr, ptr }, ptr %89, i32 0, i32 0
  %91 = load ptr, ptr %90, align 8
  %92 = getelementptr { ptr, ptr }, ptr %89, i32 0, i32 1
  %93 = load ptr, ptr %92, align 8
  %94 = call ptr %93(ptr %91, ptr %88)
  %95 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %96 = getelementptr { i8, { ptr } }, ptr %95, i32 0, i32 0
  store i8 0, ptr %96, align 1
  %97 = getelementptr { i8, { ptr } }, ptr %95, i32 0, i32 1, i32 0
  store ptr %94, ptr %97, align 8
  %98 = getelementptr { i8, <8 x i8> }, ptr %66, i32 0, i32 0
  %99 = load i8, ptr %98, align 1
  switch i8 %99, label %switch_default_6 [
    i8 0, label %"switch_branch_Builtin.String#_6"
  ]

"switch_branch_Builtin.String#_6":                ; preds = %"switch_branch_Builtin.String#_5"
  %100 = getelementptr { i8, { ptr } }, ptr %66, i32 0, i32 1
  %101 = getelementptr { ptr }, ptr %100, i32 0, i32 0
  %102 = load ptr, ptr %101, align 8
  %103 = getelementptr { i8, <8 x i8> }, ptr %95, i32 0, i32 0
  %104 = load i8, ptr %103, align 1
  switch i8 %104, label %switch_default_5 [
    i8 0, label %"switch_branch_Builtin.String#_7"
  ]

"switch_branch_Builtin.String#_7":                ; preds = %"switch_branch_Builtin.String#_6"
  %105 = getelementptr { i8, { ptr } }, ptr %95, i32 0, i32 1
  %106 = getelementptr { ptr }, ptr %105, i32 0, i32 0
  %107 = load ptr, ptr %106, align 8
  %108 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %102, ptr %p_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %108, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %108, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3180", ptr %let_func_3, align 8
  %109 = getelementptr { ptr, ptr }, ptr %108, i32 0, i32 0
  %110 = load ptr, ptr %109, align 8
  %111 = getelementptr { ptr, ptr }, ptr %108, i32 0, i32 1
  %112 = load ptr, ptr %111, align 8
  %113 = call ptr %112(ptr %110, ptr %107)
  %114 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %115 = getelementptr { i8, { ptr } }, ptr %114, i32 0, i32 0
  store i8 0, ptr %115, align 1
  %116 = getelementptr { i8, { ptr } }, ptr %114, i32 0, i32 1, i32 0
  store ptr %113, ptr %116, align 8
  ret ptr %114

switch_default_5:                                 ; preds = %"switch_branch_Builtin.String#_6"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_Builtin.String#_5"
  unreachable

switch_default_7:                                 ; preds = %"switch_branch_Builtin.String#_4"
  unreachable

switch_default_8:                                 ; preds = %switch_branch_Punctuate.SList_0
  unreachable

switch_default_9:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$raw_let_3169"(ptr %0, ptr %"Punctuate.$eta_119_0", ptr %"Punctuate.$string#_2980_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$eta_119_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$eta_119_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$string#_2980_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$string#_2980_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @Builtin.malgo_string_append(ptr null, ptr %6)
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

define internal ptr @Punctuate.main(ptr %0, ptr %"Punctuate.$$__159_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str3181, ptr %4, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr @str3182, ptr %10, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1, i32 0
  store ptr %8, ptr %13, align 8
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr @str3183, ptr %16, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %14, ptr %19, align 8
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, {} }, ptr %20, i32 0, i32 0
  store i8 0, ptr %21, align 1
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 0
  store i8 1, ptr %23, align 1
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 0
  store ptr %17, ptr %24, align 8
  %25 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1, i32 1
  store ptr %20, ptr %25, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 2, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %22, ptr %28, align 8
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 0
  store i8 1, ptr %32, align 1
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 1, i32 0
  store ptr %26, ptr %33, align 8
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 1, i32 1
  store ptr %29, ptr %34, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 0
  store i8 1, ptr %36, align 1
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 1, i32 0
  store ptr %11, ptr %37, align 8
  %38 = getelementptr { i8, { ptr, ptr } }, ptr %35, i32 0, i32 1, i32 1
  store ptr %31, ptr %38, align 8
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %40 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 0
  store i8 2, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %39, i32 0, i32 1, i32 0
  store ptr %35, ptr %41, align 8
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %43 = getelementptr { i8, {} }, ptr %42, i32 0, i32 0
  store i8 0, ptr %43, align 1
  %44 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %45 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 0
  store i8 1, ptr %45, align 1
  %46 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 0
  store ptr %39, ptr %46, align 8
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %44, i32 0, i32 1, i32 1
  store ptr %42, ptr %47, align 8
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %49 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 0
  store i8 1, ptr %49, align 1
  %50 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 1, i32 0
  store ptr %5, ptr %50, align 8
  %51 = getelementptr { i8, { ptr, ptr } }, ptr %48, i32 0, i32 1, i32 1
  store ptr %44, ptr %51, align 8
  %52 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %53 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 0
  store i8 2, ptr %53, align 1
  %54 = getelementptr { i8, { ptr } }, ptr %52, i32 0, i32 1, i32 0
  store ptr %48, ptr %54, align 8
  %55 = call ptr @Punctuate.show(ptr null, ptr %52)
  %56 = getelementptr { i8, <8 x i8> }, ptr %55, i32 0, i32 0
  %57 = load i8, ptr %56, align 1
  switch i8 %57, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %58 = getelementptr { i8, { ptr } }, ptr %55, i32 0, i32 1
  %59 = getelementptr { ptr }, ptr %58, i32 0, i32 0
  %60 = load ptr, ptr %59, align 8
  %61 = call ptr @malgo_print_string(ptr %60)
  %62 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %63 = getelementptr { i8, {} }, ptr %62, i32 0, i32 0
  store i8 0, ptr %63, align 1
  %64 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %65 = getelementptr { i8, {} }, ptr %64, i32 0, i32 0
  store i8 0, ptr %65, align 1
  %66 = call ptr @malgo_newline(ptr %64)
  ret ptr %66

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$appendString#_curry_3968"(ptr %0, ptr %"Builtin.$x_3969_0", ptr %"Builtin.$y_3970_0") {
  %2 = call ptr @malgo_string_append(ptr %"Builtin.$x_3969_0", ptr %"Builtin.$y_3970_0")
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3185"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3168"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Prelude.concatString(ptr %0, ptr %"Prelude.$nil_974_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Prelude.$nil_974_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1, i32 0
  store ptr @str3184, ptr %6, align 8
  ret ptr %4

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %"Prelude.$nil_974_0", i32 0, i32 1
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @Prelude.concatString(ptr null, ptr %11)
  %13 = getelementptr { i8, <8 x i8> }, ptr %9, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %switch_branch_Prelude.Cons_0
  %15 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1
  %16 = getelementptr { ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { i8, <8 x i8> }, ptr %12, i32 0, i32 0
  %19 = load i8, ptr %18, align 1
  switch i8 %19, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %20 = getelementptr { i8, { ptr } }, ptr %12, i32 0, i32 1
  %21 = getelementptr { ptr }, ptr %20, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %17, ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3185", ptr %let_func_0, align 8
  %24 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %17, ptr %22)
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %25, i32 0, i32 1, i32 0
  store ptr %24, ptr %27, align 8
  ret ptr %25

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Punctuate()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Punctuate.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_Punctuate() {
  ret void
}
