; ModuleID = './test/testcases/malgo/Punctuate.mlg'
source_filename = "./test/testcases/malgo/Punctuate.mlg"

@str3915 = unnamed_addr constant [8 x i8] c"Symbol \00"
@str3917 = unnamed_addr constant [6 x i8] c"SInt \00"
@str3919 = unnamed_addr constant [8 x i8] c"SList [\00"
@str3920 = unnamed_addr constant [3 x i8] c", \00"
@str3921 = unnamed_addr constant [2 x i8] c"]\00"
@str3927 = unnamed_addr constant [2 x i8] c"x\00"
@str3928 = unnamed_addr constant [2 x i8] c"y\00"
@str3929 = unnamed_addr constant [2 x i8] c"z\00"
@str3930 = unnamed_addr constant [1 x i8] zeroinitializer

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

define internal ptr @"Punctuate.$raw_let_3900"(ptr %0, ptr %"Punctuate.$d_651_0", ptr %"Punctuate.$p_663_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Punctuate.$d_651_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Punctuate.$p_663_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.$raw_let_3902"(ptr %0, ptr %"Prelude.$p_691_0", ptr %"Prelude.$p_692_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Prelude.$p_691_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Prelude.$p_692_0", ptr %5, align 8
  ret ptr %2
}

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

define internal ptr @"Punctuate.#let_closure_3907"(ptr %0, ptr %1) {
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
  store ptr @"Punctuate.#let_closure_3907", ptr %let_func_0, align 8
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

define internal ptr @"Prelude.$Cons_curry_694"(ptr %0, ptr %"Prelude.$p_695_0", ptr %"Prelude.$p_696_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Prelude.$p_695_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Prelude.$p_696_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3908"(ptr %0, ptr %1) {
  %eta_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %eta_0 = load ptr, ptr %eta_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3905"(ptr null, ptr %eta_0, ptr %1)
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
  store ptr @"Punctuate.#let_closure_3908", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Prelude.Nil(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3909"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Prelude.$Cons_curry_694"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3910"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"Prelude.$mapList_curry_790"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.$raw_let_3903"(ptr %0, ptr %"Prelude.$__777_0", ptr %"Prelude.$nil_778_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Prelude.$nil_778_0", i32 0, i32 0
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
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"Prelude.$nil_778_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %"Prelude.$__777_0", i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %"Prelude.$__777_0", i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %17 = load i8, ptr %16, align 1
  switch i8 %17, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_1
    i8 1, label %switch_branch_Prelude.Cons_1
  ]

switch_branch_Prelude.Nil_1:                      ; preds = %switch_branch_Prelude.Cons_0
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 0
  store i8 1, ptr %21, align 1
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 1, i32 0
  store ptr %15, ptr %22, align 8
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 1, i32 1
  store ptr %18, ptr %23, align 8
  ret ptr %20

switch_branch_Prelude.Cons_1:                     ; preds = %switch_branch_Prelude.Cons_0
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %25 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %"Prelude.$__777_0", i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %"Prelude.$__777_0", i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %26)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %33, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3909", ptr %let_func_0, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %"Prelude.$__777_0", ptr %__0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3910", ptr %let_func_1, align 8
  %36 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  %39 = load ptr, ptr %38, align 8
  %40 = call ptr %39(ptr %37, ptr %28)
  %41 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  %42 = load ptr, ptr %41, align 8
  %43 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  %44 = load ptr, ptr %43, align 8
  %45 = call ptr %44(ptr %42, ptr %40)
  %46 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %46, i32 0, i32 0
  store i8 1, ptr %47, align 1
  %48 = getelementptr { i8, { ptr, ptr } }, ptr %46, i32 0, i32 1, i32 0
  store ptr %15, ptr %48, align 8
  %49 = getelementptr { i8, { ptr, ptr } }, ptr %46, i32 0, i32 1, i32 1
  store ptr %45, ptr %49, align 8
  ret ptr %46

switch_default_0:                                 ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$let_3760"(ptr %0, ptr %"Punctuate.$nil_3761_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Punctuate.$nil_3761_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %4 = call ptr @Prelude.Nil(ptr null)
  ret ptr %4

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"Punctuate.$nil_3761_0", i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @Punctuate.show(ptr null, ptr %7)
  %11 = call ptr @Prelude.Cons(ptr null, ptr %10)
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %show_capture_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %show_capture_0, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @Punctuate.show, ptr %show_func_0, align 8
  %13 = call ptr @Prelude.mapList(ptr null, ptr %12)
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %9)
  %19 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %18)
  ret ptr %23

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$raw_let_3901"(ptr %0, ptr %"Prelude.$__791_0", ptr %"Punctuate.$nil_665_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Punctuate.$nil_665_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %4 = call ptr @Prelude.Nil(ptr null)
  ret ptr %4

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"Punctuate.$nil_665_0", i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr %13(ptr %11, ptr %7)
  %15 = call ptr @Prelude.Cons(ptr null, ptr %14)
  %16 = call ptr @Prelude.mapList(ptr null, ptr %"Prelude.$__791_0")
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %9)
  %22 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %21)
  ret ptr %26

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.#let_closure_3911"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3903"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @Prelude.mapList(ptr %0, ptr %"Prelude.$__777_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Prelude.$__777_0", ptr %__0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3911", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3912"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3900"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3913"(ptr %0, ptr %1) {
  %__addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %__0 = load ptr, ptr %__addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3901"(ptr null, ptr %__0, ptr %1)
  ret ptr %3
}

define internal ptr @"Prelude.$mapList_curry_790"(ptr %0, ptr %"Prelude.$__791_0", ptr %"Prelude.$nil_792_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Prelude.$nil_792_0", i32 0, i32 0
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
  %16 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %17 = load i8, ptr %16, align 1
  switch i8 %17, label %switch_default_0 [
    i8 0, label %switch_branch_Prelude.Nil_1
    i8 1, label %switch_branch_Prelude.Cons_1
  ]

switch_branch_Prelude.Nil_1:                      ; preds = %switch_branch_Prelude.Cons_0
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 0
  store i8 1, ptr %21, align 1
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 1, i32 0
  store ptr %15, ptr %22, align 8
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 1, i32 1
  store ptr %18, ptr %23, align 8
  ret ptr %20

switch_branch_Prelude.Cons_1:                     ; preds = %switch_branch_Prelude.Cons_0
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %25 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 0
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr { ptr, ptr }, ptr %"Prelude.$__791_0", i32 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = call ptr %32(ptr %30, ptr %26)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %33, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3912", ptr %let_func_0, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %__0 = getelementptr { ptr }, ptr %let_capture_2, i32 0, i32 0
  store ptr %"Prelude.$__791_0", ptr %__0, align 8
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3913", ptr %let_func_1, align 8
  %36 = call ptr @"Prelude.$mapList_curry_790"(ptr null, ptr %"Prelude.$__791_0", ptr %28)
  %37 = call ptr @"Prelude.$Cons_curry_694"(ptr null, ptr %33, ptr %36)
  %38 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %39 = getelementptr { i8, { ptr, ptr } }, ptr %38, i32 0, i32 0
  store i8 1, ptr %39, align 1
  %40 = getelementptr { i8, { ptr, ptr } }, ptr %38, i32 0, i32 1, i32 0
  store ptr %15, ptr %40, align 8
  %41 = getelementptr { i8, { ptr, ptr } }, ptr %38, i32 0, i32 1, i32 1
  store ptr %37, ptr %41, align 8
  ret ptr %38

switch_default_0:                                 ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$raw_let_3904"(ptr %0, ptr %"Punctuate.$p_1303_0", ptr %"Punctuate.$y_1312_0") {
  %2 = call ptr @Builtin.malgo_string_append(ptr null, ptr %"Punctuate.$p_1303_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Punctuate.$y_1312_0")
  ret ptr %7
}

define internal ptr @"Punctuate.#let_closure_3914"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3902"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Prelude.Cons(ptr %0, ptr %"Prelude.$p_691_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Prelude.$p_691_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3914", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3916"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3918"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3922"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3923"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3924"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3906"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3925"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Punctuate.#let_closure_3926"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Punctuate.show(ptr %0, ptr %"Punctuate.$symbol_120_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$symbol_120_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_14 [
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
  store ptr @str3915, ptr %9, align 8
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
  store ptr @"Punctuate.#let_closure_3916", ptr %let_func_0, align 8
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
  store ptr @str3917, ptr %34, align 8
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
  store ptr @"Punctuate.#let_closure_3918", ptr %let_func_1, align 8
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
  store ptr @str3919, ptr %68, align 8
  %69 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %70 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 0
  store i8 0, ptr %70, align 1
  %71 = getelementptr { i8, { ptr } }, ptr %69, i32 0, i32 1, i32 0
  store ptr @str3920, ptr %71, align 8
  %72 = getelementptr { i8, <16 x i8> }, ptr %65, i32 0, i32 0
  %73 = load i8, ptr %72, align 1
  switch i8 %73, label %switch_default_13 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %switch_branch_Punctuate.SList_0
  %74 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %75 = getelementptr { i8, {} }, ptr %74, i32 0, i32 0
  store i8 0, ptr %75, align 1
  %76 = call ptr @"Prelude.$punctuate_curry_746"(ptr null, ptr %69, ptr %74)
  %77 = call ptr @Prelude.concatString(ptr null, ptr %76)
  %78 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %79 = getelementptr { i8, { ptr } }, ptr %78, i32 0, i32 0
  store i8 0, ptr %79, align 1
  %80 = getelementptr { i8, { ptr } }, ptr %78, i32 0, i32 1, i32 0
  store ptr @str3921, ptr %80, align 8
  %81 = getelementptr { i8, <8 x i8> }, ptr %77, i32 0, i32 0
  %82 = load i8, ptr %81, align 1
  switch i8 %82, label %switch_default_8 [
    i8 0, label %"switch_branch_Builtin.String#_4"
  ]

"switch_branch_Builtin.String#_4":                ; preds = %switch_branch_Prelude.Nil_0
  %83 = getelementptr { i8, { ptr } }, ptr %77, i32 0, i32 1
  %84 = getelementptr { ptr }, ptr %83, i32 0, i32 0
  %85 = load ptr, ptr %84, align 8
  %86 = getelementptr { i8, <8 x i8> }, ptr %78, i32 0, i32 0
  %87 = load i8, ptr %86, align 1
  switch i8 %87, label %switch_default_7 [
    i8 0, label %"switch_branch_Builtin.String#_5"
  ]

"switch_branch_Builtin.String#_5":                ; preds = %"switch_branch_Builtin.String#_4"
  %88 = getelementptr { i8, { ptr } }, ptr %78, i32 0, i32 1
  %89 = getelementptr { ptr }, ptr %88, i32 0, i32 0
  %90 = load ptr, ptr %89, align 8
  %91 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_2 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %85, ptr %p_2, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3922", ptr %let_func_2, align 8
  %92 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 0
  %93 = load ptr, ptr %92, align 8
  %94 = getelementptr { ptr, ptr }, ptr %91, i32 0, i32 1
  %95 = load ptr, ptr %94, align 8
  %96 = call ptr %95(ptr %93, ptr %90)
  %97 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %98 = getelementptr { i8, { ptr } }, ptr %97, i32 0, i32 0
  store i8 0, ptr %98, align 1
  %99 = getelementptr { i8, { ptr } }, ptr %97, i32 0, i32 1, i32 0
  store ptr %96, ptr %99, align 8
  %100 = getelementptr { i8, <8 x i8> }, ptr %66, i32 0, i32 0
  %101 = load i8, ptr %100, align 1
  switch i8 %101, label %switch_default_6 [
    i8 0, label %"switch_branch_Builtin.String#_6"
  ]

"switch_branch_Builtin.String#_6":                ; preds = %"switch_branch_Builtin.String#_5"
  %102 = getelementptr { i8, { ptr } }, ptr %66, i32 0, i32 1
  %103 = getelementptr { ptr }, ptr %102, i32 0, i32 0
  %104 = load ptr, ptr %103, align 8
  %105 = getelementptr { i8, <8 x i8> }, ptr %97, i32 0, i32 0
  %106 = load i8, ptr %105, align 1
  switch i8 %106, label %switch_default_5 [
    i8 0, label %"switch_branch_Builtin.String#_7"
  ]

"switch_branch_Builtin.String#_7":                ; preds = %"switch_branch_Builtin.String#_6"
  %107 = getelementptr { i8, { ptr } }, ptr %97, i32 0, i32 1
  %108 = getelementptr { ptr }, ptr %107, i32 0, i32 0
  %109 = load ptr, ptr %108, align 8
  %110 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_3 = getelementptr { ptr }, ptr %let_capture_6, i32 0, i32 0
  store ptr %104, ptr %p_3, align 8
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %110, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %110, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3923", ptr %let_func_3, align 8
  %111 = getelementptr { ptr, ptr }, ptr %110, i32 0, i32 0
  %112 = load ptr, ptr %111, align 8
  %113 = getelementptr { ptr, ptr }, ptr %110, i32 0, i32 1
  %114 = load ptr, ptr %113, align 8
  %115 = call ptr %114(ptr %112, ptr %109)
  %116 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %117 = getelementptr { i8, { ptr } }, ptr %116, i32 0, i32 0
  store i8 0, ptr %117, align 1
  %118 = getelementptr { i8, { ptr } }, ptr %116, i32 0, i32 1, i32 0
  store ptr %115, ptr %118, align 8
  ret ptr %116

switch_default_5:                                 ; preds = %"switch_branch_Builtin.String#_6"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_Builtin.String#_5"
  unreachable

switch_default_7:                                 ; preds = %"switch_branch_Builtin.String#_4"
  unreachable

switch_default_8:                                 ; preds = %switch_branch_Prelude.Nil_0
  unreachable

switch_branch_Prelude.Cons_0:                     ; preds = %switch_branch_Punctuate.SList_0
  %119 = getelementptr { i8, { ptr, ptr } }, ptr %65, i32 0, i32 1
  %120 = getelementptr { ptr, ptr }, ptr %119, i32 0, i32 0
  %121 = load ptr, ptr %120, align 8
  %122 = getelementptr { ptr, ptr }, ptr %119, i32 0, i32 1
  %123 = load ptr, ptr %122, align 8
  %124 = call ptr @Punctuate.show(ptr null, ptr %121)
  %125 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_8, i32 0, i32 0
  store ptr %124, ptr %d_0, align 8
  %let_capture_9 = getelementptr { ptr, ptr }, ptr %125, i32 0, i32 0
  store ptr %let_capture_8, ptr %let_capture_9, align 8
  %let_func_4 = getelementptr { ptr, ptr }, ptr %125, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3924", ptr %let_func_4, align 8
  %126 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %show_capture_0 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 0
  store ptr null, ptr %show_capture_0, align 8
  %show_func_0 = getelementptr { ptr, ptr }, ptr %126, i32 0, i32 1
  store ptr @Punctuate.show, ptr %show_func_0, align 8
  %127 = call ptr @"Prelude.$mapList_curry_790"(ptr null, ptr %126, ptr %123)
  %128 = call ptr @"Prelude.$Cons_curry_694"(ptr null, ptr %124, ptr %127)
  %129 = call ptr @"Prelude.$punctuate_curry_746"(ptr null, ptr %69, ptr %128)
  %130 = call ptr @Prelude.concatString(ptr null, ptr %129)
  %131 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %132 = getelementptr { i8, { ptr } }, ptr %131, i32 0, i32 0
  store i8 0, ptr %132, align 1
  %133 = getelementptr { i8, { ptr } }, ptr %131, i32 0, i32 1, i32 0
  store ptr @str3921, ptr %133, align 8
  %134 = getelementptr { i8, <8 x i8> }, ptr %130, i32 0, i32 0
  %135 = load i8, ptr %134, align 1
  switch i8 %135, label %switch_default_12 [
    i8 0, label %"switch_branch_Builtin.String#_8"
  ]

"switch_branch_Builtin.String#_8":                ; preds = %switch_branch_Prelude.Cons_0
  %136 = getelementptr { i8, { ptr } }, ptr %130, i32 0, i32 1
  %137 = getelementptr { ptr }, ptr %136, i32 0, i32 0
  %138 = load ptr, ptr %137, align 8
  %139 = getelementptr { i8, <8 x i8> }, ptr %131, i32 0, i32 0
  %140 = load i8, ptr %139, align 1
  switch i8 %140, label %switch_default_11 [
    i8 0, label %"switch_branch_Builtin.String#_9"
  ]

"switch_branch_Builtin.String#_9":                ; preds = %"switch_branch_Builtin.String#_8"
  %141 = getelementptr { i8, { ptr } }, ptr %131, i32 0, i32 1
  %142 = getelementptr { ptr }, ptr %141, i32 0, i32 0
  %143 = load ptr, ptr %142, align 8
  %144 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_4 = getelementptr { ptr }, ptr %let_capture_10, i32 0, i32 0
  store ptr %138, ptr %p_4, align 8
  %let_capture_11 = getelementptr { ptr, ptr }, ptr %144, i32 0, i32 0
  store ptr %let_capture_10, ptr %let_capture_11, align 8
  %let_func_5 = getelementptr { ptr, ptr }, ptr %144, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3925", ptr %let_func_5, align 8
  %145 = getelementptr { ptr, ptr }, ptr %144, i32 0, i32 0
  %146 = load ptr, ptr %145, align 8
  %147 = getelementptr { ptr, ptr }, ptr %144, i32 0, i32 1
  %148 = load ptr, ptr %147, align 8
  %149 = call ptr %148(ptr %146, ptr %143)
  %150 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %151 = getelementptr { i8, { ptr } }, ptr %150, i32 0, i32 0
  store i8 0, ptr %151, align 1
  %152 = getelementptr { i8, { ptr } }, ptr %150, i32 0, i32 1, i32 0
  store ptr %149, ptr %152, align 8
  %153 = getelementptr { i8, <8 x i8> }, ptr %66, i32 0, i32 0
  %154 = load i8, ptr %153, align 1
  switch i8 %154, label %switch_default_10 [
    i8 0, label %"switch_branch_Builtin.String#_10"
  ]

"switch_branch_Builtin.String#_10":               ; preds = %"switch_branch_Builtin.String#_9"
  %155 = getelementptr { i8, { ptr } }, ptr %66, i32 0, i32 1
  %156 = getelementptr { ptr }, ptr %155, i32 0, i32 0
  %157 = load ptr, ptr %156, align 8
  %158 = getelementptr { i8, <8 x i8> }, ptr %150, i32 0, i32 0
  %159 = load i8, ptr %158, align 1
  switch i8 %159, label %switch_default_9 [
    i8 0, label %"switch_branch_Builtin.String#_11"
  ]

"switch_branch_Builtin.String#_11":               ; preds = %"switch_branch_Builtin.String#_10"
  %160 = getelementptr { i8, { ptr } }, ptr %150, i32 0, i32 1
  %161 = getelementptr { ptr }, ptr %160, i32 0, i32 0
  %162 = load ptr, ptr %161, align 8
  %163 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_5 = getelementptr { ptr }, ptr %let_capture_12, i32 0, i32 0
  store ptr %157, ptr %p_5, align 8
  %let_capture_13 = getelementptr { ptr, ptr }, ptr %163, i32 0, i32 0
  store ptr %let_capture_12, ptr %let_capture_13, align 8
  %let_func_6 = getelementptr { ptr, ptr }, ptr %163, i32 0, i32 1
  store ptr @"Punctuate.#let_closure_3926", ptr %let_func_6, align 8
  %164 = getelementptr { ptr, ptr }, ptr %163, i32 0, i32 0
  %165 = load ptr, ptr %164, align 8
  %166 = getelementptr { ptr, ptr }, ptr %163, i32 0, i32 1
  %167 = load ptr, ptr %166, align 8
  %168 = call ptr %167(ptr %165, ptr %162)
  %169 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %170 = getelementptr { i8, { ptr } }, ptr %169, i32 0, i32 0
  store i8 0, ptr %170, align 1
  %171 = getelementptr { i8, { ptr } }, ptr %169, i32 0, i32 1, i32 0
  store ptr %168, ptr %171, align 8
  ret ptr %169

switch_default_9:                                 ; preds = %"switch_branch_Builtin.String#_10"
  unreachable

switch_default_10:                                ; preds = %"switch_branch_Builtin.String#_9"
  unreachable

switch_default_11:                                ; preds = %"switch_branch_Builtin.String#_8"
  unreachable

switch_default_12:                                ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_13:                                ; preds = %switch_branch_Punctuate.SList_0
  unreachable

switch_default_14:                                ; preds = %1
  unreachable
}

define internal ptr @"Punctuate.$raw_let_3905"(ptr %0, ptr %"Punctuate.$eta_119_0", ptr %"Punctuate.$string#_3609_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$eta_119_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$eta_119_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Punctuate.$string#_3609_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Punctuate.$string#_3609_0", i32 0, i32 1
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
  store ptr @str3927, ptr %4, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr @str3928, ptr %10, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1, i32 0
  store ptr %8, ptr %13, align 8
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr @str3929, ptr %16, align 8
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

define internal ptr @"Punctuate.$raw_let_3906"(ptr %0, ptr %"Punctuate.$d_3694_0", ptr %"Punctuate.$p_3759_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Punctuate.$d_3694_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Punctuate.$p_3759_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"Punctuate.#let_closure_3931"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Punctuate.$raw_let_3904"(ptr null, ptr %p_0, ptr %1)
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
  store ptr @str3930, ptr %6, align 8
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
  store ptr @"Punctuate.#let_closure_3931", ptr %let_func_0, align 8
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
  call void @malgo_load_Punctuate()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Punctuate.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_Punctuate() {
  ret void
}
