; ModuleID = './test/testcases/malgo/TestList.mlg'
source_filename = "./test/testcases/malgo/TestList.mlg"

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

define internal ptr @TestList.main(ptr %0, ptr %"TestList.$$__41_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %5, i32 0, i32 1, i32 0
  store i32 2, ptr %7, align 4
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { i32 } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %8, i32 0, i32 1, i32 0
  store i32 3, ptr %10, align 4
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %13, i32 0, i32 0
  store i8 1, ptr %14, align 1
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %13, i32 0, i32 1, i32 0
  store ptr %8, ptr %15, align 8
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %13, i32 0, i32 1, i32 1
  store ptr %11, ptr %16, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 0
  store i8 1, ptr %18, align 1
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %5, ptr %19, align 8
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 1
  store ptr %13, ptr %20, align 8
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %21, i32 0, i32 0
  store i8 1, ptr %22, align 1
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %21, i32 0, i32 1, i32 0
  store ptr %2, ptr %23, align 8
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %21, i32 0, i32 1, i32 1
  store ptr %17, ptr %24, align 8
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, {} }, ptr %25, i32 0, i32 0
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, <16 x i8> }, ptr %21, i32 0, i32 0
  %28 = load i8, ptr %27, align 1
  switch i8 %28, label %switch_default_4 [
    i8 0, label %switch_branch_Prelude.Nil_0
    i8 1, label %switch_branch_Prelude.Cons_0
  ]

switch_branch_Prelude.Nil_0:                      ; preds = %1
  %29 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %30 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  store i8 0, ptr %30, align 1
  %31 = call ptr @malgo_exit_failure(ptr %29)
  %32 = getelementptr { i8, <4 x i8> }, ptr %31, i32 0, i32 0
  %33 = load i8, ptr %32, align 1
  switch i8 %33, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %switch_branch_Prelude.Nil_0
  %34 = getelementptr { i8, { i32 } }, ptr %31, i32 0, i32 1
  %35 = getelementptr { i32 }, ptr %34, i32 0, i32 0
  %36 = load i32, ptr %35, align 4
  %37 = call ptr @malgo_int32_t_to_string(i32 %36)
  %38 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %39 = getelementptr { i8, { ptr } }, ptr %38, i32 0, i32 0
  store i8 0, ptr %39, align 1
  %40 = getelementptr { i8, { ptr } }, ptr %38, i32 0, i32 1, i32 0
  store ptr %37, ptr %40, align 8
  %41 = getelementptr { i8, <8 x i8> }, ptr %38, i32 0, i32 0
  %42 = load i8, ptr %41, align 1
  switch i8 %42, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int32#_0"
  %43 = getelementptr { i8, { ptr } }, ptr %38, i32 0, i32 1
  %44 = getelementptr { ptr }, ptr %43, i32 0, i32 0
  %45 = load ptr, ptr %44, align 8
  %46 = call ptr @malgo_print_string(ptr %45)
  %47 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %48 = getelementptr { i8, {} }, ptr %47, i32 0, i32 0
  store i8 0, ptr %48, align 1
  %49 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %50 = getelementptr { i8, {} }, ptr %49, i32 0, i32 0
  store i8 0, ptr %50, align 1
  %51 = call ptr @malgo_newline(ptr %49)
  ret ptr %51

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Prelude.Nil_0
  unreachable

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %52 = getelementptr { i8, { ptr, ptr } }, ptr %21, i32 0, i32 1
  %53 = getelementptr { ptr, ptr }, ptr %52, i32 0, i32 0
  %54 = load ptr, ptr %53, align 8
  %55 = getelementptr { ptr, ptr }, ptr %52, i32 0, i32 1
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { i8, <4 x i8> }, ptr %54, i32 0, i32 0
  %58 = load i8, ptr %57, align 1
  switch i8 %58, label %switch_default_3 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %switch_branch_Prelude.Cons_0
  %59 = getelementptr { i8, { i32 } }, ptr %54, i32 0, i32 1
  %60 = getelementptr { i32 }, ptr %59, i32 0, i32 0
  %61 = load i32, ptr %60, align 4
  %62 = call ptr @malgo_int32_t_to_string(i32 %61)
  %63 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %64 = getelementptr { i8, { ptr } }, ptr %63, i32 0, i32 0
  store i8 0, ptr %64, align 1
  %65 = getelementptr { i8, { ptr } }, ptr %63, i32 0, i32 1, i32 0
  store ptr %62, ptr %65, align 8
  %66 = getelementptr { i8, <8 x i8> }, ptr %63, i32 0, i32 0
  %67 = load i8, ptr %66, align 1
  switch i8 %67, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.Int32#_1"
  %68 = getelementptr { i8, { ptr } }, ptr %63, i32 0, i32 1
  %69 = getelementptr { ptr }, ptr %68, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = call ptr @malgo_print_string(ptr %70)
  %72 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %73 = getelementptr { i8, {} }, ptr %72, i32 0, i32 0
  store i8 0, ptr %73, align 1
  %74 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %75 = getelementptr { i8, {} }, ptr %74, i32 0, i32 0
  store i8 0, ptr %75, align 1
  %76 = call ptr @malgo_newline(ptr %74)
  ret ptr %76

switch_default_2:                                 ; preds = %"switch_branch_Builtin.Int32#_1"
  unreachable

switch_default_3:                                 ; preds = %switch_branch_Prelude.Cons_0
  unreachable

switch_default_4:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_TestList()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @TestList.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_TestList() {
  ret void
}
