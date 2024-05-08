; ModuleID = 'test/testcases/malgo/FieldPrefix.mlg'
source_filename = "test/testcases/malgo/FieldPrefix.mlg"

@"test/testcases/malgo/FieldPrefix.mlg.zero3D" = global ptr undef
@"test/testcases/malgo/FieldPrefix.mlg.zero2D" = global ptr undef
@str2924 = unnamed_addr constant [2 x i8] c"x\00"
@str2925 = unnamed_addr constant [2 x i8] c"y\00"
@str2926 = unnamed_addr constant [3 x i8] c", \00"
@str2931 = unnamed_addr constant [2 x i8] c"z\00"

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

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @"test/testcases/malgo/FieldPrefix.mlg.y2D"(ptr %0, ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_61_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_61_0", ptr @str2924)
  %3 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_61_0", ptr @str2925)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/FieldPrefix.mlg.x2D"(ptr %0, ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_64_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_64_0", ptr @str2924)
  %3 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_64_0", ptr @str2925)
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/FieldPrefix.mlg.print2D"(ptr %0, ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_67_0") {
  %2 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_67_0", ptr @str2924)
  %3 = call ptr @malgo_hash_table_get(ptr %"test/testcases/malgo/FieldPrefix.mlg.$record_67_0", ptr @str2925)
  %4 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %5 = load i8, ptr %4, align 1
  switch i8 %5, label %switch_default_4 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0": ; preds = %1
  %6 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %7 = getelementptr { i32 }, ptr %6, i32 0, i32 0
  %8 = load i32, ptr %7, align 4
  %9 = call ptr @malgo_int32_t_to_string(i32 %8)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, { ptr } }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %10, i32 0, i32 1, i32 0
  store ptr %9, ptr %12, align 8
  %13 = getelementptr { i8, <8 x i8> }, ptr %10, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_3 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_0": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  %15 = getelementptr { i8, { ptr } }, ptr %10, i32 0, i32 1
  %16 = getelementptr { ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr @malgo_print_string(ptr %17)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1, i32 0
  store ptr @str2926, ptr %21, align 8
  %22 = getelementptr { i8, <8 x i8> }, ptr %19, i32 0, i32 0
  %23 = load i8, ptr %22, align 1
  switch i8 %23, label %switch_default_2 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  %24 = getelementptr { i8, { ptr } }, ptr %19, i32 0, i32 1
  %25 = getelementptr { ptr }, ptr %24, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr @malgo_print_string(ptr %26)
  %28 = getelementptr { i8, <4 x i8> }, ptr %3, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_1 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  %30 = getelementptr { i8, { i32 } }, ptr %3, i32 0, i32 1
  %31 = getelementptr { i32 }, ptr %30, i32 0, i32 0
  %32 = load i32, ptr %31, align 4
  %33 = call ptr @malgo_int32_t_to_string(i32 %32)
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1, i32 0
  store ptr %33, ptr %36, align 8
  %37 = getelementptr { i8, <8 x i8> }, ptr %34, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_0 [
    i8 0, label %"switch_branch_runtime/malgo/Builtin.mlg.String#_2"
  ]

"switch_branch_runtime/malgo/Builtin.mlg.String#_2": ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  %39 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1
  %40 = getelementptr { ptr }, ptr %39, i32 0, i32 0
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr @malgo_print_string(ptr %41)
  ret ptr %42

switch_default_0:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_1"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.String#_0"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0"
  unreachable

switch_default_4:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/FieldPrefix.mlg.main"(ptr %0, ptr %"test/testcases/malgo/FieldPrefix.mlg.$$__81_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %print2D_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %print2D_capture_0, align 8
  %print2D_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/FieldPrefix.mlg.print2D", ptr %print2D_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = load ptr, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero2D", align 8
  %8 = call ptr %6(ptr %4, ptr %7)
  ret ptr %8
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/FieldPrefix.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/FieldPrefix.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal void @"malgo_load_test/testcases/malgo/FieldPrefix.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { float } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { float } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { float } }, ptr %1, i32 0, i32 1, i32 0
  store float 0.000000e+00, ptr %3, align 4
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { float } }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, { float } }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { float } }, ptr %4, i32 0, i32 1, i32 0
  store float 0.000000e+00, ptr %6, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { float } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { float } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { float } }, ptr %7, i32 0, i32 1, i32 0
  store float 0.000000e+00, ptr %9, align 4
  %10 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %10, ptr @str2924, ptr %1)
  call void @malgo_hash_table_insert(ptr %10, ptr @str2925, ptr %4)
  call void @malgo_hash_table_insert(ptr %10, ptr @str2931, ptr %7)
  store ptr %10, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero3D", align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 1, i32 0
  store i32 0, ptr %13, align 4
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 0, ptr %16, align 4
  %17 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %17, ptr @str2924, ptr %11)
  call void @malgo_hash_table_insert(ptr %17, ptr @str2925, ptr %14)
  store ptr %17, ptr @"test/testcases/malgo/FieldPrefix.mlg.zero2D", align 8
  ret void
}
