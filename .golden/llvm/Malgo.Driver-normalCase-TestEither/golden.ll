; ModuleID = 'test/testcases/malgo/TestEither.mlg'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str4533 = unnamed_addr constant [6 x i8] c"error\00"
@str4563 = unnamed_addr constant [12 x i8] c"unreachable\00"

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

define internal ptr @_M29let_x2410d_x5Fclosure_x2411ae47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"_$3$fe_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"_$3$fe_0" = load ptr, ptr %"_$3$fe_addr_0", align 8
  %3 = call ptr @_M25raw_x5Flet_x2410d_x24110a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %"_$3$fe_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M7andThen47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr %0, ptr %_M15_x5F_x243_x24fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$10d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"_$3$fe_0" = getelementptr { ptr }, ptr %"let$10d_capture_0", i32 0, i32 0
  store ptr %_M15_x5F_x243_x24fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, ptr %"_$3$fe_0", align 8
  %"let$10d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$10d_capture_0", ptr %"let$10d_capture_1", align 8
  %"let$10d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M29let_x2410d_x5Fclosure_x2411ae47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$10d_func_0", align 8
  ret ptr %2
}

define internal ptr @_M25raw_x5Flet_x2410d_x24110a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M15_x5F_x243_x24fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, ptr %_M10left_x24ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M10left_x24ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M10left_x24ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  ret ptr %7

switch_branch_Right_0:                            ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %_M10left_x24ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %_M15_x5F_x243_x24fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %_M15_x5F_x243_x24fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24f67_x5Fclosure_x2411af47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$111_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$111_0" = load ptr, ptr %"d$111_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$111_0")
  ret ptr %7
}

define internal ptr @_M29fun_x2411f_x5Fclosure_x2411b047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x2411f_x24115c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411b247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24115d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411b447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24115e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411b747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411b947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ba47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411bb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411bc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411bd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411be47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411bf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411c147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24116a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411c347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24117847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411c547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24117947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411c747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24117b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411c947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24117c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ca47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411cb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24118147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411cc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411cd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24118247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ce47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24118447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411d047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$167_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$167_0" = load ptr, ptr %"d$167_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$167_0")
  ret ptr %7
}

define internal ptr @_M29fun_x24177_x5Fclosure_x2411d147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M25raw_x5Ffun_x24177_x24118547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr %0, ptr %_M21_x24_x5F_x24f8_x2410e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$f67_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$111_0" = getelementptr { ptr }, ptr %"let$2c2$f67_capture_0", i32 0, i32 0
  store ptr %5, ptr %"d$111_0", align 8
  %"let$2c2$f67_capture_1" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr %"let$2c2$f67_capture_0", ptr %"let$2c2$f67_capture_1", align 8
  %"let$2c2$f67_func_0" = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @_M36let_x242c2_x24f67_x5Fclosure_x2411af47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f67_func_0", align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$11f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$11f_capture_1" = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  store ptr %"fun$11f_capture_0", ptr %"fun$11f_capture_1", align 8
  %"fun$11f_func_0" = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  store ptr @_M29fun_x2411f_x5Fclosure_x2411b047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$11f_func_0", align 8
  %10 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = call ptr %13(ptr %11, ptr %9)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 1, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr %15, ptr %20, align 8
  %21 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_14 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_7
  ]

switch_branch_Left_0:                             ; preds = %1
  %23 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %24 = getelementptr { ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %27 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 0
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = getelementptr { i8, <8 x i8> }, ptr %26, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_6 [
    i8 0, label %switch_branch_Left_1
    i8 1, label %switch_branch_Right_3
  ]

switch_branch_Left_1:                             ; preds = %switch_branch_Left_0
  %31 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %34, i32 0, i32 1, i32 0
  store ptr %33, ptr %36, align 8
  %37 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %38 = load i8, ptr %37, align 1
  switch i8 %38, label %switch_default_2 [
    i8 0, label %switch_branch_Left_2
    i8 1, label %switch_branch_Right_1
  ]

switch_branch_Left_2:                             ; preds = %switch_branch_Left_1
  %39 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %40 = getelementptr { ptr }, ptr %39, i32 0, i32 0
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %43 = getelementptr { i8, { ptr } }, ptr %42, i32 0, i32 0
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %42, i32 0, i32 1, i32 0
  store ptr %41, ptr %44, align 8
  %45 = getelementptr { i8, <8 x i8> }, ptr %42, i32 0, i32 0
  %46 = load i8, ptr %45, align 1
  switch i8 %46, label %switch_default_0 [
    i8 0, label %switch_branch_Left_3
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_3:                             ; preds = %switch_branch_Left_2
  %47 = getelementptr { i8, { ptr } }, ptr %42, i32 0, i32 1
  %48 = getelementptr { ptr }, ptr %47, i32 0, i32 0
  %49 = load ptr, ptr %48, align 8
  %50 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %51 = getelementptr { i8, { ptr } }, ptr %50, i32 0, i32 0
  store i8 0, ptr %51, align 1
  %52 = getelementptr { i8, { ptr } }, ptr %50, i32 0, i32 1, i32 0
  store ptr %49, ptr %52, align 8
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_0" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_0", i32 0, i32 0
  store ptr %50, ptr %"d$167_0", align 8
  %"let$2c2$fd3_capture_1" = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_0", ptr %"let$2c2$fd3_capture_1", align 8
  %"let$2c2$fd3_func_0" = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_0", align 8
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_1" = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 0
  store ptr %"fun$177_capture_0", ptr %"fun$177_capture_1", align 8
  %"fun$177_func_0" = getelementptr { ptr, ptr }, ptr %54, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411b247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_0", align 8
  %55 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 0
  %56 = load ptr, ptr %55, align 8
  %57 = getelementptr { ptr, ptr }, ptr %53, i32 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = call ptr %58(ptr %56, ptr %54)
  ret ptr %59

switch_branch_Right_0:                            ; preds = %switch_branch_Left_2
  %60 = getelementptr { i8, { ptr } }, ptr %42, i32 0, i32 1
  %61 = getelementptr { ptr }, ptr %60, i32 0, i32 0
  %62 = load ptr, ptr %61, align 8
  %63 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %62)
  %64 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %65 = getelementptr { i8, { i32 } }, ptr %64, i32 0, i32 0
  store i8 0, ptr %65, align 1
  %66 = getelementptr { i8, { i32 } }, ptr %64, i32 0, i32 1, i32 0
  store i32 1, ptr %66, align 4
  %67 = getelementptr { ptr, ptr }, ptr %63, i32 0, i32 0
  %68 = load ptr, ptr %67, align 8
  %69 = getelementptr { ptr, ptr }, ptr %63, i32 0, i32 1
  %70 = load ptr, ptr %69, align 8
  %71 = call ptr %70(ptr %68, ptr %64)
  %72 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %73 = getelementptr { i8, { ptr } }, ptr %72, i32 0, i32 0
  store i8 1, ptr %73, align 1
  %74 = getelementptr { i8, { ptr } }, ptr %72, i32 0, i32 1, i32 0
  store ptr %71, ptr %74, align 8
  %75 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_2" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_1" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_2", i32 0, i32 0
  store ptr %72, ptr %"d$167_1", align 8
  %"let$2c2$fd3_capture_3" = getelementptr { ptr, ptr }, ptr %75, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_2", ptr %"let$2c2$fd3_capture_3", align 8
  %"let$2c2$fd3_func_1" = getelementptr { ptr, ptr }, ptr %75, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_1", align 8
  %76 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_2" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_3" = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 0
  store ptr %"fun$177_capture_2", ptr %"fun$177_capture_3", align 8
  %"fun$177_func_1" = getelementptr { ptr, ptr }, ptr %76, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411b447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_1", align 8
  %77 = getelementptr { ptr, ptr }, ptr %75, i32 0, i32 0
  %78 = load ptr, ptr %77, align 8
  %79 = getelementptr { ptr, ptr }, ptr %75, i32 0, i32 1
  %80 = load ptr, ptr %79, align 8
  %81 = call ptr %80(ptr %78, ptr %76)
  ret ptr %81

switch_default_0:                                 ; preds = %switch_branch_Left_2
  unreachable

switch_branch_Right_1:                            ; preds = %switch_branch_Left_1
  %82 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %83 = getelementptr { ptr }, ptr %82, i32 0, i32 0
  %84 = load ptr, ptr %83, align 8
  %85 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %86 = getelementptr { i8, { ptr } }, ptr %85, i32 0, i32 0
  store i8 0, ptr %86, align 1
  %87 = getelementptr { i8, { ptr } }, ptr %85, i32 0, i32 1, i32 0
  store ptr @str4533, ptr %87, align 8
  %88 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %89 = getelementptr { i8, { ptr } }, ptr %88, i32 0, i32 0
  store i8 0, ptr %89, align 1
  %90 = getelementptr { i8, { ptr } }, ptr %88, i32 0, i32 1, i32 0
  store ptr %85, ptr %90, align 8
  %91 = getelementptr { i8, <8 x i8> }, ptr %88, i32 0, i32 0
  %92 = load i8, ptr %91, align 1
  switch i8 %92, label %switch_default_1 [
    i8 0, label %switch_branch_Left_4
    i8 1, label %switch_branch_Right_2
  ]

switch_branch_Left_4:                             ; preds = %switch_branch_Right_1
  %93 = getelementptr { i8, { ptr } }, ptr %88, i32 0, i32 1
  %94 = getelementptr { ptr }, ptr %93, i32 0, i32 0
  %95 = load ptr, ptr %94, align 8
  %96 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %97 = getelementptr { i8, { ptr } }, ptr %96, i32 0, i32 0
  store i8 0, ptr %97, align 1
  %98 = getelementptr { i8, { ptr } }, ptr %96, i32 0, i32 1, i32 0
  store ptr %95, ptr %98, align 8
  %99 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_4" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_2" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_4", i32 0, i32 0
  store ptr %96, ptr %"d$167_2", align 8
  %"let$2c2$fd3_capture_5" = getelementptr { ptr, ptr }, ptr %99, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_4", ptr %"let$2c2$fd3_capture_5", align 8
  %"let$2c2$fd3_func_2" = getelementptr { ptr, ptr }, ptr %99, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_2", align 8
  %100 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_4" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_5" = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 0
  store ptr %"fun$177_capture_4", ptr %"fun$177_capture_5", align 8
  %"fun$177_func_2" = getelementptr { ptr, ptr }, ptr %100, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411b747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_2", align 8
  %101 = getelementptr { ptr, ptr }, ptr %99, i32 0, i32 0
  %102 = load ptr, ptr %101, align 8
  %103 = getelementptr { ptr, ptr }, ptr %99, i32 0, i32 1
  %104 = load ptr, ptr %103, align 8
  %105 = call ptr %104(ptr %102, ptr %100)
  ret ptr %105

switch_branch_Right_2:                            ; preds = %switch_branch_Right_1
  %106 = getelementptr { i8, { ptr } }, ptr %88, i32 0, i32 1
  %107 = getelementptr { ptr }, ptr %106, i32 0, i32 0
  %108 = load ptr, ptr %107, align 8
  %109 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %108)
  %110 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %111 = getelementptr { i8, { i32 } }, ptr %110, i32 0, i32 0
  store i8 0, ptr %111, align 1
  %112 = getelementptr { i8, { i32 } }, ptr %110, i32 0, i32 1, i32 0
  store i32 1, ptr %112, align 4
  %113 = getelementptr { ptr, ptr }, ptr %109, i32 0, i32 0
  %114 = load ptr, ptr %113, align 8
  %115 = getelementptr { ptr, ptr }, ptr %109, i32 0, i32 1
  %116 = load ptr, ptr %115, align 8
  %117 = call ptr %116(ptr %114, ptr %110)
  %118 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %119 = getelementptr { i8, { ptr } }, ptr %118, i32 0, i32 0
  store i8 1, ptr %119, align 1
  %120 = getelementptr { i8, { ptr } }, ptr %118, i32 0, i32 1, i32 0
  store ptr %117, ptr %120, align 8
  %121 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_6" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_3" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_6", i32 0, i32 0
  store ptr %118, ptr %"d$167_3", align 8
  %"let$2c2$fd3_capture_7" = getelementptr { ptr, ptr }, ptr %121, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_6", ptr %"let$2c2$fd3_capture_7", align 8
  %"let$2c2$fd3_func_3" = getelementptr { ptr, ptr }, ptr %121, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411b847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_3", align 8
  %122 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_6" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_7" = getelementptr { ptr, ptr }, ptr %122, i32 0, i32 0
  store ptr %"fun$177_capture_6", ptr %"fun$177_capture_7", align 8
  %"fun$177_func_3" = getelementptr { ptr, ptr }, ptr %122, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411b947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_3", align 8
  %123 = getelementptr { ptr, ptr }, ptr %121, i32 0, i32 0
  %124 = load ptr, ptr %123, align 8
  %125 = getelementptr { ptr, ptr }, ptr %121, i32 0, i32 1
  %126 = load ptr, ptr %125, align 8
  %127 = call ptr %126(ptr %124, ptr %122)
  ret ptr %127

switch_default_1:                                 ; preds = %switch_branch_Right_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Left_1
  unreachable

switch_branch_Right_3:                            ; preds = %switch_branch_Left_0
  %128 = getelementptr { i8, { ptr } }, ptr %26, i32 0, i32 1
  %129 = getelementptr { ptr }, ptr %128, i32 0, i32 0
  %130 = load ptr, ptr %129, align 8
  %131 = call ptr @_M10fun_x2414747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %130)
  %132 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %133 = load i8, ptr %132, align 1
  switch i8 %133, label %switch_default_5 [
    i8 0, label %switch_branch_Left_5
    i8 1, label %switch_branch_Right_5
  ]

switch_branch_Left_5:                             ; preds = %switch_branch_Right_3
  %134 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %135 = getelementptr { ptr }, ptr %134, i32 0, i32 0
  %136 = load ptr, ptr %135, align 8
  %137 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %138 = getelementptr { i8, { ptr } }, ptr %137, i32 0, i32 0
  store i8 0, ptr %138, align 1
  %139 = getelementptr { i8, { ptr } }, ptr %137, i32 0, i32 1, i32 0
  store ptr %136, ptr %139, align 8
  %140 = getelementptr { i8, <8 x i8> }, ptr %137, i32 0, i32 0
  %141 = load i8, ptr %140, align 1
  switch i8 %141, label %switch_default_3 [
    i8 0, label %switch_branch_Left_6
    i8 1, label %switch_branch_Right_4
  ]

switch_branch_Left_6:                             ; preds = %switch_branch_Left_5
  %142 = getelementptr { i8, { ptr } }, ptr %137, i32 0, i32 1
  %143 = getelementptr { ptr }, ptr %142, i32 0, i32 0
  %144 = load ptr, ptr %143, align 8
  %145 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %146 = getelementptr { i8, { ptr } }, ptr %145, i32 0, i32 0
  store i8 0, ptr %146, align 1
  %147 = getelementptr { i8, { ptr } }, ptr %145, i32 0, i32 1, i32 0
  store ptr %144, ptr %147, align 8
  %148 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_8" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_4" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_8", i32 0, i32 0
  store ptr %145, ptr %"d$167_4", align 8
  %"let$2c2$fd3_capture_9" = getelementptr { ptr, ptr }, ptr %148, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_8", ptr %"let$2c2$fd3_capture_9", align 8
  %"let$2c2$fd3_func_4" = getelementptr { ptr, ptr }, ptr %148, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ba47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_4", align 8
  %149 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_8" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_9" = getelementptr { ptr, ptr }, ptr %149, i32 0, i32 0
  store ptr %"fun$177_capture_8", ptr %"fun$177_capture_9", align 8
  %"fun$177_func_4" = getelementptr { ptr, ptr }, ptr %149, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411bb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_4", align 8
  %150 = getelementptr { ptr, ptr }, ptr %148, i32 0, i32 0
  %151 = load ptr, ptr %150, align 8
  %152 = getelementptr { ptr, ptr }, ptr %148, i32 0, i32 1
  %153 = load ptr, ptr %152, align 8
  %154 = call ptr %153(ptr %151, ptr %149)
  ret ptr %154

switch_branch_Right_4:                            ; preds = %switch_branch_Left_5
  %155 = getelementptr { i8, { ptr } }, ptr %137, i32 0, i32 1
  %156 = getelementptr { ptr }, ptr %155, i32 0, i32 0
  %157 = load ptr, ptr %156, align 8
  %158 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %157)
  %159 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %160 = getelementptr { i8, { i32 } }, ptr %159, i32 0, i32 0
  store i8 0, ptr %160, align 1
  %161 = getelementptr { i8, { i32 } }, ptr %159, i32 0, i32 1, i32 0
  store i32 1, ptr %161, align 4
  %162 = getelementptr { ptr, ptr }, ptr %158, i32 0, i32 0
  %163 = load ptr, ptr %162, align 8
  %164 = getelementptr { ptr, ptr }, ptr %158, i32 0, i32 1
  %165 = load ptr, ptr %164, align 8
  %166 = call ptr %165(ptr %163, ptr %159)
  %167 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %168 = getelementptr { i8, { ptr } }, ptr %167, i32 0, i32 0
  store i8 1, ptr %168, align 1
  %169 = getelementptr { i8, { ptr } }, ptr %167, i32 0, i32 1, i32 0
  store ptr %166, ptr %169, align 8
  %170 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_10" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_5" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_10", i32 0, i32 0
  store ptr %167, ptr %"d$167_5", align 8
  %"let$2c2$fd3_capture_11" = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_10", ptr %"let$2c2$fd3_capture_11", align 8
  %"let$2c2$fd3_func_5" = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411bc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_5", align 8
  %171 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_10" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_11" = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 0
  store ptr %"fun$177_capture_10", ptr %"fun$177_capture_11", align 8
  %"fun$177_func_5" = getelementptr { ptr, ptr }, ptr %171, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411bd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_5", align 8
  %172 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 0
  %173 = load ptr, ptr %172, align 8
  %174 = getelementptr { ptr, ptr }, ptr %170, i32 0, i32 1
  %175 = load ptr, ptr %174, align 8
  %176 = call ptr %175(ptr %173, ptr %171)
  ret ptr %176

switch_default_3:                                 ; preds = %switch_branch_Left_5
  unreachable

switch_branch_Right_5:                            ; preds = %switch_branch_Right_3
  %177 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %178 = getelementptr { ptr }, ptr %177, i32 0, i32 0
  %179 = load ptr, ptr %178, align 8
  %180 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %181 = getelementptr { i8, { ptr } }, ptr %180, i32 0, i32 0
  store i8 0, ptr %181, align 1
  %182 = getelementptr { i8, { ptr } }, ptr %180, i32 0, i32 1, i32 0
  store ptr @str4533, ptr %182, align 8
  %183 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %184 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 0
  store i8 0, ptr %184, align 1
  %185 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 1, i32 0
  store ptr %180, ptr %185, align 8
  %186 = getelementptr { i8, <8 x i8> }, ptr %183, i32 0, i32 0
  %187 = load i8, ptr %186, align 1
  switch i8 %187, label %switch_default_4 [
    i8 0, label %switch_branch_Left_7
    i8 1, label %switch_branch_Right_6
  ]

switch_branch_Left_7:                             ; preds = %switch_branch_Right_5
  %188 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 1
  %189 = getelementptr { ptr }, ptr %188, i32 0, i32 0
  %190 = load ptr, ptr %189, align 8
  %191 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %192 = getelementptr { i8, { ptr } }, ptr %191, i32 0, i32 0
  store i8 0, ptr %192, align 1
  %193 = getelementptr { i8, { ptr } }, ptr %191, i32 0, i32 1, i32 0
  store ptr %190, ptr %193, align 8
  %194 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_12" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_6" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_12", i32 0, i32 0
  store ptr %191, ptr %"d$167_6", align 8
  %"let$2c2$fd3_capture_13" = getelementptr { ptr, ptr }, ptr %194, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_12", ptr %"let$2c2$fd3_capture_13", align 8
  %"let$2c2$fd3_func_6" = getelementptr { ptr, ptr }, ptr %194, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411be47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_6", align 8
  %195 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_12" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_13" = getelementptr { ptr, ptr }, ptr %195, i32 0, i32 0
  store ptr %"fun$177_capture_12", ptr %"fun$177_capture_13", align 8
  %"fun$177_func_6" = getelementptr { ptr, ptr }, ptr %195, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411bf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_6", align 8
  %196 = getelementptr { ptr, ptr }, ptr %194, i32 0, i32 0
  %197 = load ptr, ptr %196, align 8
  %198 = getelementptr { ptr, ptr }, ptr %194, i32 0, i32 1
  %199 = load ptr, ptr %198, align 8
  %200 = call ptr %199(ptr %197, ptr %195)
  ret ptr %200

switch_branch_Right_6:                            ; preds = %switch_branch_Right_5
  %201 = getelementptr { i8, { ptr } }, ptr %183, i32 0, i32 1
  %202 = getelementptr { ptr }, ptr %201, i32 0, i32 0
  %203 = load ptr, ptr %202, align 8
  %204 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %203)
  %205 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %206 = getelementptr { i8, { i32 } }, ptr %205, i32 0, i32 0
  store i8 0, ptr %206, align 1
  %207 = getelementptr { i8, { i32 } }, ptr %205, i32 0, i32 1, i32 0
  store i32 1, ptr %207, align 4
  %208 = getelementptr { ptr, ptr }, ptr %204, i32 0, i32 0
  %209 = load ptr, ptr %208, align 8
  %210 = getelementptr { ptr, ptr }, ptr %204, i32 0, i32 1
  %211 = load ptr, ptr %210, align 8
  %212 = call ptr %211(ptr %209, ptr %205)
  %213 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %214 = getelementptr { i8, { ptr } }, ptr %213, i32 0, i32 0
  store i8 1, ptr %214, align 1
  %215 = getelementptr { i8, { ptr } }, ptr %213, i32 0, i32 1, i32 0
  store ptr %212, ptr %215, align 8
  %216 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_14" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_7" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_14", i32 0, i32 0
  store ptr %213, ptr %"d$167_7", align 8
  %"let$2c2$fd3_capture_15" = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_14", ptr %"let$2c2$fd3_capture_15", align 8
  %"let$2c2$fd3_func_7" = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_7", align 8
  %217 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_14" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_15" = getelementptr { ptr, ptr }, ptr %217, i32 0, i32 0
  store ptr %"fun$177_capture_14", ptr %"fun$177_capture_15", align 8
  %"fun$177_func_7" = getelementptr { ptr, ptr }, ptr %217, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411c147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_7", align 8
  %218 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 0
  %219 = load ptr, ptr %218, align 8
  %220 = getelementptr { ptr, ptr }, ptr %216, i32 0, i32 1
  %221 = load ptr, ptr %220, align 8
  %222 = call ptr %221(ptr %219, ptr %217)
  ret ptr %222

switch_default_4:                                 ; preds = %switch_branch_Right_5
  unreachable

switch_default_5:                                 ; preds = %switch_branch_Right_3
  unreachable

switch_default_6:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_7:                            ; preds = %1
  %223 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %224 = getelementptr { ptr }, ptr %223, i32 0, i32 0
  %225 = load ptr, ptr %224, align 8
  %226 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %225)
  %227 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %228 = getelementptr { i8, { i32 } }, ptr %227, i32 0, i32 0
  store i8 0, ptr %228, align 1
  %229 = getelementptr { i8, { i32 } }, ptr %227, i32 0, i32 1, i32 0
  store i32 1, ptr %229, align 4
  %230 = getelementptr { ptr, ptr }, ptr %226, i32 0, i32 0
  %231 = load ptr, ptr %230, align 8
  %232 = getelementptr { ptr, ptr }, ptr %226, i32 0, i32 1
  %233 = load ptr, ptr %232, align 8
  %234 = call ptr %233(ptr %231, ptr %227)
  %235 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %236 = getelementptr { i8, { ptr } }, ptr %235, i32 0, i32 0
  store i8 1, ptr %236, align 1
  %237 = getelementptr { i8, { ptr } }, ptr %235, i32 0, i32 1, i32 0
  store ptr %234, ptr %237, align 8
  %238 = getelementptr { i8, <8 x i8> }, ptr %235, i32 0, i32 0
  %239 = load i8, ptr %238, align 1
  switch i8 %239, label %switch_default_13 [
    i8 0, label %switch_branch_Left_8
    i8 1, label %switch_branch_Right_11
  ]

switch_branch_Left_8:                             ; preds = %switch_branch_Right_7
  %240 = getelementptr { i8, { ptr } }, ptr %235, i32 0, i32 1
  %241 = getelementptr { ptr }, ptr %240, i32 0, i32 0
  %242 = load ptr, ptr %241, align 8
  %243 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %244 = getelementptr { i8, { ptr } }, ptr %243, i32 0, i32 0
  store i8 0, ptr %244, align 1
  %245 = getelementptr { i8, { ptr } }, ptr %243, i32 0, i32 1, i32 0
  store ptr %242, ptr %245, align 8
  %246 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %247 = load i8, ptr %246, align 1
  switch i8 %247, label %switch_default_9 [
    i8 0, label %switch_branch_Left_9
    i8 1, label %switch_branch_Right_9
  ]

switch_branch_Left_9:                             ; preds = %switch_branch_Left_8
  %248 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %249 = getelementptr { ptr }, ptr %248, i32 0, i32 0
  %250 = load ptr, ptr %249, align 8
  %251 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %252 = getelementptr { i8, { ptr } }, ptr %251, i32 0, i32 0
  store i8 0, ptr %252, align 1
  %253 = getelementptr { i8, { ptr } }, ptr %251, i32 0, i32 1, i32 0
  store ptr %250, ptr %253, align 8
  %254 = getelementptr { i8, <8 x i8> }, ptr %251, i32 0, i32 0
  %255 = load i8, ptr %254, align 1
  switch i8 %255, label %switch_default_7 [
    i8 0, label %switch_branch_Left_10
    i8 1, label %switch_branch_Right_8
  ]

switch_branch_Left_10:                            ; preds = %switch_branch_Left_9
  %256 = getelementptr { i8, { ptr } }, ptr %251, i32 0, i32 1
  %257 = getelementptr { ptr }, ptr %256, i32 0, i32 0
  %258 = load ptr, ptr %257, align 8
  %259 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %260 = getelementptr { i8, { ptr } }, ptr %259, i32 0, i32 0
  store i8 0, ptr %260, align 1
  %261 = getelementptr { i8, { ptr } }, ptr %259, i32 0, i32 1, i32 0
  store ptr %258, ptr %261, align 8
  %262 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_16" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_8" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_16", i32 0, i32 0
  store ptr %259, ptr %"d$167_8", align 8
  %"let$2c2$fd3_capture_17" = getelementptr { ptr, ptr }, ptr %262, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_16", ptr %"let$2c2$fd3_capture_17", align 8
  %"let$2c2$fd3_func_8" = getelementptr { ptr, ptr }, ptr %262, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_8", align 8
  %263 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_16" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_17" = getelementptr { ptr, ptr }, ptr %263, i32 0, i32 0
  store ptr %"fun$177_capture_16", ptr %"fun$177_capture_17", align 8
  %"fun$177_func_8" = getelementptr { ptr, ptr }, ptr %263, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411c347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_8", align 8
  %264 = getelementptr { ptr, ptr }, ptr %262, i32 0, i32 0
  %265 = load ptr, ptr %264, align 8
  %266 = getelementptr { ptr, ptr }, ptr %262, i32 0, i32 1
  %267 = load ptr, ptr %266, align 8
  %268 = call ptr %267(ptr %265, ptr %263)
  ret ptr %268

switch_branch_Right_8:                            ; preds = %switch_branch_Left_9
  %269 = getelementptr { i8, { ptr } }, ptr %251, i32 0, i32 1
  %270 = getelementptr { ptr }, ptr %269, i32 0, i32 0
  %271 = load ptr, ptr %270, align 8
  %272 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %271)
  %273 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %274 = getelementptr { i8, { i32 } }, ptr %273, i32 0, i32 0
  store i8 0, ptr %274, align 1
  %275 = getelementptr { i8, { i32 } }, ptr %273, i32 0, i32 1, i32 0
  store i32 1, ptr %275, align 4
  %276 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 0
  %277 = load ptr, ptr %276, align 8
  %278 = getelementptr { ptr, ptr }, ptr %272, i32 0, i32 1
  %279 = load ptr, ptr %278, align 8
  %280 = call ptr %279(ptr %277, ptr %273)
  %281 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %282 = getelementptr { i8, { ptr } }, ptr %281, i32 0, i32 0
  store i8 1, ptr %282, align 1
  %283 = getelementptr { i8, { ptr } }, ptr %281, i32 0, i32 1, i32 0
  store ptr %280, ptr %283, align 8
  %284 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_18" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_9" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_18", i32 0, i32 0
  store ptr %281, ptr %"d$167_9", align 8
  %"let$2c2$fd3_capture_19" = getelementptr { ptr, ptr }, ptr %284, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_18", ptr %"let$2c2$fd3_capture_19", align 8
  %"let$2c2$fd3_func_9" = getelementptr { ptr, ptr }, ptr %284, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_9", align 8
  %285 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_18" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_19" = getelementptr { ptr, ptr }, ptr %285, i32 0, i32 0
  store ptr %"fun$177_capture_18", ptr %"fun$177_capture_19", align 8
  %"fun$177_func_9" = getelementptr { ptr, ptr }, ptr %285, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411c547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_9", align 8
  %286 = getelementptr { ptr, ptr }, ptr %284, i32 0, i32 0
  %287 = load ptr, ptr %286, align 8
  %288 = getelementptr { ptr, ptr }, ptr %284, i32 0, i32 1
  %289 = load ptr, ptr %288, align 8
  %290 = call ptr %289(ptr %287, ptr %285)
  ret ptr %290

switch_default_7:                                 ; preds = %switch_branch_Left_9
  unreachable

switch_branch_Right_9:                            ; preds = %switch_branch_Left_8
  %291 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %292 = getelementptr { ptr }, ptr %291, i32 0, i32 0
  %293 = load ptr, ptr %292, align 8
  %294 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %295 = getelementptr { i8, { ptr } }, ptr %294, i32 0, i32 0
  store i8 0, ptr %295, align 1
  %296 = getelementptr { i8, { ptr } }, ptr %294, i32 0, i32 1, i32 0
  store ptr @str4533, ptr %296, align 8
  %297 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %298 = getelementptr { i8, { ptr } }, ptr %297, i32 0, i32 0
  store i8 0, ptr %298, align 1
  %299 = getelementptr { i8, { ptr } }, ptr %297, i32 0, i32 1, i32 0
  store ptr %294, ptr %299, align 8
  %300 = getelementptr { i8, <8 x i8> }, ptr %297, i32 0, i32 0
  %301 = load i8, ptr %300, align 1
  switch i8 %301, label %switch_default_8 [
    i8 0, label %switch_branch_Left_11
    i8 1, label %switch_branch_Right_10
  ]

switch_branch_Left_11:                            ; preds = %switch_branch_Right_9
  %302 = getelementptr { i8, { ptr } }, ptr %297, i32 0, i32 1
  %303 = getelementptr { ptr }, ptr %302, i32 0, i32 0
  %304 = load ptr, ptr %303, align 8
  %305 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %306 = getelementptr { i8, { ptr } }, ptr %305, i32 0, i32 0
  store i8 0, ptr %306, align 1
  %307 = getelementptr { i8, { ptr } }, ptr %305, i32 0, i32 1, i32 0
  store ptr %304, ptr %307, align 8
  %308 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_20" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_10" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_20", i32 0, i32 0
  store ptr %305, ptr %"d$167_10", align 8
  %"let$2c2$fd3_capture_21" = getelementptr { ptr, ptr }, ptr %308, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_20", ptr %"let$2c2$fd3_capture_21", align 8
  %"let$2c2$fd3_func_10" = getelementptr { ptr, ptr }, ptr %308, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_10", align 8
  %309 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_20" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_21" = getelementptr { ptr, ptr }, ptr %309, i32 0, i32 0
  store ptr %"fun$177_capture_20", ptr %"fun$177_capture_21", align 8
  %"fun$177_func_10" = getelementptr { ptr, ptr }, ptr %309, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411c747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_10", align 8
  %310 = getelementptr { ptr, ptr }, ptr %308, i32 0, i32 0
  %311 = load ptr, ptr %310, align 8
  %312 = getelementptr { ptr, ptr }, ptr %308, i32 0, i32 1
  %313 = load ptr, ptr %312, align 8
  %314 = call ptr %313(ptr %311, ptr %309)
  ret ptr %314

switch_branch_Right_10:                           ; preds = %switch_branch_Right_9
  %315 = getelementptr { i8, { ptr } }, ptr %297, i32 0, i32 1
  %316 = getelementptr { ptr }, ptr %315, i32 0, i32 0
  %317 = load ptr, ptr %316, align 8
  %318 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %317)
  %319 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %320 = getelementptr { i8, { i32 } }, ptr %319, i32 0, i32 0
  store i8 0, ptr %320, align 1
  %321 = getelementptr { i8, { i32 } }, ptr %319, i32 0, i32 1, i32 0
  store i32 1, ptr %321, align 4
  %322 = getelementptr { ptr, ptr }, ptr %318, i32 0, i32 0
  %323 = load ptr, ptr %322, align 8
  %324 = getelementptr { ptr, ptr }, ptr %318, i32 0, i32 1
  %325 = load ptr, ptr %324, align 8
  %326 = call ptr %325(ptr %323, ptr %319)
  %327 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %328 = getelementptr { i8, { ptr } }, ptr %327, i32 0, i32 0
  store i8 1, ptr %328, align 1
  %329 = getelementptr { i8, { ptr } }, ptr %327, i32 0, i32 1, i32 0
  store ptr %326, ptr %329, align 8
  %330 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_22" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_11" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_22", i32 0, i32 0
  store ptr %327, ptr %"d$167_11", align 8
  %"let$2c2$fd3_capture_23" = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_22", ptr %"let$2c2$fd3_capture_23", align 8
  %"let$2c2$fd3_func_11" = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411c847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_11", align 8
  %331 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_22" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_23" = getelementptr { ptr, ptr }, ptr %331, i32 0, i32 0
  store ptr %"fun$177_capture_22", ptr %"fun$177_capture_23", align 8
  %"fun$177_func_11" = getelementptr { ptr, ptr }, ptr %331, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411c947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_11", align 8
  %332 = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 0
  %333 = load ptr, ptr %332, align 8
  %334 = getelementptr { ptr, ptr }, ptr %330, i32 0, i32 1
  %335 = load ptr, ptr %334, align 8
  %336 = call ptr %335(ptr %333, ptr %331)
  ret ptr %336

switch_default_8:                                 ; preds = %switch_branch_Right_9
  unreachable

switch_default_9:                                 ; preds = %switch_branch_Left_8
  unreachable

switch_branch_Right_11:                           ; preds = %switch_branch_Right_7
  %337 = getelementptr { i8, { ptr } }, ptr %235, i32 0, i32 1
  %338 = getelementptr { ptr }, ptr %337, i32 0, i32 0
  %339 = load ptr, ptr %338, align 8
  %340 = call ptr @_M10fun_x2414747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %339)
  %341 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %342 = load i8, ptr %341, align 1
  switch i8 %342, label %switch_default_12 [
    i8 0, label %switch_branch_Left_12
    i8 1, label %switch_branch_Right_13
  ]

switch_branch_Left_12:                            ; preds = %switch_branch_Right_11
  %343 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %344 = getelementptr { ptr }, ptr %343, i32 0, i32 0
  %345 = load ptr, ptr %344, align 8
  %346 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %347 = getelementptr { i8, { ptr } }, ptr %346, i32 0, i32 0
  store i8 0, ptr %347, align 1
  %348 = getelementptr { i8, { ptr } }, ptr %346, i32 0, i32 1, i32 0
  store ptr %345, ptr %348, align 8
  %349 = getelementptr { i8, <8 x i8> }, ptr %346, i32 0, i32 0
  %350 = load i8, ptr %349, align 1
  switch i8 %350, label %switch_default_10 [
    i8 0, label %switch_branch_Left_13
    i8 1, label %switch_branch_Right_12
  ]

switch_branch_Left_13:                            ; preds = %switch_branch_Left_12
  %351 = getelementptr { i8, { ptr } }, ptr %346, i32 0, i32 1
  %352 = getelementptr { ptr }, ptr %351, i32 0, i32 0
  %353 = load ptr, ptr %352, align 8
  %354 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %355 = getelementptr { i8, { ptr } }, ptr %354, i32 0, i32 0
  store i8 0, ptr %355, align 1
  %356 = getelementptr { i8, { ptr } }, ptr %354, i32 0, i32 1, i32 0
  store ptr %353, ptr %356, align 8
  %357 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_24" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_12" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_24", i32 0, i32 0
  store ptr %354, ptr %"d$167_12", align 8
  %"let$2c2$fd3_capture_25" = getelementptr { ptr, ptr }, ptr %357, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_24", ptr %"let$2c2$fd3_capture_25", align 8
  %"let$2c2$fd3_func_12" = getelementptr { ptr, ptr }, ptr %357, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ca47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_12", align 8
  %358 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_24" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_25" = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 0
  store ptr %"fun$177_capture_24", ptr %"fun$177_capture_25", align 8
  %"fun$177_func_12" = getelementptr { ptr, ptr }, ptr %358, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411cb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_12", align 8
  %359 = getelementptr { ptr, ptr }, ptr %357, i32 0, i32 0
  %360 = load ptr, ptr %359, align 8
  %361 = getelementptr { ptr, ptr }, ptr %357, i32 0, i32 1
  %362 = load ptr, ptr %361, align 8
  %363 = call ptr %362(ptr %360, ptr %358)
  ret ptr %363

switch_branch_Right_12:                           ; preds = %switch_branch_Left_12
  %364 = getelementptr { i8, { ptr } }, ptr %346, i32 0, i32 1
  %365 = getelementptr { ptr }, ptr %364, i32 0, i32 0
  %366 = load ptr, ptr %365, align 8
  %367 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %366)
  %368 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %369 = getelementptr { i8, { i32 } }, ptr %368, i32 0, i32 0
  store i8 0, ptr %369, align 1
  %370 = getelementptr { i8, { i32 } }, ptr %368, i32 0, i32 1, i32 0
  store i32 1, ptr %370, align 4
  %371 = getelementptr { ptr, ptr }, ptr %367, i32 0, i32 0
  %372 = load ptr, ptr %371, align 8
  %373 = getelementptr { ptr, ptr }, ptr %367, i32 0, i32 1
  %374 = load ptr, ptr %373, align 8
  %375 = call ptr %374(ptr %372, ptr %368)
  %376 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %377 = getelementptr { i8, { ptr } }, ptr %376, i32 0, i32 0
  store i8 1, ptr %377, align 1
  %378 = getelementptr { i8, { ptr } }, ptr %376, i32 0, i32 1, i32 0
  store ptr %375, ptr %378, align 8
  %379 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_26" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_13" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_26", i32 0, i32 0
  store ptr %376, ptr %"d$167_13", align 8
  %"let$2c2$fd3_capture_27" = getelementptr { ptr, ptr }, ptr %379, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_26", ptr %"let$2c2$fd3_capture_27", align 8
  %"let$2c2$fd3_func_13" = getelementptr { ptr, ptr }, ptr %379, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411cc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_13", align 8
  %380 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_26" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_27" = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 0
  store ptr %"fun$177_capture_26", ptr %"fun$177_capture_27", align 8
  %"fun$177_func_13" = getelementptr { ptr, ptr }, ptr %380, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411cd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_13", align 8
  %381 = getelementptr { ptr, ptr }, ptr %379, i32 0, i32 0
  %382 = load ptr, ptr %381, align 8
  %383 = getelementptr { ptr, ptr }, ptr %379, i32 0, i32 1
  %384 = load ptr, ptr %383, align 8
  %385 = call ptr %384(ptr %382, ptr %380)
  ret ptr %385

switch_default_10:                                ; preds = %switch_branch_Left_12
  unreachable

switch_branch_Right_13:                           ; preds = %switch_branch_Right_11
  %386 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %387 = getelementptr { ptr }, ptr %386, i32 0, i32 0
  %388 = load ptr, ptr %387, align 8
  %389 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %390 = getelementptr { i8, { ptr } }, ptr %389, i32 0, i32 0
  store i8 0, ptr %390, align 1
  %391 = getelementptr { i8, { ptr } }, ptr %389, i32 0, i32 1, i32 0
  store ptr @str4533, ptr %391, align 8
  %392 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %393 = getelementptr { i8, { ptr } }, ptr %392, i32 0, i32 0
  store i8 0, ptr %393, align 1
  %394 = getelementptr { i8, { ptr } }, ptr %392, i32 0, i32 1, i32 0
  store ptr %389, ptr %394, align 8
  %395 = getelementptr { i8, <8 x i8> }, ptr %392, i32 0, i32 0
  %396 = load i8, ptr %395, align 1
  switch i8 %396, label %switch_default_11 [
    i8 0, label %switch_branch_Left_14
    i8 1, label %switch_branch_Right_14
  ]

switch_branch_Left_14:                            ; preds = %switch_branch_Right_13
  %397 = getelementptr { i8, { ptr } }, ptr %392, i32 0, i32 1
  %398 = getelementptr { ptr }, ptr %397, i32 0, i32 0
  %399 = load ptr, ptr %398, align 8
  %400 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %401 = getelementptr { i8, { ptr } }, ptr %400, i32 0, i32 0
  store i8 0, ptr %401, align 1
  %402 = getelementptr { i8, { ptr } }, ptr %400, i32 0, i32 1, i32 0
  store ptr %399, ptr %402, align 8
  %403 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_28" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_14" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_28", i32 0, i32 0
  store ptr %400, ptr %"d$167_14", align 8
  %"let$2c2$fd3_capture_29" = getelementptr { ptr, ptr }, ptr %403, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_28", ptr %"let$2c2$fd3_capture_29", align 8
  %"let$2c2$fd3_func_14" = getelementptr { ptr, ptr }, ptr %403, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411ce47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_14", align 8
  %404 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_28" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_29" = getelementptr { ptr, ptr }, ptr %404, i32 0, i32 0
  store ptr %"fun$177_capture_28", ptr %"fun$177_capture_29", align 8
  %"fun$177_func_14" = getelementptr { ptr, ptr }, ptr %404, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411cf47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_14", align 8
  %405 = getelementptr { ptr, ptr }, ptr %403, i32 0, i32 0
  %406 = load ptr, ptr %405, align 8
  %407 = getelementptr { ptr, ptr }, ptr %403, i32 0, i32 1
  %408 = load ptr, ptr %407, align 8
  %409 = call ptr %408(ptr %406, ptr %404)
  ret ptr %409

switch_branch_Right_14:                           ; preds = %switch_branch_Right_13
  %410 = getelementptr { i8, { ptr } }, ptr %392, i32 0, i32 1
  %411 = getelementptr { ptr }, ptr %410, i32 0, i32 0
  %412 = load ptr, ptr %411, align 8
  %413 = call ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %412)
  %414 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %415 = getelementptr { i8, { i32 } }, ptr %414, i32 0, i32 0
  store i8 0, ptr %415, align 1
  %416 = getelementptr { i8, { i32 } }, ptr %414, i32 0, i32 1, i32 0
  store i32 1, ptr %416, align 4
  %417 = getelementptr { ptr, ptr }, ptr %413, i32 0, i32 0
  %418 = load ptr, ptr %417, align 8
  %419 = getelementptr { ptr, ptr }, ptr %413, i32 0, i32 1
  %420 = load ptr, ptr %419, align 8
  %421 = call ptr %420(ptr %418, ptr %414)
  %422 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %423 = getelementptr { i8, { ptr } }, ptr %422, i32 0, i32 0
  store i8 1, ptr %423, align 1
  %424 = getelementptr { i8, { ptr } }, ptr %422, i32 0, i32 1, i32 0
  store ptr %421, ptr %424, align 8
  %425 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fd3_capture_30" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$167_15" = getelementptr { ptr }, ptr %"let$2c2$fd3_capture_30", i32 0, i32 0
  store ptr %422, ptr %"d$167_15", align 8
  %"let$2c2$fd3_capture_31" = getelementptr { ptr, ptr }, ptr %425, i32 0, i32 0
  store ptr %"let$2c2$fd3_capture_30", ptr %"let$2c2$fd3_capture_31", align 8
  %"let$2c2$fd3_func_15" = getelementptr { ptr, ptr }, ptr %425, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fd3_x5Fclosure_x2411d047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fd3_func_15", align 8
  %426 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$177_capture_30" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$177_capture_31" = getelementptr { ptr, ptr }, ptr %426, i32 0, i32 0
  store ptr %"fun$177_capture_30", ptr %"fun$177_capture_31", align 8
  %"fun$177_func_15" = getelementptr { ptr, ptr }, ptr %426, i32 0, i32 1
  store ptr @_M29fun_x24177_x5Fclosure_x2411d147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"fun$177_func_15", align 8
  %427 = getelementptr { ptr, ptr }, ptr %425, i32 0, i32 0
  %428 = load ptr, ptr %427, align 8
  %429 = getelementptr { ptr, ptr }, ptr %425, i32 0, i32 1
  %430 = load ptr, ptr %429, align 8
  %431 = call ptr %430(ptr %428, ptr %426)
  ret ptr %431

switch_default_11:                                ; preds = %switch_branch_Right_13
  unreachable

switch_default_12:                                ; preds = %switch_branch_Right_11
  unreachable

switch_default_13:                                ; preds = %switch_branch_Right_7
  unreachable

switch_default_14:                                ; preds = %1
  unreachable
}

define internal ptr @_M30let_x2410c5_x5Fclosure_x2411d247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"int32#$10ae_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#$10ae_0" = load ptr, ptr %"int32#$10ae_addr_0", align 8
  %3 = call ptr @_M26raw_x5Flet_x2410c5_x24110947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr null, ptr %"int32#$10ae_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8addInt3234runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M17int32_x23_x2410ae34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$10c5_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#$10ae_0" = getelementptr { ptr }, ptr %"let$10c5_capture_0", i32 0, i32 0
  store ptr %_M17int32_x23_x2410ae34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"int32#$10ae_0", align 8
  %"let$10c5_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$10c5_capture_0", ptr %"let$10c5_capture_1", align 8
  %"let$10c5_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M30let_x2410c5_x5Fclosure_x2411d247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$10c5_func_0", align 8
  ret ptr %2
}

define internal ptr @_M6putStr34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M16str_x245a_x242d434runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16str_x245a_x242d434runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M16str_x245a_x242d434runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr %0, ptr %_M16str_x245b_x242cd34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M16str_x245b_x242cd34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M16str_x245b_x242cd34runtime_x2Fmalgo_x2FPrelude_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  %12 = call ptr @malgo_newline(ptr %10)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M25raw_x5Ffun_x2411f_x24115c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M12right_x2411547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M12right_x2411547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_3 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M12right_x2411547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1, i32 0
  store ptr @str4563, ptr %9, align 8
  %10 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %12 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %13 = getelementptr { ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr @malgo_print_string(ptr %14)
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = call ptr @malgo_newline(ptr %18)
  ret ptr %20

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %21 = getelementptr { i8, { ptr } }, ptr %_M12right_x2411547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %22 = getelementptr { ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { i8, <4 x i8> }, ptr %23, i32 0, i32 0
  %25 = load i8, ptr %24, align 1
  switch i8 %25, label %switch_default_2 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %26 = getelementptr { i8, { i32 } }, ptr %23, i32 0, i32 1
  %27 = getelementptr { i32 }, ptr %26, i32 0, i32 0
  %28 = load i32, ptr %27, align 4
  %29 = call ptr @malgo_int32_t_to_string(i32 %28)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 1, i32 0
  store ptr %29, ptr %32, align 8
  %33 = getelementptr { i8, <8 x i8> }, ptr %30, i32 0, i32 0
  %34 = load i8, ptr %33, align 1
  switch i8 %34, label %switch_default_1 [
    i8 0, label %"switch_branch_String#_1"
  ]

"switch_branch_String#_1":                        ; preds = %"switch_branch_Int32#_0"
  %35 = getelementptr { i8, { ptr } }, ptr %30, i32 0, i32 1
  %36 = getelementptr { ptr }, ptr %35, i32 0, i32 0
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr @malgo_print_string(ptr %37)
  ret ptr %38

switch_default_1:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24115d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24115e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24115f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411d947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411da47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411da47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411db47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411db47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411dc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411dc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411dd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411dd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411de47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411de47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411df47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411df47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24116f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411e947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ea47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ea47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411eb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411eb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ec47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ec47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ed47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ed47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ee47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ee47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ef47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ef47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24117f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411f947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fa47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fa47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fc47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411fe47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x2411ff47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24118f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24120f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x24119f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121a47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121c47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121d47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121e47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a847test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24121f47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411a947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122047test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411aa47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122147test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411ab47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122247test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411ac47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122347test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$1060_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$1060_0" = load ptr, ptr %"d$88a$1060_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$1060_0")
  ret ptr %7
}

define internal ptr @_M25raw_x5Ffun_x24177_x2411ad47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Left_0
    i8 1, label %switch_branch_Right_0
  ]

switch_branch_Left_0:                             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %switch_branch_Left_0
  %9 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
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

switch_default_0:                                 ; preds = %switch_branch_Left_0
  unreachable

switch_branch_Right_0:                            ; preds = %1
  %18 = getelementptr { i8, { ptr } }, ptr %_M11left_x2416b47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %19 = getelementptr { ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, <4 x i8> }, ptr %20, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %switch_branch_Right_0
  %23 = getelementptr { i8, { i32 } }, ptr %20, i32 0, i32 1
  %24 = getelementptr { i32 }, ptr %23, i32 0, i32 0
  %25 = load i32, ptr %24, align 4
  %26 = call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr %26, ptr %29, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$fe4_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$1060_0" = getelementptr { ptr }, ptr %"let$2c2$fe4_capture_0", i32 0, i32 0
  store ptr %27, ptr %"d$88a$1060_0", align 8
  %"let$2c2$fe4_capture_1" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr %"let$2c2$fe4_capture_0", ptr %"let$2c2$fe4_capture_1", align 8
  %"let$2c2$fe4_func_0" = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @_M36let_x242c2_x24fe4_x5Fclosure_x24122447test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$fe4_func_0", align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %35, i32 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0, align 8
  %36 = call ptr %34(ptr %32, ptr %35)
  ret ptr %36

switch_default_1:                                 ; preds = %switch_branch_Right_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4Left47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr %0, ptr %_M7p_x24f947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x24f947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M5Right47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr %0, ptr %_M7p_x24fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x24fb47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M36let_x242c2_x24f99_x5Fclosure_x24122547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$88a$103c_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$88a$103c_0" = load ptr, ptr %"d$88a$103c_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$88a$103c_0")
  ret ptr %7
}

define internal ptr @_M36let_x242c2_x24f9f_x5Fclosure_x24122647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$142_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$142_0" = load ptr, ptr %"cast$142_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"cast$142_0")
  ret ptr %7
}

define internal ptr @_M10fun_x2414747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M13y_x24b_x2413947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M13y_x24b_x2413947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M13y_x24b_x2413947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$f99_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$88a$103c_0" = getelementptr { ptr }, ptr %"let$2c2$f99_capture_0", i32 0, i32 0
  store ptr %8, ptr %"d$88a$103c_0", align 8
  %"let$2c2$f99_capture_1" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %"let$2c2$f99_capture_0", ptr %"let$2c2$f99_capture_1", align 8
  %"let$2c2$f99_func_0" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @_M36let_x242c2_x24f99_x5Fclosure_x24122547test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f99_func_0", align 8
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStr_capture_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr null, ptr %putStr_capture_0, align 8
  %putStr_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M6putStr34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStr_func_0, align 8
  %17 = call ptr %15(ptr %13, ptr %16)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$2c2$f9f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$142_0" = getelementptr { ptr }, ptr %"let$2c2$f9f_capture_0", i32 0, i32 0
  store ptr %17, ptr %"cast$142_0", align 8
  %"let$2c2$f9f_capture_1" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %"let$2c2$f9f_capture_0", ptr %"let$2c2$f9f_capture_1", align 8
  %"let$2c2$f9f_func_0" = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @_M36let_x242c2_x24f9f_x5Fclosure_x24122647test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$2c2$f9f_func_0", align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Right_capture_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr null, ptr %Right_capture_0, align 8
  %Right_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @_M5Right47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External, ptr %Right_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %19)
  ret ptr %24

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M2id47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr %0, ptr %_M12x_x242_x24fd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0) {
  ret ptr %_M12x_x242_x24fd47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal_0
}

define internal i32 @_M36let_x24715_x24dcf_x5Fclosure_x24122747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal(ptr %0, i32 %1) {
  %"p$10b0$10bd$db2_addr_0" = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %"p$10b0$10bd$db2_0" = load i32, ptr %"p$10b0$10bd$db2_addr_0", align 4
  %3 = call i32 @malgo_add_int32_t(i32 %"p$10b0$10bd$db2_0", i32 %1)
  ret i32 %3
}

define internal ptr @_M26raw_x5Flet_x2410c5_x24110947test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Temporal(ptr %0, ptr %_M17int32_x23_x2410ae34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M17int32_x23_x2410af34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <4 x i8> }, ptr %_M17int32_x23_x2410ae34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %_M17int32_x23_x2410ae34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %_M17int32_x23_x2410af34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_1"
  ]

"switch_branch_Int32#_1":                         ; preds = %"switch_branch_Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %_M17int32_x23_x2410af34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$715$dcf_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %"p$10b0$10bd$db2_0" = getelementptr { i32 }, ptr %"let$715$dcf_capture_0", i32 0, i32 0
  store i32 %6, ptr %"p$10b0$10bd$db2_0", align 4
  %"let$715$dcf_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$715$dcf_capture_0", ptr %"let$715$dcf_capture_1", align 8
  %"let$715$dcf_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M36let_x24715_x24dcf_x5Fclosure_x24122747test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8Internal, ptr %"let$715$dcf_func_0", align 8
  %13 = call i32 @malgo_add_int32_t(i32 %6, i32 %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 %13, ptr %16, align 4
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestEither.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FTestEither_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestEither.mlg"() {
  ret void
}
