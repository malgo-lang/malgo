; ModuleID = 'test/testcases/malgo/Show.mlg'
source_filename = "test/testcases/malgo/Show.mlg"

@_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External = global ptr undef
@str3967 = unnamed_addr constant [5 x i8] c"show\00"
@str3969 = unnamed_addr constant [2 x i8] c"(\00"
@str3970 = unnamed_addr constant [3 x i8] c", \00"
@str3971 = unnamed_addr constant [2 x i8] c")\00"

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

define internal ptr @_M34let_x2488_x24f5f_x5Fclosure_x24f7c41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"cast$e7_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"cast$e7_0" = load ptr, ptr %"cast$e7_addr_0", align 8
  %3 = call ptr @_M30raw_x5Flet_x2488_x24f5f_x24f7b41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"cast$e7_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2470_x24e141test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %3 = call ptr @_M10showTuple241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr null, ptr %2)
  %4 = load ptr, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %4)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$88$f5f_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"cast$e7_0" = getelementptr { ptr }, ptr %"let$88$f5f_capture_0", i32 0, i32 0
  store ptr %9, ptr %"cast$e7_0", align 8
  %"let$88$f5f_capture_1" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr %"let$88$f5f_capture_0", ptr %"let$88$f5f_capture_1", align 8
  %"let$88$f5f_func_0" = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @_M34let_x2488_x24f5f_x5Fclosure_x24f7c41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$88$f5f_func_0", align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %11, i32 0, i32 1, i32 0
  store i32 1, ptr %13, align 4
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %14, i32 0, i32 1, i32 0
  store i32 2, ptr %16, align 4
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 0
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 0
  store ptr %11, ptr %19, align 8
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %17, i32 0, i32 1, i32 1
  store ptr %14, ptr %20, align 8
  %21 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %24 = load ptr, ptr %23, align 8
  %25 = call ptr %24(ptr %22, ptr %17)
  ret ptr %25
}

define internal ptr @_M27let_x24e0_x5Fclosure_x24f7d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"showDictA$8$95_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"showDictA$8$95_0" = load ptr, ptr %"showDictA$8$95_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x24e0_x24f7a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"showDictA$8$95_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M10showTuple241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$e0_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"showDictA$8$95_0" = getelementptr { ptr }, ptr %"let$e0_capture_0", i32 0, i32 0
  store ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %"showDictA$8$95_0", align 8
  %"let$e0_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$e0_capture_0", ptr %"let$e0_capture_1", align 8
  %"let$e0_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x24e0_x5Fclosure_x24f7d41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$e0_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x2488_x5Fclosure_x24f7e41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"showDict$d$79_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"showDict$d$79_0" = load ptr, ptr %"showDict$d$79_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2488_x24f7641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"showDict$d$79_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M5print41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M19showDict_x24d_x247941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$88_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"showDict$d$79_0" = getelementptr { ptr }, ptr %"let$88_capture_0", i32 0, i32 0
  store ptr %_M19showDict_x24d_x247941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %"showDict$d$79_0", align 8
  %"let$88_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$88_capture_0", ptr %"let$88_capture_1", align 8
  %"let$88_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2488_x5Fclosure_x24f7e41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$88_func_0", align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr)

define internal ptr @_M4show41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M12record_x247741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M12record_x247741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr @str3967)
  ret ptr %2
}

define internal ptr @_M40fun_x24b7_x24bd_x24f20_x5Fclosure_x24f8041test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"showDictA$8$95_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"showDictA$8$95_0" = load ptr, ptr %"showDictA$8$95_addr_0", align 8
  %"showDictB$9$96_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"showDictB$9$96_0" = load ptr, ptr %"showDictB$9$96_addr_0", align 8
  %3 = call ptr @_M36raw_x5Ffun_x24b7_x24bd_x24f20_x24f7941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"showDictA$8$95_0", ptr %"showDictB$9$96_0", ptr %1)
  ret ptr %3
}

declare ptr @malgo_hash_table_new()

declare void @malgo_hash_table_insert(ptr, ptr, ptr)

define internal ptr @_M23raw_x5Flet_x24e0_x24f7a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M20showDictB_x249_x249641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$b7$bd$f20_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"showDictA$8$95_0" = getelementptr { ptr, ptr }, ptr %"fun$b7$bd$f20_capture_0", i32 0, i32 0
  store ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %"showDictA$8$95_0", align 8
  %"showDictB$9$96_0" = getelementptr { ptr, ptr }, ptr %"fun$b7$bd$f20_capture_0", i32 0, i32 1
  store ptr %_M20showDictB_x249_x249641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %"showDictB$9$96_0", align 8
  %"fun$b7$bd$f20_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"fun$b7$bd$f20_capture_0", ptr %"fun$b7$bd$f20_capture_1", align 8
  %"fun$b7$bd$f20_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M40fun_x24b7_x24bd_x24f20_x5Fclosure_x24f8041test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"fun$b7$bd$f20_func_0", align 8
  %3 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %3, ptr @str3967, ptr %2)
  ret ptr %3
}

define internal ptr @_M36raw_x5Ffun_x24b7_x24bd_x24f20_x24f7941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M20showDictB_x249_x249641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M24tuple_x2497_x24be_x24f2141test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M24tuple_x2497_x24be_x24f2141test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Tuple#_0"
  ]

"switch_branch_Tuple#_0":                         ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %_M24tuple_x2497_x24be_x24f2141test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { ptr } }, ptr %9, i32 0, i32 1, i32 0
  store ptr @str3969, ptr %11, align 8
  %12 = call ptr @malgo_hash_table_get(ptr %_M20showDictA_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr @str3967)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %6)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1, i32 0
  store ptr @str3970, ptr %20, align 8
  %21 = call ptr @malgo_hash_table_get(ptr %_M20showDictB_x249_x249641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr @str3967)
  %22 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %8)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %28 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 0
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { ptr } }, ptr %27, i32 0, i32 1, i32 0
  store ptr @str3971, ptr %29, align 8
  %30 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %26)
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr %34(ptr %32, ptr %27)
  %36 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %18)
  %37 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  %38 = load ptr, ptr %37, align 8
  %39 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  %40 = load ptr, ptr %39, align 8
  %41 = call ptr %40(ptr %38, ptr %35)
  %42 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %17)
  %43 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 0
  %44 = load ptr, ptr %43, align 8
  %45 = getelementptr { ptr, ptr }, ptr %42, i32 0, i32 1
  %46 = load ptr, ptr %45, align 8
  %47 = call ptr %46(ptr %44, ptr %41)
  %48 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %9)
  %49 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 0
  %50 = load ptr, ptr %49, align 8
  %51 = getelementptr { ptr, ptr }, ptr %48, i32 0, i32 1
  %52 = load ptr, ptr %51, align 8
  %53 = call ptr %52(ptr %50, ptr %47)
  ret ptr %53

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M30raw_x5Flet_x2488_x24f5f_x24f7b41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M10cast_x24e741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M19x_x24e_x247a_x24f6041test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M10cast_x24e741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr @str3967)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M19x_x24e_x247a_x24f6041test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0)
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M23raw_x5Flet_x2488_x24f7641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M19showDict_x24d_x247941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M12x_x24e_x247a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_hash_table_get(ptr %_M19showDict_x24d_x247941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr @str3967)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12x_x24e_x247a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0)
  %8 = getelementptr { i8, <8 x i8> }, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %10 = getelementptr { i8, { ptr } }, ptr %7, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M27let_x2494_x5Fclosure_x24f8441test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$0$89_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$0$89_0" = load ptr, ptr %"x$0$89_addr_0", align 8
  %3 = call ptr @_M23raw_x5Flet_x2494_x24f7741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"x$0$89_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M8_x3C_x3E41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr %0, ptr %_M12x_x240_x248941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$94_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$0$89_0" = getelementptr { ptr }, ptr %"let$94_capture_0", i32 0, i32 0
  store ptr %_M12x_x240_x248941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %"x$0$89_0", align 8
  %"let$94_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$94_capture_0", ptr %"let$94_capture_1", align 8
  %"let$94_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2494_x5Fclosure_x24f8441test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$94_func_0", align 8
  ret ptr %2
}

define internal ptr @_M28let_x24f9a_x5Fclosure_x24f8541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"string#$f87_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#$f87_0" = load ptr, ptr %"string#$f87_addr_0", align 8
  %3 = call ptr @_M24raw_x5Flet_x24f9a_x24f7541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr null, ptr %"string#$f87_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr %0, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$f9a_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#$f87_0" = getelementptr { ptr }, ptr %"let$f9a_capture_0", i32 0, i32 0
  store ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %"string#$f87_0", align 8
  %"let$f9a_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$f9a_capture_0", ptr %"let$f9a_capture_1", align 8
  %"let$f9a_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M28let_x24f9a_x5Fclosure_x24f8541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$f9a_func_0", align 8
  ret ptr %2
}

define internal ptr @_M23raw_x5Flet_x2494_x24f7741test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M12x_x240_x248941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0, ptr %_M12y_x241_x248a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0) {
  %2 = call ptr @_M12appendString34runtime_x2Fmalgo_x2FBuiltin_x2Emlg8External(ptr null, ptr %_M12x_x240_x248941test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %_M12y_x241_x248a41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal_0)
  ret ptr %7
}

define internal ptr @_M35let_x24861_x24cdd_x5Fclosure_x24f8641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %"p$f89$f93$cc0_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"p$f89$f93$cc0_0" = load ptr, ptr %"p$f89$f93$cc0_addr_0", align 8
  %3 = call ptr @malgo_string_append(ptr %"p$f89$f93$cc0_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M24raw_x5Flet_x24f9a_x24f7541test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Temporal(ptr %0, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M17string_x23_x24f8734runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_1"
  ]

"switch_branch_String#_1":                        ; preds = %"switch_branch_String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %_M17string_x23_x24f8834runtime_x2Fmalgo_x2FBuiltin_x2Emlg8Temporal_0, i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$861$cdd_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"p$f89$f93$cc0_0" = getelementptr { ptr }, ptr %"let$861$cdd_capture_0", i32 0, i32 0
  store ptr %6, ptr %"p$f89$f93$cc0_0", align 8
  %"let$861$cdd_capture_1" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %"let$861$cdd_capture_0", ptr %"let$861$cdd_capture_1", align 8
  %"let$861$cdd_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @_M35let_x24861_x24cdd_x5Fclosure_x24f8641test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"let$861$cdd_func_0", align 8
  %13 = call ptr @malgo_string_append(ptr %6, ptr %11)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %15 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 0
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %14, i32 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  ret ptr %14

switch_default_0:                                 ; preds = %"switch_branch_String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Show.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @_M27fun_x2473_x5Fclosure_x24f8b41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <4 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_Int32#_0"
  ]

"switch_branch_Int32#_0":                         ; preds = %2
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
  %"fun$73_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$73_capture_1" = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr %"fun$73_capture_0", ptr %"fun$73_capture_1", align 8
  %"fun$73_func_0" = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr @_M27fun_x2473_x5Fclosure_x24f8b41test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8Internal, ptr %"fun$73_func_0", align 8
  %2 = call ptr @malgo_hash_table_new()
  call void @malgo_hash_table_insert(ptr %2, ptr @str3967, ptr %1)
  store ptr %2, ptr @_M9showInt3241test_x2Ftestcases_x2Fmalgo_x2FShow_x2Emlg8External, align 8
  ret void
}
