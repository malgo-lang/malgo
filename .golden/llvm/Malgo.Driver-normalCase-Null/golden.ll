; ModuleID = 'test/testcases/malgo/Null.mlg'
source_filename = "test/testcases/malgo/Null.mlg"

@str263 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @_M5False41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M3Nil41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M7Nothing41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M4True41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M7p_x248541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M7p_x248541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M32let_x248e_x24da_x5Fclosure_x24fd41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$98$d9_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$98$d9_0" = load ptr, ptr %"d$98$d9_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$98$d9_0")
  ret ptr %7
}

define internal ptr @_M26fun_x24ae_x5Fclosure_x24fe41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M22raw_x5Ffun_x24ae_x24f441test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M32let_x248e_x24da_x5Fclosure_x24ff41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$98$d9_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$98$d9_0" = load ptr, ptr %"d$98$d9_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$98$d9_0")
  ret ptr %7
}

define internal ptr @_M27fun_x24ae_x5Fclosure_x2410041test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M22raw_x5Ffun_x24ae_x24f541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M33let_x248e_x24da_x5Fclosure_x2410141test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$98$d9_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$98$d9_0" = load ptr, ptr %"d$98$d9_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$98$d9_0")
  ret ptr %7
}

define internal ptr @_M27fun_x24ae_x5Fclosure_x2410241test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M22raw_x5Ffun_x24ae_x24f741test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M33let_x248e_x24da_x5Fclosure_x2410341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$98$d9_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$98$d9_0" = load ptr, ptr %"d$98$d9_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$98$d9_0")
  ret ptr %7
}

define internal ptr @_M27fun_x24ae_x5Fclosure_x2410441test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M22raw_x5Ffun_x24ae_x24f841test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2476_x249941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 1, ptr %5, align 1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %8, i32 0, i32 0
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %4, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %8, i32 0, i32 1, i32 1
  store ptr %6, ptr %11, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 0
  store i8 1, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 0
  store ptr %2, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1, i32 1
  store ptr %8, ptr %15, align 8
  %16 = getelementptr { i8, <16 x i8> }, ptr %12, i32 0, i32 0
  %17 = load i8, ptr %16, align 1
  switch i8 %17, label %switch_default_2 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, <8 x i8> }, ptr %18, i32 0, i32 0
  %21 = load i8, ptr %20, align 1
  switch i8 %21, label %switch_default_0 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %switch_branch_Nil_0
  %22 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %23 = getelementptr { i8, {} }, ptr %22, i32 0, i32 0
  store i8 1, ptr %23, align 1
  %24 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8e$da_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$98$d9_0" = getelementptr { ptr }, ptr %"let$8e$da_capture_0", i32 0, i32 0
  store ptr %22, ptr %"d$98$d9_0", align 8
  %"let$8e$da_capture_1" = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  store ptr %"let$8e$da_capture_0", ptr %"let$8e$da_capture_1", align 8
  %"let$8e$da_func_0" = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  store ptr @_M32let_x248e_x24da_x5Fclosure_x24fd41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$8e$da_func_0", align 8
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$ae_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$ae_capture_1" = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 0
  store ptr %"fun$ae_capture_0", ptr %"fun$ae_capture_1", align 8
  %"fun$ae_func_0" = getelementptr { ptr, ptr }, ptr %25, i32 0, i32 1
  store ptr @_M26fun_x24ae_x5Fclosure_x24fe41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"fun$ae_func_0", align 8
  %26 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 0
  %27 = load ptr, ptr %26, align 8
  %28 = getelementptr { ptr, ptr }, ptr %24, i32 0, i32 1
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr %29(ptr %27, ptr %25)
  ret ptr %30

switch_branch_Just_0:                             ; preds = %switch_branch_Nil_0
  %31 = getelementptr { i8, { ptr } }, ptr %18, i32 0, i32 1
  %32 = getelementptr { ptr }, ptr %31, i32 0, i32 0
  %33 = load ptr, ptr %32, align 8
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %35 = getelementptr { i8, {} }, ptr %34, i32 0, i32 0
  store i8 0, ptr %35, align 1
  %36 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8e$da_capture_2" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$98$d9_1" = getelementptr { ptr }, ptr %"let$8e$da_capture_2", i32 0, i32 0
  store ptr %34, ptr %"d$98$d9_1", align 8
  %"let$8e$da_capture_3" = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  store ptr %"let$8e$da_capture_2", ptr %"let$8e$da_capture_3", align 8
  %"let$8e$da_func_1" = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  store ptr @_M32let_x248e_x24da_x5Fclosure_x24ff41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$8e$da_func_1", align 8
  %37 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$ae_capture_2" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$ae_capture_3" = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 0
  store ptr %"fun$ae_capture_2", ptr %"fun$ae_capture_3", align 8
  %"fun$ae_func_1" = getelementptr { ptr, ptr }, ptr %37, i32 0, i32 1
  store ptr @_M27fun_x24ae_x5Fclosure_x2410041test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"fun$ae_func_1", align 8
  %38 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 0
  %39 = load ptr, ptr %38, align 8
  %40 = getelementptr { ptr, ptr }, ptr %36, i32 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = call ptr %41(ptr %39, ptr %37)
  ret ptr %42

switch_default_0:                                 ; preds = %switch_branch_Nil_0
  unreachable

switch_branch_Cons_0:                             ; preds = %1
  %43 = getelementptr { i8, { ptr, ptr } }, ptr %12, i32 0, i32 1
  %44 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 0
  %45 = load ptr, ptr %44, align 8
  %46 = getelementptr { ptr, ptr }, ptr %43, i32 0, i32 1
  %47 = load ptr, ptr %46, align 8
  %48 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %49 = getelementptr { i8, { ptr } }, ptr %48, i32 0, i32 0
  store i8 1, ptr %49, align 1
  %50 = getelementptr { i8, { ptr } }, ptr %48, i32 0, i32 1, i32 0
  store ptr %45, ptr %50, align 8
  %51 = getelementptr { i8, <8 x i8> }, ptr %48, i32 0, i32 0
  %52 = load i8, ptr %51, align 1
  switch i8 %52, label %switch_default_1 [
    i8 0, label %switch_branch_Nothing_1
    i8 1, label %switch_branch_Just_1
  ]

switch_branch_Nothing_1:                          ; preds = %switch_branch_Cons_0
  %53 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %54 = getelementptr { i8, {} }, ptr %53, i32 0, i32 0
  store i8 1, ptr %54, align 1
  %55 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8e$da_capture_4" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$98$d9_2" = getelementptr { ptr }, ptr %"let$8e$da_capture_4", i32 0, i32 0
  store ptr %53, ptr %"d$98$d9_2", align 8
  %"let$8e$da_capture_5" = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  store ptr %"let$8e$da_capture_4", ptr %"let$8e$da_capture_5", align 8
  %"let$8e$da_func_2" = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  store ptr @_M33let_x248e_x24da_x5Fclosure_x2410141test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$8e$da_func_2", align 8
  %56 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$ae_capture_4" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$ae_capture_5" = getelementptr { ptr, ptr }, ptr %56, i32 0, i32 0
  store ptr %"fun$ae_capture_4", ptr %"fun$ae_capture_5", align 8
  %"fun$ae_func_2" = getelementptr { ptr, ptr }, ptr %56, i32 0, i32 1
  store ptr @_M27fun_x24ae_x5Fclosure_x2410241test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"fun$ae_func_2", align 8
  %57 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 0
  %58 = load ptr, ptr %57, align 8
  %59 = getelementptr { ptr, ptr }, ptr %55, i32 0, i32 1
  %60 = load ptr, ptr %59, align 8
  %61 = call ptr %60(ptr %58, ptr %56)
  ret ptr %61

switch_branch_Just_1:                             ; preds = %switch_branch_Cons_0
  %62 = getelementptr { i8, { ptr } }, ptr %48, i32 0, i32 1
  %63 = getelementptr { ptr }, ptr %62, i32 0, i32 0
  %64 = load ptr, ptr %63, align 8
  %65 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %66 = getelementptr { i8, {} }, ptr %65, i32 0, i32 0
  store i8 0, ptr %66, align 1
  %67 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8e$da_capture_6" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$98$d9_3" = getelementptr { ptr }, ptr %"let$8e$da_capture_6", i32 0, i32 0
  store ptr %65, ptr %"d$98$d9_3", align 8
  %"let$8e$da_capture_7" = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 0
  store ptr %"let$8e$da_capture_6", ptr %"let$8e$da_capture_7", align 8
  %"let$8e$da_func_3" = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 1
  store ptr @_M33let_x248e_x24da_x5Fclosure_x2410341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$8e$da_func_3", align 8
  %68 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$ae_capture_6" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$ae_capture_7" = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 0
  store ptr %"fun$ae_capture_6", ptr %"fun$ae_capture_7", align 8
  %"fun$ae_func_3" = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 1
  store ptr @_M27fun_x24ae_x5Fclosure_x2410441test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"fun$ae_func_3", align 8
  %69 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 1
  %72 = load ptr, ptr %71, align 8
  %73 = call ptr %72(ptr %70, ptr %68)
  ret ptr %73

switch_default_1:                                 ; preds = %switch_branch_Cons_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M24malgo_x5Fexit_x5Ffailure41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M7p_x248641test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_exit_failure(ptr %_M7p_x248641test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M27let_x248e_x5Fclosure_x2410541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$b$87_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$b$87_0" = load ptr, ptr %"x$b$87_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"x$b$87_0")
  ret ptr %7
}

define internal ptr @_M8_x7C_x3E41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M12x_x24b_x248741test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$8e_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$b$87_0" = getelementptr { ptr }, ptr %"let$8e_capture_0", i32 0, i32 0
  store ptr %_M12x_x24b_x248741test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %"x$b$87_0", align 8
  %"let$8e_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$8e_capture_0", ptr %"let$8e_capture_1", align 8
  %"let$8e_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x248e_x5Fclosure_x2410541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$8e_func_0", align 8
  ret ptr %2
}

define internal ptr @_M27let_x2484_x5Fclosure_x2410641test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal(ptr %0, ptr %1) {
  %"p$7d_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"p$7d_0" = load ptr, ptr %"p$7d_addr_0", align 8
  %3 = call ptr @_M22raw_x5Flet_x2484_x24f341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr null, ptr %"p$7d_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4Cons41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M7p_x247d41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$84_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"p$7d_0" = getelementptr { ptr }, ptr %"let$84_capture_0", i32 0, i32 0
  store ptr %_M7p_x247d41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %"p$7d_0", align 8
  %"let$84_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$84_capture_0", ptr %"let$84_capture_1", align 8
  %"let$84_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M27let_x2484_x5Fclosure_x2410641test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Internal, ptr %"let$84_func_0", align 8
  ret ptr %2
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f441test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f641test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f741test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f841test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24f941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24fa41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24fb41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Ffun_x24ae_x24fc41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M11false_x24a941test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_False_0
    i8 1, label %switch_branch_True_0
  ]

switch_branch_False_0:                            ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str263)
  ret ptr %4

switch_branch_True_0:                             ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = call ptr @malgo_exit_failure(ptr %5)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4Just41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M7p_x247841test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x247841test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M9isNothing41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M13nothing_x249341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M13nothing_x249341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 1, ptr %5, align 1
  ret ptr %4

switch_branch_Just_0:                             ; preds = %1
  %6 = getelementptr { i8, { ptr } }, ptr %_M13nothing_x249341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = getelementptr { ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, {} }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M5mHead41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M9nil_x248f41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M9nil_x248f41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %_M9nil_x248f41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 0
  store i8 1, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %11, i32 0, i32 1, i32 0
  store ptr %8, ptr %13, align 8
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M4null41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr %0, ptr %_M13as_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M13as_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, <8 x i8> }, ptr %4, i32 0, i32 0
  %7 = load i8, ptr %6, align 1
  switch i8 %7, label %switch_default_0 [
    i8 0, label %switch_branch_Nothing_0
    i8 1, label %switch_branch_Just_0
  ]

switch_branch_Nothing_0:                          ; preds = %switch_branch_Nil_0
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 1, ptr %9, align 1
  ret ptr %8

switch_branch_Just_0:                             ; preds = %switch_branch_Nil_0
  %10 = getelementptr { i8, { ptr } }, ptr %4, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  ret ptr %13

switch_default_0:                                 ; preds = %switch_branch_Nil_0
  unreachable

switch_branch_Cons_0:                             ; preds = %1
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %_M13as_x248_x249541test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, i32 0, i32 1
  %16 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %21 = getelementptr { i8, { ptr } }, ptr %20, i32 0, i32 0
  store i8 1, ptr %21, align 1
  %22 = getelementptr { i8, { ptr } }, ptr %20, i32 0, i32 1, i32 0
  store ptr %17, ptr %22, align 8
  %23 = getelementptr { i8, <8 x i8> }, ptr %20, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_1 [
    i8 0, label %switch_branch_Nothing_1
    i8 1, label %switch_branch_Just_1
  ]

switch_branch_Nothing_1:                          ; preds = %switch_branch_Cons_0
  %25 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %26 = getelementptr { i8, {} }, ptr %25, i32 0, i32 0
  store i8 1, ptr %26, align 1
  ret ptr %25

switch_branch_Just_1:                             ; preds = %switch_branch_Cons_0
  %27 = getelementptr { i8, { ptr } }, ptr %20, i32 0, i32 1
  %28 = getelementptr { ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %31 = getelementptr { i8, {} }, ptr %30, i32 0, i32 0
  store i8 0, ptr %31, align 1
  ret ptr %30

switch_default_1:                                 ; preds = %switch_branch_Cons_0
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Flet_x2484_x24f341test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal(ptr %0, ptr %_M7p_x247d41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %_M7p_x247e41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x247d41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %_M7p_x247e41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8Temporal_0, ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Null.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main41test_x2Ftestcases_x2Fmalgo_x2FNull_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Null.mlg"() {
  ret void
}
