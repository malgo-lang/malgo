; ModuleID = 'test/testcases/malgo/Test1.mlg'
source_filename = "test/testcases/malgo/Test1.mlg"

@str89 = unnamed_addr constant [5 x i8] c"True\00"
@str91 = unnamed_addr constant [6 x i8] c"False\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @_M5False42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M4True42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M22raw_x5Flet_x243d_x245642test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal(ptr %0, ptr %_M12t_x240_x242b42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %_M15_x5F_x241_x242c42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, {} }, ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_True_0
    i8 1, label %switch_branch_False_0
  ]

switch_branch_True_0:                             ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %_M12t_x240_x242b42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %_M12t_x240_x242b42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10

switch_branch_False_0:                            ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %_M15_x5F_x241_x242c42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %_M15_x5F_x241_x242c42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr %0, ptr %_M7p_x242942test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M7p_x242942test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M26fun_x2445_x5Fclosure_x245842test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @malgo_print_string(ptr @str89)
  ret ptr %3
}

define internal ptr @_M26fun_x244b_x5Fclosure_x245a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @malgo_print_string(ptr @str91)
  ret ptr %3
}

define internal ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2426_x243f42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M2if42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr null, ptr %2)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$45_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$45_capture_1" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %"fun$45_capture_0", ptr %"fun$45_capture_1", align 8
  %"fun$45_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @_M26fun_x2445_x5Fclosure_x245842test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal, ptr %"fun$45_func_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %5)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$4b_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$4b_capture_1" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %"fun$4b_capture_0", ptr %"fun$4b_capture_1", align 8
  %"fun$4b_func_0" = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @_M26fun_x244b_x5Fclosure_x245a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal, ptr %"fun$4b_func_0", align 8
  %12 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %11)
  ret ptr %16
}

define internal ptr @_M26let_x243e_x5Fclosure_x245c42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal(ptr %0, ptr %1) {
  %"true$2a_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"true$2a_0" = load ptr, ptr %"true$2a_addr_0", align 8
  %3 = call ptr @_M22raw_x5Flet_x243e_x245742test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal(ptr null, ptr %"true$2a_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M2if42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr %0, ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$3e_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"true$2a_0" = getelementptr { ptr }, ptr %"let$3e_capture_0", i32 0, i32 0
  store ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %"true$2a_0", align 8
  %"let$3e_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$3e_capture_0", ptr %"let$3e_capture_1", align 8
  %"let$3e_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M26let_x243e_x5Fclosure_x245c42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal, ptr %"let$3e_func_0", align 8
  ret ptr %2
}

define internal ptr @_M26let_x243d_x5Fclosure_x245d42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal(ptr %0, ptr %1) {
  %"t$0$2b_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"t$0$2b_0" = load ptr, ptr %"t$0$2b_addr_0", align 8
  %"true$2a_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"true$2a_0" = load ptr, ptr %"true$2a_addr_0", align 8
  %3 = call ptr @_M22raw_x5Flet_x243d_x245642test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal(ptr null, ptr %"t$0$2b_0", ptr %"true$2a_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M22raw_x5Flet_x243e_x245742test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal(ptr %0, ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %_M12t_x240_x242b42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$3d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"t$0$2b_0" = getelementptr { ptr, ptr }, ptr %"let$3d_capture_0", i32 0, i32 0
  store ptr %_M12t_x240_x242b42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %"t$0$2b_0", align 8
  %"true$2a_0" = getelementptr { ptr, ptr }, ptr %"let$3d_capture_0", i32 0, i32 1
  store ptr %_M10true_x242a42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Temporal_0, ptr %"true$2a_0", align 8
  %"let$3d_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$3d_capture_0", ptr %"let$3d_capture_1", align 8
  %"let$3d_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M26let_x243d_x5Fclosure_x245d42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8Internal, ptr %"let$3d_func_0", align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test1.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest1_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test1.mlg"() {
  ret void
}
