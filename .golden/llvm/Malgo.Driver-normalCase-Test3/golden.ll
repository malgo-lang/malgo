; ModuleID = 'test/testcases/malgo/Test3.mlg'
source_filename = "test/testcases/malgo/Test3.mlg"

@str151 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str152 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @_M1A42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M1B42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M3Nil42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0, ptr %_M7p_x244d42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_print_string(ptr %_M7p_x244d42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0)
  ret ptr %2
}

define internal ptr @_M32let_x2455_x2486_x5Fclosure_x249342test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr %0, ptr %1) {
  %"d$60_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"d$60_0" = load ptr, ptr %"d$60_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"d$60_0")
  ret ptr %7
}

define internal ptr @_M26fun_x246d_x5Fclosure_x249442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr %0, ptr %1) {
  %3 = call ptr @_M22raw_x5Ffun_x246d_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0, ptr %_M20_x24_x5F_x2441_x245642test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
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
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$55$86_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"d$60_0" = getelementptr { ptr }, ptr %"let$55$86_capture_0", i32 0, i32 0
  store ptr %12, ptr %"d$60_0", align 8
  %"let$55$86_capture_1" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %"let$55$86_capture_0", ptr %"let$55$86_capture_1", align 8
  %"let$55$86_func_0" = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @_M32let_x2455_x2486_x5Fclosure_x249342test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"let$55$86_func_0", align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"fun$6d_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %"fun$6d_capture_1" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %"fun$6d_capture_0", ptr %"fun$6d_capture_1", align 8
  %"fun$6d_func_0" = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @_M26fun_x246d_x5Fclosure_x249442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"fun$6d_func_0", align 8
  %18 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define internal ptr @_M26let_x2455_x5Fclosure_x249542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr %0, ptr %1) {
  %"x$3$4e_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"x$3$4e_0" = load ptr, ptr %"x$3$4e_addr_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"x$3$4e_0")
  ret ptr %7
}

define internal ptr @_M8_x7C_x3E42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0, ptr %_M12x_x243_x244e42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$55_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"x$3$4e_0" = getelementptr { ptr }, ptr %"let$55_capture_0", i32 0, i32 0
  store ptr %_M12x_x243_x244e42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, ptr %"x$3$4e_0", align 8
  %"let$55_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$55_capture_0", ptr %"let$55_capture_1", align 8
  %"let$55_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M26let_x2455_x5Fclosure_x249542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"let$55_func_0", align 8
  ret ptr %2
}

define internal ptr @_M26let_x244c_x5Fclosure_x249642test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal(ptr %0, ptr %1) {
  %"p$45_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"p$45_0" = load ptr, ptr %"p$45_addr_0", align 8
  %3 = call ptr @_M22raw_x5Flet_x244c_x249142test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal(ptr null, ptr %"p$45_0", ptr %1)
  ret ptr %3
}

define internal ptr @_M4Cons42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr %0, ptr %_M7p_x244542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"let$4c_capture_0" = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"p$45_0" = getelementptr { ptr }, ptr %"let$4c_capture_0", i32 0, i32 0
  store ptr %_M7p_x244542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, ptr %"p$45_0", align 8
  %"let$4c_capture_1" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %"let$4c_capture_0", ptr %"let$4c_capture_1", align 8
  %"let$4c_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @_M26let_x244c_x5Fclosure_x249642test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Internal, ptr %"let$4c_func_0", align 8
  ret ptr %2
}

define internal ptr @_M22raw_x5Ffun_x246d_x249242test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal(ptr %0, ptr %_M10cons_x246442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = getelementptr { i8, <16 x i8> }, ptr %_M10cons_x246442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_4 [
    i8 0, label %switch_branch_Nil_0
    i8 1, label %switch_branch_Cons_0
  ]

switch_branch_Nil_0:                              ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str151)
  ret ptr %4

switch_branch_Cons_0:                             ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %_M10cons_x246442test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_3 [
    i8 0, label %switch_branch_A_0
    i8 1, label %switch_branch_B_1
  ]

switch_branch_A_0:                                ; preds = %switch_branch_Cons_0
  %12 = getelementptr { i8, <16 x i8> }, ptr %9, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_2 [
    i8 0, label %switch_branch_Nil_1
    i8 1, label %switch_branch_Cons_1
  ]

switch_branch_Nil_1:                              ; preds = %switch_branch_A_0
  %14 = call ptr @malgo_print_string(ptr @str151)
  ret ptr %14

switch_branch_Cons_1:                             ; preds = %switch_branch_A_0
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %9, i32 0, i32 1
  %16 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { i8, {} }, ptr %17, i32 0, i32 0
  %21 = load i8, ptr %20, align 1
  switch i8 %21, label %switch_default_1 [
    i8 0, label %switch_branch_A_1
    i8 1, label %switch_branch_B_0
  ]

switch_branch_A_1:                                ; preds = %switch_branch_Cons_1
  %22 = call ptr @malgo_print_string(ptr @str151)
  ret ptr %22

switch_branch_B_0:                                ; preds = %switch_branch_Cons_1
  %23 = getelementptr { i8, <16 x i8> }, ptr %19, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_0 [
    i8 0, label %switch_branch_Nil_2
    i8 1, label %switch_branch_Cons_2
  ]

switch_branch_Nil_2:                              ; preds = %switch_branch_B_0
  %25 = call ptr @malgo_print_string(ptr @str152)
  ret ptr %25

switch_branch_Cons_2:                             ; preds = %switch_branch_B_0
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1
  %27 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr @malgo_print_string(ptr @str151)
  ret ptr %31

switch_default_0:                                 ; preds = %switch_branch_B_0
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Cons_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_A_0
  unreachable

switch_branch_B_1:                                ; preds = %switch_branch_Cons_0
  %32 = call ptr @malgo_print_string(ptr @str151)
  ret ptr %32

switch_default_3:                                 ; preds = %switch_branch_Cons_0
  unreachable

switch_default_4:                                 ; preds = %1
  unreachable
}

define internal ptr @_M22raw_x5Flet_x244c_x249142test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal(ptr %0, ptr %_M7p_x244542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, ptr %_M7p_x244642test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M7p_x244542test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %_M7p_x244642test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8Temporal_0, ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test3.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest3_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test3.mlg"() {
  ret void
}
