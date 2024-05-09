; ModuleID = 'test/testcases/malgo/Test6.mlg'
source_filename = "test/testcases/malgo/Test6.mlg"

@str63 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str64 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @_M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @_M24malgo_x5Fprint_x5Fstring42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0, ptr %_M1p42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal34_0) {
  %2 = call ptr @malgo_print_string(ptr %_M1p42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal34_0)
  ret ptr %2
}

define internal ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal39_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  %5 = load i8, ptr %4, align 1
  switch i8 %5, label %switch_default_3 [
    i8 0, label %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
    i8 1, label %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
    i8 2, label %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
  ]

switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 2, ptr %7, align 1
  %8 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
    i8 1, label %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
    i8 2, label %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  ]

switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1: ; preds = %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  %10 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %10

switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  %11 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %11

switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  %12 = call ptr @malgo_print_string(ptr @str64)
  ret ptr %12

switch_default_0:                                 ; preds = %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  unreachable

switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1: ; preds = %1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_exit_failure(ptr %13)
  %16 = getelementptr { i8, {} }, ptr %15, i32 0, i32 0
  %17 = load i8, ptr %16, align 1
  switch i8 %17, label %switch_default_1 [
    i8 0, label %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
    i8 1, label %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
    i8 2, label %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
  ]

switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2: ; preds = %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
  %18 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %18

switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2: ; preds = %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
  %19 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %19

switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1: ; preds = %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
  %20 = call ptr @malgo_print_string(ptr @str64)
  ret ptr %20

switch_default_1:                                 ; preds = %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_1
  unreachable

switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2: ; preds = %1
  %21 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %22 = getelementptr { i8, {} }, ptr %21, i32 0, i32 0
  store i8 0, ptr %22, align 1
  %23 = call ptr @malgo_exit_failure(ptr %21)
  %24 = getelementptr { i8, {} }, ptr %23, i32 0, i32 0
  %25 = load i8, ptr %24, align 1
  switch i8 %25, label %switch_default_2 [
    i8 0, label %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3
    i8 1, label %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3
    i8 2, label %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3
  ]

switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3: ; preds = %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
  %26 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %26

switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3: ; preds = %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
  %27 = call ptr @malgo_print_string(ptr @str63)
  ret ptr %27

switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_3: ; preds = %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
  %28 = call ptr @malgo_print_string(ptr @str64)
  ret ptr %28

switch_default_2:                                 ; preds = %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_2
  unreachable

switch_default_3:                                 ; preds = %1
  unreachable
}

define internal ptr @_M24malgo_x5Fexit_x5Ffailure42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0, ptr %_M1p42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal33_0) {
  %2 = call ptr @malgo_exit_failure(ptr %_M1p42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal33_0)
  ret ptr %2
}

define internal ptr @_M4rtob42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr %0, ptr %_M1r42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal35_0) {
  %2 = getelementptr { i8, {} }, ptr %_M1r42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg10Temporal35_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
    i8 1, label %switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
    i8 2, label %switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0
  ]

switch_branch__M1R42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 2, ptr %5, align 1
  ret ptr %4

switch_branch__M1G42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, {} }, ptr %6, i32 0, i32 0
  store i8 0, ptr %7, align 1
  %8 = call ptr @malgo_exit_failure(ptr %6)
  ret ptr %8

switch_branch__M1B42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External_0: ; preds = %1
  %9 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %10 = getelementptr { i8, {} }, ptr %9, i32 0, i32 0
  store i8 0, ptr %10, align 1
  %11 = call ptr @malgo_exit_failure(ptr %9)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test6.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main42test_x2Ftestcases_x2Fmalgo_x2FTest6_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test6.mlg"() {
  ret void
}
