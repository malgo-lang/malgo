; ModuleID = 'test/testcases/malgo/HelloBoxed.mlg'
source_filename = "test/testcases/malgo/HelloBoxed.mlg"

@str47 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_newline(ptr)

define internal ptr @_M24malgo_x5Fprint_x5Fstring47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal23_0) {
  %2 = call ptr @malgo_print_string(ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal23_0)
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @_M10String_x2347test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal21_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal21_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M10string_x2347test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M1x47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal25_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %_M1x47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal25_0, ptr %4, align 8
  ret ptr %2
}

define internal ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal34_0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str47, ptr %4, align 8
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %7 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_print_string(ptr %9)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = call ptr @malgo_newline(ptr %11)
  ret ptr %13

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @_M16malgo_x5Fnewline47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal24_0) {
  %2 = call ptr @malgo_newline(ptr %_M1p47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal24_0)
  ret ptr %2
}

define internal ptr @_M8putStrLn47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr %0, ptr %_M10string_x2347test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal27_0) {
  %2 = getelementptr { i8, <8 x i8> }, ptr %_M10string_x2347test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal27_0, i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_String#_0"
  ]

"switch_branch_String#_0":                        ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %_M10string_x2347test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg10Temporal27_0, i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_newline(ptr %8)
  ret ptr %10

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/HelloBoxed.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main47test_x2Ftestcases_x2Fmalgo_x2FHelloBoxed_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/HelloBoxed.mlg"() {
  ret void
}
