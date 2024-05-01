; ModuleID = 'test/testcases/malgo/HelloBoxed.mlg'
source_filename = "test/testcases/malgo/HelloBoxed.mlg"

@str39 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_newline(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.String#"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_21_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_21_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_23_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_23_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.malgo_newline"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_24_0") {
  %2 = call ptr @malgo_newline(ptr %"test/testcases/malgo/HelloBoxed.mlg.$p_24_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.string#"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$x_25_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/HelloBoxed.mlg.String#", ptr %"String#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"test/testcases/malgo/HelloBoxed.mlg.$x_25_0")
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.putStrLn"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$string#_27_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/HelloBoxed.mlg.$string#_27_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/HelloBoxed.mlg.String#_0"
  ]

"switch_branch_test/testcases/malgo/HelloBoxed.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/HelloBoxed.mlg.$string#_27_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"test/testcases/malgo/HelloBoxed.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_newline_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %malgo_newline_capture_0, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @"test/testcases/malgo/HelloBoxed.mlg.malgo_newline", ptr %malgo_newline_func_0, align 8
  %16 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %13)
  ret ptr %20

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.main"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$$__34_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/HelloBoxed.mlg.String#", ptr %"String#_func_0", align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr @str39)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %putStrLn_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %putStrLn_capture_0, align 8
  %putStrLn_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/HelloBoxed.mlg.putStrLn", ptr %putStrLn_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  ret ptr %13
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/HelloBoxed.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/HelloBoxed.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/HelloBoxed.mlg"() {
  ret void
}
