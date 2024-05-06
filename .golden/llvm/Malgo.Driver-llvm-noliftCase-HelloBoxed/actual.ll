; ModuleID = 'test/testcases/malgo/HelloBoxed.mlg'
source_filename = "test/testcases/malgo/HelloBoxed.mlg"

@str47 = unnamed_addr constant [13 x i8] c"Hello, world\00"

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
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/HelloBoxed.mlg.$x_25_0", ptr %4, align 8
  ret ptr %2
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
  %7 = call ptr @malgo_print_string(ptr %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_newline(ptr %8)
  ret ptr %10

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/HelloBoxed.mlg.main"(ptr %0, ptr %"test/testcases/malgo/HelloBoxed.mlg.$$__34_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr @str47, ptr %4, align 8
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/HelloBoxed.mlg.String#_0"
  ]

"switch_branch_test/testcases/malgo/HelloBoxed.mlg.String#_0": ; preds = %1
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
