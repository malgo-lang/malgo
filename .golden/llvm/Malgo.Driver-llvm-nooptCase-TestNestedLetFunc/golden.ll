; ModuleID = 'test/testcases/malgo/TestNestedLetFunc.mlg'
source_filename = "test/testcases/malgo/TestNestedLetFunc.mlg"

@str27 = unnamed_addr constant [5 x i8] c"PASS\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

define internal ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/TestNestedLetFunc.mlg.$p_13_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/TestNestedLetFunc.mlg.$p_13_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.$fun_18"(ptr %0, ptr %"test/testcases/malgo/TestNestedLetFunc.mlg.$a_16_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.$fun_23"(ptr %0, ptr %"test/testcases/malgo/TestNestedLetFunc.mlg.$$__15_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.$fun_18"(ptr null, ptr %2)
  ret ptr %4
}

define internal ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.main"(ptr %0, ptr %"test/testcases/malgo/TestNestedLetFunc.mlg.$$__14_0") {
  %2 = call ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.malgo_print_string"(ptr null, ptr @str27)
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestNestedLetFunc.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/TestNestedLetFunc.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestNestedLetFunc.mlg"() {
  ret void
}
