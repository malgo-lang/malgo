; ModuleID = 'test/testcases/malgo/TestNestedLetFunc.mlg'
source_filename = "test/testcases/malgo/TestNestedLetFunc.mlg"

@str29 = unnamed_addr constant [5 x i8] c"PASS\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

define internal ptr @TestNestedLetFunc.malgo_print_string(ptr %0, ptr %"TestNestedLetFunc.$p_13_0") {
  %2 = call ptr @malgo_print_string(ptr %"TestNestedLetFunc.$p_13_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @"TestNestedLetFunc.#fun_closure_28"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, {} }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  ret ptr %3
}

define internal ptr @"TestNestedLetFunc.#fun_closure_27"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"TestNestedLetFunc.#fun_closure_28", ptr %fun_func_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10
}

define internal ptr @TestNestedLetFunc.main(ptr %0, ptr %"TestNestedLetFunc.$$__14_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"TestNestedLetFunc.#fun_closure_27", ptr %fun_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @TestNestedLetFunc.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr @str29)
  ret ptr %8
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_TestNestedLetFunc()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @TestNestedLetFunc.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_TestNestedLetFunc() {
  ret void
}
