; ModuleID = 'test/testcases/malgo/TestNestedLetFunc.mlg'
source_filename = "test/testcases/malgo/TestNestedLetFunc.mlg"

@str27 = unnamed_addr constant [5 x i8] c"PASS\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

define internal ptr @_M24malgo_x5Fprint_x5Fstring54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg8External(ptr %0, ptr %_M1p54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg10Temporal13_0) {
  %2 = call ptr @malgo_print_string(ptr %_M1p54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg10Temporal13_0)
  ret ptr %2
}

define internal ptr @_M4main54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg8External(ptr %0, ptr %_M8_x24_x5F54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg10Temporal14_0) {
  %2 = call ptr @malgo_print_string(ptr @str27)
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/TestNestedLetFunc.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @_M4main54test_x2Ftestcases_x2Fmalgo_x2FTestNestedLetFunc_x2Emlg8External(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/TestNestedLetFunc.mlg"() {
  ret void
}
