; ModuleID = './test/testcases/malgo/TestNestedLetFunc.mlg'
source_filename = "./test/testcases/malgo/TestNestedLetFunc.mlg"

@str27 = unnamed_addr constant [5 x i8] c"PASS\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

define internal ptr @TestNestedLetFunc.main(ptr %0, ptr %"TestNestedLetFunc.$$__14_0") {
  %2 = call ptr @malgo_print_string(ptr @str27)
  ret ptr %2
}

define internal ptr @TestNestedLetFunc.malgo_print_string(ptr %0, ptr %"TestNestedLetFunc.$p_13_0") {
  %2 = call ptr @malgo_print_string(ptr %"TestNestedLetFunc.$p_13_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_TestNestedLetFunc()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @TestNestedLetFunc.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_TestNestedLetFunc() {
  ret void
}
