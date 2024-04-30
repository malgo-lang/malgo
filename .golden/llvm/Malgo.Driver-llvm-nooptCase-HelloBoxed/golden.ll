; ModuleID = 'test/testcases/malgo/HelloBoxed.mlg'
source_filename = "test/testcases/malgo/HelloBoxed.mlg"

@str39 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_newline(ptr)

define internal ptr @HelloBoxed.main(ptr %0, ptr %"HelloBoxed.$$__34_0") {
  %2 = call ptr @"HelloBoxed.String#"(ptr null, ptr @str39)
  %3 = call ptr @HelloBoxed.putStrLn(ptr null, ptr %2)
  ret ptr %3
}

define internal ptr @HelloBoxed.malgo_newline(ptr %0, ptr %"HelloBoxed.$p_24_0") {
  %2 = call ptr @malgo_newline(ptr %"HelloBoxed.$p_24_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @HelloBoxed.putStrLn(ptr %0, ptr %"HelloBoxed.$string#_27_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"HelloBoxed.$string#_27_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_HelloBoxed.String#_0"
  ]

"switch_branch_HelloBoxed.String#_0":             ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"HelloBoxed.$string#_27_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @HelloBoxed.malgo_print_string(ptr null, ptr %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @HelloBoxed.malgo_newline(ptr null, ptr %8)
  ret ptr %10

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"HelloBoxed.string#"(ptr %0, ptr %"HelloBoxed.$x_25_0") {
  %2 = call ptr @"HelloBoxed.String#"(ptr null, ptr %"HelloBoxed.$x_25_0")
  ret ptr %2
}

define internal ptr @HelloBoxed.malgo_print_string(ptr %0, ptr %"HelloBoxed.$p_23_0") {
  %2 = call ptr @malgo_print_string(ptr %"HelloBoxed.$p_23_0")
  ret ptr %2
}

define internal ptr @"HelloBoxed.String#"(ptr %0, ptr %"HelloBoxed.$p_21_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"HelloBoxed.$p_21_0", ptr %4, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_HelloBoxed()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @HelloBoxed.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_HelloBoxed() {
  ret void
}
