; ModuleID = 'test/testcases/malgo/Test3.mlg'
source_filename = "test/testcases/malgo/Test3.mlg"

@str137 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str138 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @Test3.A(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test3.B(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test3.Nil(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_133"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 0
  store i8 1, ptr %4, align 1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %p_0, ptr %5, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 1, i32 1
  store ptr %1, ptr %6, align 8
  ret ptr %3
}

define internal ptr @Test3.Cons(ptr %0, ptr %"Test3.$p_69_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test3.$p_69_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test3.#let_closure_133", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test3.malgo_print_string(ptr %0, ptr %"Test3.$p_77_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test3.$p_77_0")
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_134"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %x_0)
  ret ptr %7
}

define internal ptr @"Test3.|>"(ptr %0, ptr %"Test3.$x_78_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test3.$x_78_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test3.#let_closure_134", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_135"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"Test3.#fun_closure_136"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <16 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_4 [
    i8 0, label %switch_branch_Test3.Nil_0
    i8 1, label %switch_branch_Test3.Cons_0
  ]

switch_branch_Test3.Nil_0:                        ; preds = %2
  %5 = call ptr @malgo_print_string(ptr @str137)
  ret ptr %5

switch_branch_Test3.Cons_0:                       ; preds = %2
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %1, i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_3 [
    i8 0, label %switch_branch_Test3.A_0
    i8 1, label %switch_branch_Test3.B_1
  ]

switch_branch_Test3.A_0:                          ; preds = %switch_branch_Test3.Cons_0
  %13 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %14 = load i8, ptr %13, align 1
  switch i8 %14, label %switch_default_2 [
    i8 0, label %switch_branch_Test3.Nil_1
    i8 1, label %switch_branch_Test3.Cons_1
  ]

switch_branch_Test3.Nil_1:                        ; preds = %switch_branch_Test3.A_0
  %15 = call ptr @malgo_print_string(ptr @str137)
  ret ptr %15

switch_branch_Test3.Cons_1:                       ; preds = %switch_branch_Test3.A_0
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { i8, {} }, ptr %18, i32 0, i32 0
  %22 = load i8, ptr %21, align 1
  switch i8 %22, label %switch_default_1 [
    i8 0, label %switch_branch_Test3.A_1
    i8 1, label %switch_branch_Test3.B_0
  ]

switch_branch_Test3.A_1:                          ; preds = %switch_branch_Test3.Cons_1
  %23 = call ptr @malgo_print_string(ptr @str137)
  ret ptr %23

switch_branch_Test3.B_0:                          ; preds = %switch_branch_Test3.Cons_1
  %24 = getelementptr { i8, <16 x i8> }, ptr %20, i32 0, i32 0
  %25 = load i8, ptr %24, align 1
  switch i8 %25, label %switch_default_0 [
    i8 0, label %switch_branch_Test3.Nil_2
    i8 1, label %switch_branch_Test3.Cons_2
  ]

switch_branch_Test3.Nil_2:                        ; preds = %switch_branch_Test3.B_0
  %26 = call ptr @malgo_print_string(ptr @str138)
  ret ptr %26

switch_branch_Test3.Cons_2:                       ; preds = %switch_branch_Test3.B_0
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %20, i32 0, i32 1
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr @malgo_print_string(ptr @str137)
  ret ptr %32

switch_default_0:                                 ; preds = %switch_branch_Test3.B_0
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Test3.Cons_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Test3.A_0
  unreachable

switch_branch_Test3.B_1:                          ; preds = %switch_branch_Test3.Cons_0
  %33 = call ptr @malgo_print_string(ptr @str137)
  ret ptr %33

switch_default_3:                                 ; preds = %switch_branch_Test3.Cons_0
  unreachable

switch_default_4:                                 ; preds = %2
  unreachable
}

define internal ptr @Test3.main(ptr %0, ptr %"Test3.$$__86_0") {
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
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %12, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"Test3.#let_closure_135", ptr %let_func_0, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"Test3.#fun_closure_136", ptr %fun_func_0, align 8
  %18 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Test3()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test3.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_Test3() {
  ret void
}
