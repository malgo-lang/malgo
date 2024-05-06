; ModuleID = 'test/testcases/malgo/Test3.mlg'
source_filename = "test/testcases/malgo/Test3.mlg"

@str139 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str140 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/Test3.mlg.A"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.B"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.Nil"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$p_77_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/Test3.mlg.$p_77_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.#let_closure_135"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %d_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/Test3.mlg.#fun_closure_136"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/Test3.mlg.$raw_fun_134"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test3.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$$__86_0") {
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
  store ptr @"test/testcases/malgo/Test3.mlg.#let_closure_135", ptr %let_func_0, align 8
  %17 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test3.mlg.#fun_closure_136", ptr %fun_func_0, align 8
  %18 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr %17)
  ret ptr %22
}

define internal ptr @"test/testcases/malgo/Test3.mlg.#let_closure_137"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %x_0)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/Test3.mlg.|>"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$x_78_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Test3.mlg.$x_78_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test3.mlg.#let_closure_137", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.#let_closure_138"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/Test3.mlg.$raw_let_133"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test3.mlg.Cons"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$p_69_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Test3.mlg.$p_69_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test3.mlg.#let_closure_138", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test3.mlg.$raw_fun_134"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$cons_100_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"test/testcases/malgo/Test3.mlg.$cons_100_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_4 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test3.mlg.Nil_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_0"
  ]

"switch_branch_test/testcases/malgo/Test3.mlg.Nil_0": ; preds = %1
  %4 = call ptr @malgo_print_string(ptr @str139)
  ret ptr %4

"switch_branch_test/testcases/malgo/Test3.mlg.Cons_0": ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/Test3.mlg.$cons_100_0", i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  switch i8 %11, label %switch_default_3 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test3.mlg.A_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test3.mlg.B_1"
  ]

"switch_branch_test/testcases/malgo/Test3.mlg.A_0": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_0"
  %12 = getelementptr { i8, <16 x i8> }, ptr %9, i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_2 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test3.mlg.Nil_1"
    i8 1, label %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_1"
  ]

"switch_branch_test/testcases/malgo/Test3.mlg.Nil_1": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.A_0"
  %14 = call ptr @malgo_print_string(ptr @str139)
  ret ptr %14

"switch_branch_test/testcases/malgo/Test3.mlg.Cons_1": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.A_0"
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %9, i32 0, i32 1
  %16 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { i8, {} }, ptr %17, i32 0, i32 0
  %21 = load i8, ptr %20, align 1
  switch i8 %21, label %switch_default_1 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test3.mlg.A_1"
    i8 1, label %"switch_branch_test/testcases/malgo/Test3.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test3.mlg.A_1": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_1"
  %22 = call ptr @malgo_print_string(ptr @str139)
  ret ptr %22

"switch_branch_test/testcases/malgo/Test3.mlg.B_0": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_1"
  %23 = getelementptr { i8, <16 x i8> }, ptr %19, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test3.mlg.Nil_2"
    i8 1, label %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_2"
  ]

"switch_branch_test/testcases/malgo/Test3.mlg.Nil_2": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.B_0"
  %25 = call ptr @malgo_print_string(ptr @str140)
  ret ptr %25

"switch_branch_test/testcases/malgo/Test3.mlg.Cons_2": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.B_0"
  %26 = getelementptr { i8, { ptr, ptr } }, ptr %19, i32 0, i32 1
  %27 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 0
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr { ptr, ptr }, ptr %26, i32 0, i32 1
  %30 = load ptr, ptr %29, align 8
  %31 = call ptr @malgo_print_string(ptr @str139)
  ret ptr %31

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.B_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.A_0"
  unreachable

"switch_branch_test/testcases/malgo/Test3.mlg.B_1": ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_0"
  %32 = call ptr @malgo_print_string(ptr @str139)
  ret ptr %32

switch_default_3:                                 ; preds = %"switch_branch_test/testcases/malgo/Test3.mlg.Cons_0"
  unreachable

switch_default_4:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Test3.mlg.$raw_let_133"(ptr %0, ptr %"test/testcases/malgo/Test3.mlg.$p_69_0", ptr %"test/testcases/malgo/Test3.mlg.$p_70_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Test3.mlg.$p_69_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"test/testcases/malgo/Test3.mlg.$p_70_0", ptr %5, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test3.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Test3.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test3.mlg"() {
  ret void
}
