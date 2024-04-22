; ModuleID = './test/testcases/malgo/Test3.mlg'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str119 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str120 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"Test3.$Cons_curry_72"(ptr %0, ptr %"Test3.$p_73_0", ptr %"Test3.$p_74_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Test3.$p_73_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Test3.$p_74_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @Test3.B(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test3.#fun_closure_118"(ptr %0, ptr %1) {
  %3 = call ptr @"Test3.$raw_fun_117"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @Test3.main(ptr %0, ptr %"Test3.$$__86_0") {
  %2 = call ptr @Test3.A(ptr null)
  %3 = call ptr @Test3.Cons(ptr null, ptr %2)
  %4 = call ptr @Test3.B(ptr null)
  %5 = call ptr @Test3.Cons(ptr null, ptr %4)
  %6 = call ptr @Test3.Nil(ptr null)
  %7 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %6)
  %12 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %11)
  %17 = call ptr @"Test3.|>"(ptr null, ptr %16)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"Test3.#fun_closure_118", ptr %fun_func_0, align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = call ptr %22(ptr %20, ptr %18)
  ret ptr %23
}

define internal ptr @"Test3.$raw_fun_117"(ptr %0, ptr %"Test3.$cons_100_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Test3.$cons_100_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_4 [
    i8 0, label %switch_branch_Test3.Nil_0
    i8 1, label %switch_branch_Test3.Cons_0
  ]

switch_branch_Test3.Nil_0:                        ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Test3.$cons_100_0", i32 0, i32 1
  %5 = call ptr @Test3.malgo_print_string(ptr null, ptr @str119)
  ret ptr %5

switch_branch_Test3.Cons_0:                       ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"Test3.$cons_100_0", i32 0, i32 1
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
  %13 = getelementptr { i8, {} }, ptr %8, i32 0, i32 1
  %14 = getelementptr { i8, <16 x i8> }, ptr %10, i32 0, i32 0
  %15 = load i8, ptr %14, align 1
  switch i8 %15, label %switch_default_2 [
    i8 0, label %switch_branch_Test3.Nil_1
    i8 1, label %switch_branch_Test3.Cons_1
  ]

switch_branch_Test3.Nil_1:                        ; preds = %switch_branch_Test3.A_0
  %16 = getelementptr { i8, {} }, ptr %10, i32 0, i32 1
  %17 = call ptr @Test3.malgo_print_string(ptr null, ptr @str119)
  ret ptr %17

switch_branch_Test3.Cons_1:                       ; preds = %switch_branch_Test3.A_0
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %10, i32 0, i32 1
  %19 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = getelementptr { i8, {} }, ptr %20, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_1 [
    i8 0, label %switch_branch_Test3.A_1
    i8 1, label %switch_branch_Test3.B_0
  ]

switch_branch_Test3.A_1:                          ; preds = %switch_branch_Test3.Cons_1
  %25 = getelementptr { i8, {} }, ptr %20, i32 0, i32 1
  %26 = call ptr @Test3.malgo_print_string(ptr null, ptr @str119)
  ret ptr %26

switch_branch_Test3.B_0:                          ; preds = %switch_branch_Test3.Cons_1
  %27 = getelementptr { i8, {} }, ptr %20, i32 0, i32 1
  %28 = getelementptr { i8, <16 x i8> }, ptr %22, i32 0, i32 0
  %29 = load i8, ptr %28, align 1
  switch i8 %29, label %switch_default_0 [
    i8 0, label %switch_branch_Test3.Nil_2
    i8 1, label %switch_branch_Test3.Cons_2
  ]

switch_branch_Test3.Nil_2:                        ; preds = %switch_branch_Test3.B_0
  %30 = getelementptr { i8, {} }, ptr %22, i32 0, i32 1
  %31 = call ptr @Test3.malgo_print_string(ptr null, ptr @str120)
  ret ptr %31

switch_branch_Test3.Cons_2:                       ; preds = %switch_branch_Test3.B_0
  %32 = getelementptr { i8, { ptr, ptr } }, ptr %22, i32 0, i32 1
  %33 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  %34 = load ptr, ptr %33, align 8
  %35 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = call ptr @Test3.malgo_print_string(ptr null, ptr @str119)
  ret ptr %37

switch_default_0:                                 ; preds = %switch_branch_Test3.B_0
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Test3.Cons_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Test3.A_0
  unreachable

switch_branch_Test3.B_1:                          ; preds = %switch_branch_Test3.Cons_0
  %38 = getelementptr { i8, {} }, ptr %8, i32 0, i32 1
  %39 = call ptr @Test3.malgo_print_string(ptr null, ptr @str119)
  ret ptr %39

switch_default_3:                                 ; preds = %switch_branch_Test3.Cons_0
  unreachable

switch_default_4:                                 ; preds = %1
  unreachable
}

define internal ptr @"Test3.#let_closure_121"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Test3.$Cons_curry_72"(ptr null, ptr %p_0, ptr %1)
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
  store ptr @"Test3.#let_closure_121", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test3.A(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Test3.Nil(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_122"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"Test3.$|>_curry_81"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Test3.|>"(ptr %0, ptr %"Test3.$x_78_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test3.$x_78_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test3.#let_closure_122", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test3.malgo_print_string(ptr %0, ptr %"Test3.$p_77_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test3.$p_77_0")
  ret ptr %2
}

define internal ptr @"Test3.$|>_curry_81"(ptr %0, ptr %"Test3.$x_82_0", ptr %"Test3.$f_83_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"Test3.$f_83_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Test3.$f_83_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"Test3.$x_82_0")
  ret ptr %6
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Test3()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test3.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_Test3() {
  ret void
}
