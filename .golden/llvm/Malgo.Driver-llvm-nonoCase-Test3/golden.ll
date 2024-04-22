; ModuleID = './test/testcases/malgo/Test3.mlg'
source_filename = "./test/testcases/malgo/Test3.mlg"

@str120 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str121 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

define internal ptr @"Test3.$|>_curry_81"(ptr %0, ptr %"Test3.$x_82_0", ptr %"Test3.$f_83_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"Test3.$f_83_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Test3.$f_83_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"Test3.$x_82_0")
  ret ptr %6
}

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

define internal ptr @"Test3.#let_closure_117"(ptr %0, ptr %1) {
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
  store ptr @"Test3.#let_closure_117", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test3.malgo_print_string(ptr %0, ptr %"Test3.$p_77_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test3.$p_77_0")
  ret ptr %2
}

define internal ptr @"Test3.#let_closure_118"(ptr %0, ptr %1) {
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
  store ptr @"Test3.#let_closure_118", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test3.#fun_closure_119"(ptr %0, ptr %1) {
  %malgo_print_string_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  %3 = getelementptr { i8, <16 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_4 [
    i8 0, label %switch_branch_Test3.Nil_0
    i8 1, label %switch_branch_Test3.Cons_0
  ]

switch_branch_Test3.Nil_0:                        ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str120)
  ret ptr %10

switch_branch_Test3.Cons_0:                       ; preds = %2
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %1, i32 0, i32 1
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  %17 = load i8, ptr %16, align 1
  switch i8 %17, label %switch_default_3 [
    i8 0, label %switch_branch_Test3.A_0
    i8 1, label %switch_branch_Test3.B_1
  ]

switch_branch_Test3.A_0:                          ; preds = %switch_branch_Test3.Cons_0
  %18 = getelementptr { i8, {} }, ptr %13, i32 0, i32 1
  %19 = getelementptr { i8, <16 x i8> }, ptr %15, i32 0, i32 0
  %20 = load i8, ptr %19, align 1
  switch i8 %20, label %switch_default_2 [
    i8 0, label %switch_branch_Test3.Nil_1
    i8 1, label %switch_branch_Test3.Cons_1
  ]

switch_branch_Test3.Nil_1:                        ; preds = %switch_branch_Test3.A_0
  %21 = getelementptr { i8, {} }, ptr %15, i32 0, i32 1
  %22 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr @str120)
  ret ptr %26

switch_branch_Test3.Cons_1:                       ; preds = %switch_branch_Test3.A_0
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %15, i32 0, i32 1
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = getelementptr { i8, {} }, ptr %29, i32 0, i32 0
  %33 = load i8, ptr %32, align 1
  switch i8 %33, label %switch_default_1 [
    i8 0, label %switch_branch_Test3.A_1
    i8 1, label %switch_branch_Test3.B_0
  ]

switch_branch_Test3.A_1:                          ; preds = %switch_branch_Test3.Cons_1
  %34 = getelementptr { i8, {} }, ptr %29, i32 0, i32 1
  %35 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call ptr %38(ptr %36, ptr @str120)
  ret ptr %39

switch_branch_Test3.B_0:                          ; preds = %switch_branch_Test3.Cons_1
  %40 = getelementptr { i8, {} }, ptr %29, i32 0, i32 1
  %41 = getelementptr { i8, <16 x i8> }, ptr %31, i32 0, i32 0
  %42 = load i8, ptr %41, align 1
  switch i8 %42, label %switch_default_0 [
    i8 0, label %switch_branch_Test3.Nil_2
    i8 1, label %switch_branch_Test3.Cons_2
  ]

switch_branch_Test3.Nil_2:                        ; preds = %switch_branch_Test3.B_0
  %43 = getelementptr { i8, {} }, ptr %31, i32 0, i32 1
  %44 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %45 = load ptr, ptr %44, align 8
  %46 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %47 = load ptr, ptr %46, align 8
  %48 = call ptr %47(ptr %45, ptr @str121)
  ret ptr %48

switch_branch_Test3.Cons_2:                       ; preds = %switch_branch_Test3.B_0
  %49 = getelementptr { i8, { ptr, ptr } }, ptr %31, i32 0, i32 1
  %50 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 0
  %51 = load ptr, ptr %50, align 8
  %52 = getelementptr { ptr, ptr }, ptr %49, i32 0, i32 1
  %53 = load ptr, ptr %52, align 8
  %54 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %55 = load ptr, ptr %54, align 8
  %56 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %57 = load ptr, ptr %56, align 8
  %58 = call ptr %57(ptr %55, ptr @str120)
  ret ptr %58

switch_default_0:                                 ; preds = %switch_branch_Test3.B_0
  unreachable

switch_default_1:                                 ; preds = %switch_branch_Test3.Cons_1
  unreachable

switch_default_2:                                 ; preds = %switch_branch_Test3.A_0
  unreachable

switch_branch_Test3.B_1:                          ; preds = %switch_branch_Test3.Cons_0
  %59 = getelementptr { i8, {} }, ptr %13, i32 0, i32 1
  %60 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %61 = load ptr, ptr %60, align 8
  %62 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %63 = load ptr, ptr %62, align 8
  %64 = call ptr %63(ptr %61, ptr @str120)
  ret ptr %64

switch_default_3:                                 ; preds = %switch_branch_Test3.Cons_0
  unreachable

switch_default_4:                                 ; preds = %2
  unreachable
}

define internal ptr @Test3.main(ptr %0, ptr %"Test3.$$__86_0") {
  %2 = call ptr @Test3.A(ptr null)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %Cons_capture_0, align 8
  %Cons_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @Test3.Cons, ptr %Cons_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = call ptr @Test3.B(ptr null)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %Cons_capture_1, align 8
  %Cons_func_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @Test3.Cons, ptr %Cons_func_1, align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %9)
  %16 = call ptr @Test3.Nil(ptr null)
  %17 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %16)
  %22 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = call ptr %25(ptr %23, ptr %21)
  %27 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"|>_capture_0" = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  store ptr null, ptr %"|>_capture_0", align 8
  %"|>_func_0" = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  store ptr @"Test3.|>", ptr %"|>_func_0", align 8
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %26)
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @Test3.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %malgo_print_string_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %34, ptr %malgo_print_string_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"Test3.#fun_closure_119", ptr %fun_func_0, align 8
  %35 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %32, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call ptr %38(ptr %36, ptr %33)
  ret ptr %39
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
