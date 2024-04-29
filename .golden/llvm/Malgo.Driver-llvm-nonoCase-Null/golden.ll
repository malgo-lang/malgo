; ModuleID = './test/testcases/malgo/Null.mlg'
source_filename = "./test/testcases/malgo/Null.mlg"

@str183 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_exit_failure(ptr)

define internal ptr @"Null.$|>_curry_138"(ptr %0, ptr %"Null.$x_139_0", ptr %"Null.$f_140_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"Null.$f_140_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Null.$f_140_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"Null.$x_139_0")
  ret ptr %6
}

declare ptr @malgo_malloc(i64)

define internal ptr @"Null.$Cons_curry_128"(ptr %0, ptr %"Null.$p_129_0", ptr %"Null.$p_130_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Null.$p_129_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"Null.$p_130_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @Null.Nothing(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Null.Just(ptr %0, ptr %"Null.$p_120_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Null.$p_120_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @Null.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Null.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @Null.Nil(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Null.#let_closure_180"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Null.$Cons_curry_128"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Null.Cons(ptr %0, ptr %"Null.$p_125_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Null.$p_125_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Null.#let_closure_180", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Null.malgo_print_string(ptr %0, ptr %"Null.$p_133_0") {
  %2 = call ptr @malgo_print_string(ptr %"Null.$p_133_0")
  ret ptr %2
}

define internal ptr @Null.malgo_exit_failure(ptr %0, ptr %"Null.$p_134_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"Null.$p_134_0")
  ret ptr %2
}

define internal ptr @"Null.#let_closure_181"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"Null.$|>_curry_138"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Null.|>"(ptr %0, ptr %"Null.$x_135_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Null.$x_135_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Null.#let_closure_181", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Null.mHead(ptr %0, ptr %"Null.$nil_143_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"Null.$nil_143_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Null.Nil_0
    i8 1, label %switch_branch_Null.Cons_0
  ]

switch_branch_Null.Nil_0:                         ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Null.$nil_143_0", i32 0, i32 1
  %5 = call ptr @Null.Nothing(ptr null)
  ret ptr %5

switch_branch_Null.Cons_0:                        ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"Null.$nil_143_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Just_capture_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr null, ptr %Just_capture_0, align 8
  %Just_func_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @Null.Just, ptr %Just_func_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %8)
  ret ptr %16

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Null.isNothing(ptr %0, ptr %"Null.$nothing_147_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Null.$nothing_147_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Null.Nothing_0
    i8 1, label %switch_branch_Null.Just_0
  ]

switch_branch_Null.Nothing_0:                     ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Null.$nothing_147_0", i32 0, i32 1
  %5 = call ptr @Null.True(ptr null)
  ret ptr %5

switch_branch_Null.Just_0:                        ; preds = %1
  %6 = getelementptr { i8, { ptr } }, ptr %"Null.$nothing_147_0", i32 0, i32 1
  %7 = getelementptr { ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @Null.False(ptr null)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Null.null(ptr %0, ptr %"Null.$as_149_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %mHead_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %mHead_capture_0, align 8
  %mHead_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @Null.mHead, ptr %mHead_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Null.$as_149_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %isNothing_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %isNothing_capture_0, align 8
  %isNothing_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @Null.isNothing, ptr %isNothing_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  ret ptr %13
}

define internal ptr @"Null.#fun_closure_182"(ptr %0, ptr %1) {
  %malgo_exit_failure_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %malgo_exit_failure_0 = load ptr, ptr %malgo_exit_failure_addr_0, align 8
  %malgo_print_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_Null.False_0
    i8 1, label %switch_branch_Null.True_0
  ]

switch_branch_Null.False_0:                       ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str183)
  ret ptr %10

switch_branch_Null.True_0:                        ; preds = %2
  %11 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %13 = getelementptr { i8, {} }, ptr %12, i32 0, i32 0
  store i8 0, ptr %13, align 1
  %14 = getelementptr { ptr, ptr }, ptr %malgo_exit_failure_0, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %malgo_exit_failure_0, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @Null.main(ptr %0, ptr %"Null.$$__153_0") {
  %2 = call ptr @Null.True(ptr null)
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %Cons_capture_0, align 8
  %Cons_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @Null.Cons, ptr %Cons_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = call ptr @Null.True(ptr null)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Cons_capture_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %Cons_capture_1, align 8
  %Cons_func_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @Null.Cons, ptr %Cons_func_1, align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %9)
  %16 = call ptr @Null.Nil(ptr null)
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
  %null_capture_0 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  store ptr null, ptr %null_capture_0, align 8
  %null_func_0 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  store ptr @Null.null, ptr %null_func_0, align 8
  %28 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 0
  %29 = load ptr, ptr %28, align 8
  %30 = getelementptr { ptr, ptr }, ptr %27, i32 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = call ptr %31(ptr %29, ptr %26)
  %33 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"|>_capture_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  store ptr null, ptr %"|>_capture_0", align 8
  %"|>_func_0" = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  store ptr @"Null.|>", ptr %"|>_func_0", align 8
  %34 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 0
  %35 = load ptr, ptr %34, align 8
  %36 = getelementptr { ptr, ptr }, ptr %33, i32 0, i32 1
  %37 = load ptr, ptr %36, align 8
  %38 = call ptr %37(ptr %35, ptr %32)
  %39 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_exit_failure_capture_0 = getelementptr { ptr, ptr }, ptr %40, i32 0, i32 0
  store ptr null, ptr %malgo_exit_failure_capture_0, align 8
  %malgo_exit_failure_func_0 = getelementptr { ptr, ptr }, ptr %40, i32 0, i32 1
  store ptr @Null.malgo_exit_failure, ptr %malgo_exit_failure_func_0, align 8
  %malgo_exit_failure_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %40, ptr %malgo_exit_failure_0, align 8
  %41 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %41, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %41, i32 0, i32 1
  store ptr @Null.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %malgo_print_string_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %41, ptr %malgo_print_string_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %39, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %39, i32 0, i32 1
  store ptr @"Null.#fun_closure_182", ptr %fun_func_0, align 8
  %42 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 0
  %43 = load ptr, ptr %42, align 8
  %44 = getelementptr { ptr, ptr }, ptr %38, i32 0, i32 1
  %45 = load ptr, ptr %44, align 8
  %46 = call ptr %45(ptr %43, ptr %39)
  ret ptr %46
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Null()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Null.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_Null() {
  ret void
}
