; ModuleID = 'test/testcases/malgo/Null.mlg'
source_filename = "test/testcases/malgo/Null.mlg"

@str183 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_exit_failure(ptr)

define internal ptr @"test/testcases/malgo/Null.mlg.malgo_exit_failure"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$p_134_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"test/testcases/malgo/Null.mlg.$p_134_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.$|>_curry_138"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$x_139_0", ptr %"test/testcases/malgo/Null.mlg.$f_140_0") {
  %2 = getelementptr { ptr, ptr }, ptr %"test/testcases/malgo/Null.mlg.$f_140_0", i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %"test/testcases/malgo/Null.mlg.$f_140_0", i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, ptr %"test/testcases/malgo/Null.mlg.$x_139_0")
  ret ptr %6
}

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/Null.mlg.Nil"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.mHead"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$nil_143_0") {
  %2 = getelementptr { i8, <16 x i8> }, ptr %"test/testcases/malgo/Null.mlg.$nil_143_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Null.mlg.Nil_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Null.mlg.Cons_0"
  ]

"switch_branch_test/testcases/malgo/Null.mlg.Nil_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Null.mlg.$nil_143_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Null.mlg.Nothing"(ptr null)
  ret ptr %5

"switch_branch_test/testcases/malgo/Null.mlg.Cons_0": ; preds = %1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/Null.mlg.$nil_143_0", i32 0, i32 1
  %7 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %6, i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr @"test/testcases/malgo/Null.mlg.Just"(ptr null, ptr %8)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Null.mlg.False"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.null"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$as_149_0") {
  %2 = call ptr @"test/testcases/malgo/Null.mlg.mHead"(ptr null, ptr %"test/testcases/malgo/Null.mlg.$as_149_0")
  %3 = call ptr @"test/testcases/malgo/Null.mlg.isNothing"(ptr null, ptr %2)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Null.mlg.#fun_closure_181"(ptr %0, ptr %1) {
  %3 = call ptr @"test/testcases/malgo/Null.mlg.$raw_fun_180"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Null.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$$__153_0") {
  %2 = call ptr @"test/testcases/malgo/Null.mlg.True"(ptr null)
  %3 = call ptr @"test/testcases/malgo/Null.mlg.Cons"(ptr null, ptr %2)
  %4 = call ptr @"test/testcases/malgo/Null.mlg.True"(ptr null)
  %5 = call ptr @"test/testcases/malgo/Null.mlg.Cons"(ptr null, ptr %4)
  %6 = call ptr @"test/testcases/malgo/Null.mlg.Nil"(ptr null)
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
  %17 = call ptr @"test/testcases/malgo/Null.mlg.null"(ptr null, ptr %16)
  %18 = call ptr @"test/testcases/malgo/Null.mlg.|>"(ptr null, ptr %17)
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %19, i32 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#fun_closure_181", ptr %fun_func_0, align 8
  %20 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %21 = load ptr, ptr %20, align 8
  %22 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr %23(ptr %21, ptr %19)
  ret ptr %24
}

define internal ptr @"test/testcases/malgo/Null.mlg.#let_closure_182"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/Null.mlg.$Cons_curry_128"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Null.mlg.Cons"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$p_125_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Null.mlg.$p_125_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_182", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.Nothing"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.isNothing"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$nothing_147_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/Null.mlg.$nothing_147_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Null.mlg.Nothing_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Null.mlg.Just_0"
  ]

"switch_branch_test/testcases/malgo/Null.mlg.Nothing_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Null.mlg.$nothing_147_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Null.mlg.True"(ptr null)
  ret ptr %5

"switch_branch_test/testcases/malgo/Null.mlg.Just_0": ; preds = %1
  %6 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/Null.mlg.$nothing_147_0", i32 0, i32 1
  %7 = getelementptr { ptr }, ptr %6, i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr @"test/testcases/malgo/Null.mlg.False"(ptr null)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Null.mlg.$Cons_curry_128"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$p_129_0", ptr %"test/testcases/malgo/Null.mlg.$p_130_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Null.mlg.$p_129_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"test/testcases/malgo/Null.mlg.$p_130_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.True"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.$raw_fun_180"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$false_169_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Null.mlg.$false_169_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Null.mlg.False_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Null.mlg.True_0"
  ]

"switch_branch_test/testcases/malgo/Null.mlg.False_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Null.mlg.$false_169_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Null.mlg.malgo_print_string"(ptr null, ptr @str183)
  ret ptr %5

"switch_branch_test/testcases/malgo/Null.mlg.True_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Null.mlg.$false_169_0", i32 0, i32 1
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %8 = getelementptr { i8, {} }, ptr %7, i32 0, i32 0
  store i8 0, ptr %8, align 1
  %9 = call ptr @"test/testcases/malgo/Null.mlg.malgo_exit_failure"(ptr null, ptr %7)
  ret ptr %9

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Null.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$p_133_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/Null.mlg.$p_133_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.Just"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$p_120_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/Null.mlg.$p_120_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Null.mlg.#let_closure_184"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/Null.mlg.$|>_curry_138"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Null.mlg.|>"(ptr %0, ptr %"test/testcases/malgo/Null.mlg.$x_135_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/Null.mlg.$x_135_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_184", ptr %let_func_0, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Null.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Null.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Null.mlg"() {
  ret void
}
