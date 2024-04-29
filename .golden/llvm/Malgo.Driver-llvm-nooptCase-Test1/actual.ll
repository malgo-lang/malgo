; ModuleID = './test/testcases/malgo/Test1.mlg'
source_filename = "./test/testcases/malgo/Test1.mlg"

@str86 = unnamed_addr constant [6 x i8] c"False\00"
@str87 = unnamed_addr constant [5 x i8] c"True\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"Test1.#fun_closure_83"(ptr %0, ptr %1) {
  %3 = call ptr @"Test1.$raw_fun_81"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @"Test1.#fun_closure_84"(ptr %0, ptr %1) {
  %3 = call ptr @"Test1.$raw_fun_82"(ptr null, ptr %1)
  ret ptr %3
}

define internal ptr @Test1.main(ptr %0, ptr %"Test1.$$__63_0") {
  %2 = call ptr @Test1.True(ptr null)
  %3 = call ptr @Test1.if(ptr null, ptr %2)
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"Test1.#fun_closure_83", ptr %fun_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = call ptr %8(ptr %6, ptr %4)
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"Test1.#fun_closure_84", ptr %fun_func_1, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %9, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  ret ptr %15
}

define internal ptr @"Test1.$if_curry_51"(ptr %0, ptr %"Test1.$true_52_0", ptr %"Test1.$t_53_0", ptr %"Test1.$__54_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test1.$true_52_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test1.True_0
    i8 1, label %switch_branch_Test1.False_0
  ]

switch_branch_Test1.True_0:                       ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Test1.$true_52_0", i32 0, i32 1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, {} }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { ptr, ptr }, ptr %"Test1.$t_53_0", i32 0, i32 0
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %"Test1.$t_53_0", i32 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = call ptr %10(ptr %8, ptr %5)
  ret ptr %11

switch_branch_Test1.False_0:                      ; preds = %1
  %12 = getelementptr { i8, {} }, ptr %"Test1.$true_52_0", i32 0, i32 1
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %14 = getelementptr { i8, {} }, ptr %13, i32 0, i32 0
  store i8 0, ptr %14, align 1
  %15 = getelementptr { ptr, ptr }, ptr %"Test1.$__54_0", i32 0, i32 0
  %16 = load ptr, ptr %15, align 8
  %17 = getelementptr { ptr, ptr }, ptr %"Test1.$__54_0", i32 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr %18(ptr %16, ptr %13)
  ret ptr %19

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Test1.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_85"(ptr %0, ptr %1) {
  %true_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @"Test1.$raw_let_79"(ptr null, ptr %true_0, ptr %1)
  ret ptr %3
}

define internal ptr @Test1.if(ptr %0, ptr %"Test1.$true_42_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %true_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test1.$true_42_0", ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test1.#let_closure_85", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test1.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test1.$raw_fun_82"(ptr %0, ptr %"Test1.$$__73_0") {
  %2 = call ptr @Test1.malgo_print_string(ptr null, ptr @str86)
  ret ptr %2
}

define internal ptr @"Test1.$raw_fun_81"(ptr %0, ptr %"Test1.$$__67_0") {
  %2 = call ptr @Test1.malgo_print_string(ptr null, ptr @str87)
  ret ptr %2
}

define internal ptr @Test1.malgo_print_string(ptr %0, ptr %"Test1.$p_41_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test1.$p_41_0")
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_88"(ptr %0, ptr %1) {
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @"Test1.$if_curry_51"(ptr null, ptr %true_0, ptr %t_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Test1.$raw_let_79"(ptr %0, ptr %"Test1.$true_42_0", ptr %"Test1.$t_43_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test1.$t_43_0", ptr %t_0, align 8
  %true_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"Test1.$true_42_0", ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test1.#let_closure_88", ptr %let_func_0, align 8
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Test1()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test1.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_Test1() {
  ret void
}
