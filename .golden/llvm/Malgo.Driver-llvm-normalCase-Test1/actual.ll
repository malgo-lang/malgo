; ModuleID = './test/testcases/malgo/Test1.mlg'
source_filename = "./test/testcases/malgo/Test1.mlg"

@str102 = unnamed_addr constant [5 x i8] c"True\00"
@str104 = unnamed_addr constant [6 x i8] c"False\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"Test1.#let_closure_100"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = call ptr @"Test1.$raw_let_99"(ptr null, ptr %d_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Test1.#fun_closure_101"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_print_string(ptr @str102)
  ret ptr %3
}

define internal ptr @"Test1.#fun_closure_103"(ptr %0, ptr %1) {
  %3 = call ptr @malgo_print_string(ptr @str104)
  ret ptr %3
}

define internal ptr @Test1.main(ptr %0, ptr %"Test1.$$__63_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %2, ptr %d_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"Test1.#let_closure_100", ptr %let_func_0, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"Test1.#fun_closure_101", ptr %fun_func_0, align 8
  %6 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %5)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_3 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr %fun_capture_2, ptr %fun_capture_3, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"Test1.#fun_closure_103", ptr %fun_func_1, align 8
  %12 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr %11)
  ret ptr %16
}

define internal ptr @Test1.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_105"(ptr %0, ptr %1) {
  %true_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @"Test1.$raw_let_96"(ptr null, ptr %true_0, ptr %1)
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
  store ptr @"Test1.#let_closure_105", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Test1.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_106"(ptr %0, ptr %1) {
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %3 = call ptr @"Test1.$raw_let_95"(ptr null, ptr %t_0, ptr %true_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Test1.$raw_let_96"(ptr %0, ptr %"Test1.$true_42_0", ptr %"Test1.$t_43_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test1.$t_43_0", ptr %t_0, align 8
  %true_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"Test1.$true_42_0", ptr %true_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test1.#let_closure_106", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_107"(ptr %0, ptr %1) {
  %d_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %3 = call ptr @"Test1.$raw_let_98"(ptr null, ptr %d_0, ptr %t_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Test1.$raw_let_99"(ptr %0, ptr %"Test1.$d_64_0", ptr %"Test1.$t_86_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %d_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Test1.$d_64_0", ptr %d_0, align 8
  %t_0 = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"Test1.$t_86_0", ptr %t_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Test1.#let_closure_107", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test1.$raw_let_98"(ptr %0, ptr %"Test1.$d_64_0", ptr %"Test1.$t_86_0", ptr %"Test1.$__88_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test1.$d_64_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test1.True_0
    i8 1, label %switch_branch_Test1.False_0
  ]

switch_branch_Test1.True_0:                       ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %"Test1.$t_86_0", i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Test1.$t_86_0", i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10

switch_branch_Test1.False_0:                      ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %"Test1.$__88_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Test1.$__88_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Test1.malgo_print_string(ptr %0, ptr %"Test1.$p_41_0") {
  %2 = call ptr @malgo_print_string(ptr %"Test1.$p_41_0")
  ret ptr %2
}

define internal ptr @"Test1.$raw_let_95"(ptr %0, ptr %"Test1.$t_43_0", ptr %"Test1.$true_42_0", ptr %"Test1.$__44_0") {
  %2 = getelementptr { i8, {} }, ptr %"Test1.$true_42_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Test1.True_0
    i8 1, label %switch_branch_Test1.False_0
  ]

switch_branch_Test1.True_0:                       ; preds = %1
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = getelementptr { ptr, ptr }, ptr %"Test1.$t_43_0", i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %"Test1.$t_43_0", i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  ret ptr %10

switch_branch_Test1.False_0:                      ; preds = %1
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  %13 = getelementptr { ptr, ptr }, ptr %"Test1.$__44_0", i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Test1.$__44_0", i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  ret ptr %17

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_Test1()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Test1.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_Test1() {
  ret void
}
