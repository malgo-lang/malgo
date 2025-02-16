; ModuleID = 'test/testcases/malgo/Test2.mlg'
source_filename = "test/testcases/malgo/Test2.mlg"

@str41 = unnamed_addr constant [6 x i8] c"WRONG\00"
@str42 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/Test2.mlg.R"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.G"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.B"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 2, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$p_26_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/Test2.mlg.$p_26_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Test2.mlg.rtob"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0") {
  %2 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test2.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test2.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test2.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test2.mlg.R_0": ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  %5 = call ptr @"test/testcases/malgo/Test2.mlg.B"(ptr null)
  ret ptr %5

"switch_branch_test/testcases/malgo/Test2.mlg.G_0": ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  ret ptr %"test/testcases/malgo/Test2.mlg.$r_27_0"

"switch_branch_test/testcases/malgo/Test2.mlg.B_0": ; preds = %1
  %7 = getelementptr { i8, {} }, ptr %"test/testcases/malgo/Test2.mlg.$r_27_0", i32 0, i32 1
  ret ptr %"test/testcases/malgo/Test2.mlg.$r_27_0"

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/Test2.mlg.#fun_closure_40"(ptr %0, ptr %1) {
  %malgo_print_string_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  %3 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/Test2.mlg.R_0"
    i8 1, label %"switch_branch_test/testcases/malgo/Test2.mlg.G_0"
    i8 2, label %"switch_branch_test/testcases/malgo/Test2.mlg.B_0"
  ]

"switch_branch_test/testcases/malgo/Test2.mlg.R_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str41)
  ret ptr %10

"switch_branch_test/testcases/malgo/Test2.mlg.G_0": ; preds = %2
  %11 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %12 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %13 = load ptr, ptr %12, align 8
  %14 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr %15(ptr %13, ptr @str41)
  ret ptr %16

"switch_branch_test/testcases/malgo/Test2.mlg.B_0": ; preds = %2
  %17 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %18 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, ptr @str42)
  ret ptr %22

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/Test2.mlg.main"(ptr %0, ptr %"test/testcases/malgo/Test2.mlg.$$__28_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test2.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %malgo_print_string_0 = getelementptr { ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %malgo_print_string_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test2.mlg.#fun_closure_40", ptr %fun_func_0, align 8
  %4 = call ptr @"test/testcases/malgo/Test2.mlg.R"(ptr null)
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %rtob_capture_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %rtob_capture_0, align 8
  %rtob_func_0 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"test/testcases/malgo/Test2.mlg.rtob", ptr %rtob_func_0, align 8
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %4)
  %11 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  ret ptr %15
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/Test2.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/Test2.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/Test2.mlg"() {
  ret void
}
