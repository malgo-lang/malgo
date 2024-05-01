; ModuleID = 'test/testcases/malgo/DataDef.mlg'
source_filename = "test/testcases/malgo/DataDef.mlg"

declare void @GC_init()

declare ptr @malgo_int64_t_to_string(i64)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/DataDef.mlg.$Cons_curry_24"(ptr %0, ptr %"test/testcases/malgo/DataDef.mlg.$p_25_0", ptr %"test/testcases/malgo/DataDef.mlg.$p_26_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/DataDef.mlg.$p_25_0", ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %2, i32 0, i32 1, i32 1
  store ptr %"test/testcases/malgo/DataDef.mlg.$p_26_0", ptr %5, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.Int#"(ptr %0, i64 %"test/testcases/malgo/DataDef.mlg.$p_18_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"test/testcases/malgo/DataDef.mlg.$p_18_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.Nil"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.#let_closure_43"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/DataDef.mlg.$Cons_curry_24"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.Cons"(ptr %0, ptr %"test/testcases/malgo/DataDef.mlg.$p_21_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/DataDef.mlg.$p_21_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/DataDef.mlg.#let_closure_43", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.malgo_int64_t_to_string"(ptr %0, i64 %"test/testcases/malgo/DataDef.mlg.$p_29_0") {
  %2 = call ptr @malgo_int64_t_to_string(i64 %"test/testcases/malgo/DataDef.mlg.$p_29_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/DataDef.mlg.$p_30_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/DataDef.mlg.$p_30_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.#fun_closure_44"(ptr %0, ptr %1) {
  %malgo_print_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  %malgo_int64_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %malgo_int64_t_to_string_0 = load ptr, ptr %malgo_int64_t_to_string_addr_0, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/DataDef.mlg.Int#_0"
  ]

"switch_branch_test/testcases/malgo/DataDef.mlg.Int#_0": ; preds = %2
  %5 = getelementptr { i8, { i64 } }, ptr %1, i32 0, i32 1
  %6 = getelementptr { i64 }, ptr %5, i32 0, i32 0
  %7 = load i64, ptr %6, align 4
  %8 = getelementptr { ptr, ptr }, ptr %malgo_int64_t_to_string_0, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %malgo_int64_t_to_string_0, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i64 %7)
  %13 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %12)
  ret ptr %17

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/DataDef.mlg.main"(ptr %0, ptr %"test/testcases/malgo/DataDef.mlg.$$__31_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/DataDef.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %malgo_print_string_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %malgo_print_string_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int64_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %malgo_int64_t_to_string_capture_0, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"test/testcases/malgo/DataDef.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0, align 8
  %malgo_int64_t_to_string_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %4, ptr %malgo_int64_t_to_string_0, align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/DataDef.mlg.#fun_closure_44", ptr %fun_func_0, align 8
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int#_capture_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  store ptr null, ptr %"Int#_capture_0", align 8
  %"Int#_func_0" = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  store ptr @"test/testcases/malgo/DataDef.mlg.Int#", ptr %"Int#_func_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, i64 1)
  %11 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  ret ptr %15
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/DataDef.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/DataDef.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal void @"malgo_load_test/testcases/malgo/DataDef.mlg"() {
  ret void
}
