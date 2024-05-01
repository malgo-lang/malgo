; ModuleID = 'test/testcases/malgo/ToplevelVariableNoImport.mlg'
source_filename = "test/testcases/malgo/ToplevelVariableNoImport.mlg"

@"test/testcases/malgo/ToplevelVariableNoImport.mlg.one" = global ptr undef
@"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp" = global ptr undef
@str289 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare i32 @malgo_add_int32_t(i32, i32)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_int32_t_to_string(i32)

declare ptr @malgo_malloc(i64)

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$addInt32_curry_244"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_245_0", ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_246_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_245_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_245_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_246_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_1"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_1": ; preds = %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_246_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"addInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %"addInt32#_capture_0", align 8
  %"addInt32#_func_0" = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addInt32#", ptr %"addInt32#_func_0", align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, i32 %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call i32 %21(ptr %19, i32 %11)
  %23 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %24 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 0
  %25 = load ptr, ptr %24, align 8
  %26 = getelementptr { ptr, ptr }, ptr %23, i32 0, i32 1
  %27 = load ptr, ptr %26, align 8
  %28 = call ptr %27(ptr %25, i32 %22)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$addInt32#_curry_228"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_229_0", i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$y_230_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_add_int32_t_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_add_int32_t_capture_0, align 8
  %malgo_add_int32_t_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_add_int32_t", ptr %malgo_add_int32_t_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_229_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call i32 %11(ptr %9, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$y_230_0")
  ret i32 %12
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$const_curry_203"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_204_0", ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$__205_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_204_0"
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$malgo_add_int32_t_curry_176"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_177_0", i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_178_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_177_0", i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_178_0")
  ret i32 %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_167_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_167_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.String#"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_169_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_169_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing"(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Just"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_172_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_172_0", ptr %4, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_284"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$malgo_add_int32_t_curry_176"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_add_int32_t"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_174_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_174_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_284", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_print_string"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_180_0") {
  %2 = call ptr @malgo_print_string(ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_180_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_int32_t_to_string"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_181_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$p_181_0")
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.toStringInt32#"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_182_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_int32_t_to_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_int32_t_to_string_capture_0, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_182_0")
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.toStringInt32"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_184_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_184_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_184_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"toStringInt32#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"toStringInt32#_capture_0", align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i32 %6)
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.String#", ptr %"String#_func_0", align 8
  %14 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = call ptr %17(ptr %15, ptr %12)
  ret ptr %18

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printString#"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_189_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %malgo_print_string_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %malgo_print_string_capture_0, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_189_0")
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printString"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$string#_191_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$string#_191_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.String#_0"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$string#_191_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"printString#_capture_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr null, ptr %"printString#_capture_0", align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printString#", ptr %"printString#_func_0", align 8
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %6)
  ret ptr %12

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printInt32"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$i_194_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %toStringInt32_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %toStringInt32_capture_0, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.toStringInt32", ptr %toStringInt32_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$i_194_0")
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printString", ptr %printString_func_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %7)
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.identity"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_200_0") {
  ret ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_200_0"
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_285"(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$const_curry_203"(ptr null, ptr %a_0, ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.const"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_201_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %a_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_201_0", ptr %a_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_285", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.constId"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$eta_210_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.identity", ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.const", ptr %const_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %2)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$eta_210_0")
  ret ptr %13
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_286"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$addInt32#_curry_228"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addInt32#"(ptr %0, i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_223_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_223_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_286", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.$addInt32_curry_244"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addInt32"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_235_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$int32#_235_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addOne"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$eta_257_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addInt32_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %addInt32_capture_0, align 8
  %addInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addInt32", ptr %addInt32_func_0, align 8
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = load ptr, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.one", align 8
  %8 = call ptr %6(ptr %4, ptr %7)
  %9 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 0
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i32 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr %12(ptr %10, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$eta_257_0")
  ret ptr %13
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_288"(ptr %0, ptr %1) {
  %printString_addr_0 = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0"
    i8 1, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %"String#_0", i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr @str289)
  %11 = getelementptr { ptr, ptr }, ptr %printString_0, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %printString_0, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %10)
  ret ptr %15

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0": ; preds = %2
  %16 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %17 = getelementptr { ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %20 = getelementptr { i8, {} }, ptr %19, i32 0, i32 0
  store i8 0, ptr %20, align 1
  ret ptr %19

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.main"(ptr %0, ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$$__259_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printString_capture_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr null, ptr %printString_capture_0, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printString", ptr %printString_func_0, align 8
  %printString_0 = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 0
  store ptr %3, ptr %printString_0, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"String#_capture_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %"String#_capture_0", align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.String#", ptr %"String#_func_0", align 8
  %"String#_0" = getelementptr { ptr, ptr }, ptr %fun_capture_0, i32 0, i32 1
  store ptr %4, ptr %"String#_0", align 8
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_288", ptr %fun_func_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = load ptr, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp", align 8
  %10 = call ptr %8(ptr %6, ptr %9)
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %11, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.const", ptr %const_func_0, align 8
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %constId_capture_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr null, ptr %constId_capture_0, align 8
  %constId_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.constId", ptr %constId_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %addOne_capture_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  store ptr null, ptr %addOne_capture_0, align 8
  %addOne_func_0 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.addOne", ptr %addOne_func_0, align 8
  %19 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 0
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %18, i32 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = load ptr, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.one", align 8
  %24 = call ptr %22(ptr %20, ptr %23)
  %25 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %26 = load ptr, ptr %25, align 8
  %27 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %28 = load ptr, ptr %27, align 8
  %29 = call ptr %28(ptr %26, ptr %24)
  %30 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %printInt32_capture_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  store ptr null, ptr %printInt32_capture_0, align 8
  %printInt32_func_0 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.printInt32", ptr %printInt32_func_0, align 8
  %31 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 0
  %32 = load ptr, ptr %31, align 8
  %33 = getelementptr { ptr, ptr }, ptr %30, i32 0, i32 1
  %34 = load ptr, ptr %33, align 8
  %35 = call ptr %34(ptr %32, ptr %29)
  ret ptr %35
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @"malgo_load_test/testcases/malgo/ToplevelVariableNoImport.mlg"()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.main"(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_294"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0"
    i8 1, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0"
  ]

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0": ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing"(ptr null)
  ret ptr %6

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0": ; preds = %2
  %7 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing"(ptr null)
  ret ptr %10

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal void @"malgo_load_test/testcases/malgo/ToplevelVariableNoImport.mlg"() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"Int32#_capture_0" = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr null, ptr %"Int32#_capture_0", align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Int32#", ptr %"Int32#_func_0", align 8
  %2 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 0
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i32 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = call ptr %5(ptr %3, i32 1)
  store ptr %6, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.one", align 8
  %7 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_294", ptr %fun_func_0, align 8
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, {} }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %Just_capture_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  store ptr null, ptr %Just_capture_0, align 8
  %Just_func_0 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.Just", ptr %Just_func_0, align 8
  %11 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = getelementptr { ptr, ptr }, ptr %10, i32 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = call ptr %14(ptr %12, ptr %8)
  %16 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %17 = load ptr, ptr %16, align 8
  %18 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %19 = load ptr, ptr %18, align 8
  %20 = call ptr %19(ptr %17, ptr %15)
  store ptr %20, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp", align 8
  ret void
}
