; ModuleID = 'test/testcases/malgo/ToplevelVariableNoImport.mlg'
source_filename = "test/testcases/malgo/ToplevelVariableNoImport.mlg"

@ToplevelVariableNoImport.one = global ptr undef
@ToplevelVariableNoImport.comp = global ptr undef
@str286 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare i32 @malgo_add_int32_t(i32, i32)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_int32_t_to_string(i32)

define internal ptr @"ToplevelVariableNoImport.printString#"(ptr %0, ptr %"ToplevelVariableNoImport.$x_189_0") {
  %2 = call ptr @ToplevelVariableNoImport.malgo_print_string(ptr null, ptr %"ToplevelVariableNoImport.$x_189_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @ToplevelVariableNoImport.constId(ptr %0, ptr %"ToplevelVariableNoImport.$eta_210_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_0, align 8
  %3 = call ptr @ToplevelVariableNoImport.const(ptr null, ptr %2)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %"ToplevelVariableNoImport.$eta_210_0")
  ret ptr %8
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_284"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"ToplevelVariableNoImport.$addInt32_curry_244"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @ToplevelVariableNoImport.addInt32(ptr %0, ptr %"ToplevelVariableNoImport.$int32#_235_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"ToplevelVariableNoImport.$int32#_235_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_284", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_285"(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$const_curry_203"(ptr null, ptr %a_0, ptr %1)
  ret ptr %3
}

define internal ptr @ToplevelVariableNoImport.const(ptr %0, ptr %"ToplevelVariableNoImport.$a_201_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %a_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"ToplevelVariableNoImport.$a_201_0", ptr %a_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_285", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.$fun_266"(ptr %0, ptr %"ToplevelVariableNoImport.$nothing_260_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"ToplevelVariableNoImport.$nothing_260_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_ToplevelVariableNoImport.Nothing_0
    i8 1, label %switch_branch_ToplevelVariableNoImport.Just_0
  ]

switch_branch_ToplevelVariableNoImport.Nothing_0: ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"ToplevelVariableNoImport.$nothing_260_0", i32 0, i32 1
  %5 = call ptr @"ToplevelVariableNoImport.String#"(ptr null, ptr @str286)
  %6 = call ptr @ToplevelVariableNoImport.printString(ptr null, ptr %5)
  ret ptr %6

switch_branch_ToplevelVariableNoImport.Just_0:    ; preds = %1
  %7 = getelementptr { i8, { ptr } }, ptr %"ToplevelVariableNoImport.$nothing_260_0", i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %11 = getelementptr { i8, {} }, ptr %10, i32 0, i32 0
  store i8 0, ptr %11, align 1
  ret ptr %10

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"ToplevelVariableNoImport.Int32#"(ptr %0, i32 %"ToplevelVariableNoImport.$p_167_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"ToplevelVariableNoImport.$p_167_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.addOne(ptr %0, ptr %"ToplevelVariableNoImport.$eta_257_0") {
  %2 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %3 = call ptr @ToplevelVariableNoImport.addInt32(ptr null, ptr %2)
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %"ToplevelVariableNoImport.$eta_257_0")
  ret ptr %8
}

define internal ptr @"ToplevelVariableNoImport.$const_curry_203"(ptr %0, ptr %"ToplevelVariableNoImport.$a_204_0", ptr %"ToplevelVariableNoImport.$__205_0") {
  ret ptr %"ToplevelVariableNoImport.$a_204_0"
}

define internal ptr @ToplevelVariableNoImport.main(ptr %0, ptr %"ToplevelVariableNoImport.$$__259_0") {
  %2 = load ptr, ptr @ToplevelVariableNoImport.comp, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$fun_266"(ptr null, ptr %2)
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %4, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_0, align 8
  %5 = call ptr @ToplevelVariableNoImport.constId(ptr null, ptr %4)
  %6 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %7 = call ptr @ToplevelVariableNoImport.addOne(ptr null, ptr %6)
  %8 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %5, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, ptr %7)
  %13 = call ptr @ToplevelVariableNoImport.printInt32(ptr null, ptr %12)
  ret ptr %13
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_287"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$malgo_add_int32_t_curry_176"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @ToplevelVariableNoImport.malgo_add_int32_t(ptr %0, i32 %"ToplevelVariableNoImport.$p_174_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"ToplevelVariableNoImport.$p_174_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_287", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_288"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"ToplevelVariableNoImport.addInt32#"(ptr %0, i32 %"ToplevelVariableNoImport.$x_223_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"ToplevelVariableNoImport.$x_223_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_288", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.malgo_int32_t_to_string(ptr %0, i32 %"ToplevelVariableNoImport.$p_181_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"ToplevelVariableNoImport.$p_181_0")
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr %0, i32 %"ToplevelVariableNoImport.$x_229_0", i32 %"ToplevelVariableNoImport.$y_230_0") {
  %2 = call ptr @ToplevelVariableNoImport.malgo_add_int32_t(ptr null, i32 %"ToplevelVariableNoImport.$x_229_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"ToplevelVariableNoImport.$y_230_0")
  ret i32 %7
}

define internal ptr @ToplevelVariableNoImport.printString(ptr %0, ptr %"ToplevelVariableNoImport.$string#_191_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"ToplevelVariableNoImport.$string#_191_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.String#_0"
  ]

"switch_branch_ToplevelVariableNoImport.String#_0": ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"ToplevelVariableNoImport.$string#_191_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @"ToplevelVariableNoImport.printString#"(ptr null, ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @ToplevelVariableNoImport.Just(ptr %0, ptr %"ToplevelVariableNoImport.$p_172_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"ToplevelVariableNoImport.$p_172_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.String#"(ptr %0, ptr %"ToplevelVariableNoImport.$p_169_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"ToplevelVariableNoImport.$p_169_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.malgo_print_string(ptr %0, ptr %"ToplevelVariableNoImport.$p_180_0") {
  %2 = call ptr @malgo_print_string(ptr %"ToplevelVariableNoImport.$p_180_0")
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.$addInt32_curry_244"(ptr %0, ptr %"ToplevelVariableNoImport.$int32#_245_0", ptr %"ToplevelVariableNoImport.$int32#_246_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$int32#_245_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_245_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$int32#_246_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_1": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_246_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"ToplevelVariableNoImport.addInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"ToplevelVariableNoImport.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @ToplevelVariableNoImport.toStringInt32(ptr %0, ptr %"ToplevelVariableNoImport.$int32#_184_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$int32#_184_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_184_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @"ToplevelVariableNoImport.toStringInt32#"(ptr null, i32 %6)
  %8 = call ptr @"ToplevelVariableNoImport.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @ToplevelVariableNoImport.identity(ptr %0, ptr %"ToplevelVariableNoImport.$x_200_0") {
  ret ptr %"ToplevelVariableNoImport.$x_200_0"
}

define internal ptr @"ToplevelVariableNoImport.toStringInt32#"(ptr %0, i32 %"ToplevelVariableNoImport.$x_182_0") {
  %2 = call ptr @ToplevelVariableNoImport.malgo_int32_t_to_string(ptr null, i32 %"ToplevelVariableNoImport.$x_182_0")
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.printInt32(ptr %0, ptr %"ToplevelVariableNoImport.$i_194_0") {
  %2 = call ptr @ToplevelVariableNoImport.toStringInt32(ptr null, ptr %"ToplevelVariableNoImport.$i_194_0")
  %3 = call ptr @ToplevelVariableNoImport.printString(ptr null, ptr %2)
  ret ptr %3
}

define internal i32 @"ToplevelVariableNoImport.$malgo_add_int32_t_curry_176"(ptr %0, i32 %"ToplevelVariableNoImport.$p_177_0", i32 %"ToplevelVariableNoImport.$p_178_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"ToplevelVariableNoImport.$p_177_0", i32 %"ToplevelVariableNoImport.$p_178_0")
  ret i32 %2
}

define internal ptr @ToplevelVariableNoImport.Nothing(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_ToplevelVariableNoImport()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @ToplevelVariableNoImport.main(ptr null, ptr %2)
  ret i32 0
}

define internal ptr @"ToplevelVariableNoImport.#fun_closure_293"(ptr %0, ptr %1) {
  %3 = getelementptr { i8, <8 x i8> }, ptr %1, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %switch_branch_ToplevelVariableNoImport.Nothing_0
    i8 1, label %switch_branch_ToplevelVariableNoImport.Just_0
  ]

switch_branch_ToplevelVariableNoImport.Nothing_0: ; preds = %2
  %5 = getelementptr { i8, {} }, ptr %1, i32 0, i32 1
  %6 = call ptr @ToplevelVariableNoImport.Nothing(ptr null)
  ret ptr %6

switch_branch_ToplevelVariableNoImport.Just_0:    ; preds = %2
  %7 = getelementptr { i8, { ptr } }, ptr %1, i32 0, i32 1
  %8 = getelementptr { ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr @ToplevelVariableNoImport.Nothing(ptr null)
  ret ptr %10

switch_default_0:                                 ; preds = %2
  unreachable
}

define internal void @malgo_load_ToplevelVariableNoImport() {
  %1 = call ptr @"ToplevelVariableNoImport.Int32#"(ptr null, i32 1)
  store ptr %1, ptr @ToplevelVariableNoImport.one, align 8
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %fun_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({}, ptr null, i32 1) to i64))
  %fun_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %fun_capture_0, ptr %fun_capture_1, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#fun_closure_293", ptr %fun_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, {} }, ptr %3, i32 0, i32 0
  store i8 0, ptr %4, align 1
  %5 = call ptr @ToplevelVariableNoImport.Just(ptr null, ptr %3)
  %6 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = call ptr %9(ptr %7, ptr %5)
  store ptr %10, ptr @ToplevelVariableNoImport.comp, align 8
  ret void
}
