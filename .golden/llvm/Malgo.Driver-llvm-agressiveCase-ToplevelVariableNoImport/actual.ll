; ModuleID = 'test/testcases/malgo/ToplevelVariableNoImport.mlg'
source_filename = "test/testcases/malgo/ToplevelVariableNoImport.mlg"

@ToplevelVariableNoImport.one = global ptr undef
@ToplevelVariableNoImport.comp = global ptr undef
@str544 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init()

declare i32 @malgo_add_int32_t(i32, i32)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_int32_t_to_string(i32)

define internal ptr @"ToplevelVariableNoImport.printString#"(ptr %0, ptr %"ToplevelVariableNoImport.$x_189_0") {
  %2 = call ptr @malgo_print_string(ptr %"ToplevelVariableNoImport.$x_189_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64)

define internal ptr @"ToplevelVariableNoImport.#let_closure_540"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$raw_let_533"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal ptr @ToplevelVariableNoImport.constId(ptr %0, ptr %"ToplevelVariableNoImport.$eta_210_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %2, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_540", ptr %let_func_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %3, i32 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = call ptr %7(ptr %5, ptr %"ToplevelVariableNoImport.$eta_210_0")
  ret ptr %8
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_541"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"ToplevelVariableNoImport.$raw_let_534"(ptr null, ptr %"int32#_0", ptr %1)
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
  store ptr @"ToplevelVariableNoImport.#let_closure_541", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_542"(ptr %0, ptr %1) {
  %a_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %a_0 = load ptr, ptr %a_addr_0, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$raw_let_532"(ptr null, ptr %a_0, ptr %1)
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
  store ptr @"ToplevelVariableNoImport.#let_closure_542", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.Int32#"(ptr %0, i32 %"ToplevelVariableNoImport.$p_167_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"ToplevelVariableNoImport.$p_167_0", ptr %4, align 4
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_543"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$raw_let_535"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @ToplevelVariableNoImport.addOne(ptr %0, ptr %"ToplevelVariableNoImport.$eta_257_0") {
  %2 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %3 = getelementptr { i8, <4 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_1 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %1
  %5 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1
  %6 = getelementptr { i32 }, ptr %5, i32 0, i32 0
  %7 = load i32, ptr %6, align 4
  %8 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$eta_257_0", i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_1": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  %10 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$eta_257_0", i32 0, i32 1
  %11 = getelementptr { i32 }, ptr %10, i32 0, i32 0
  %12 = load i32, ptr %11, align 4
  %13 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %7, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %13, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_543", ptr %let_func_0, align 8
  %14 = call i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr null, i32 %7, i32 %12)
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %16 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 0
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %15, i32 0, i32 1, i32 0
  store i32 %14, ptr %17, align 4
  ret ptr %15

switch_default_0:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_545"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$raw_let_536"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_546"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"ToplevelVariableNoImport.#let_closure_547"(ptr %0, ptr %1) {
  %cast_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %cast_0 = load ptr, ptr %cast_addr_0, align 8
  %3 = call ptr @"ToplevelVariableNoImport.$raw_let_538"(ptr null, ptr %cast_0, ptr %1)
  ret ptr %3
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_548"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @ToplevelVariableNoImport.main(ptr %0, ptr %"ToplevelVariableNoImport.$$__259_0") {
  %2 = load ptr, ptr @ToplevelVariableNoImport.comp, align 8
  %3 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_9 [
    i8 0, label %switch_branch_ToplevelVariableNoImport.Nothing_0
    i8 1, label %switch_branch_ToplevelVariableNoImport.Just_0
  ]

switch_branch_ToplevelVariableNoImport.Nothing_0: ; preds = %1
  %5 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %6 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 0
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1, i32 0
  store ptr @str544, ptr %7, align 8
  %8 = getelementptr { i8, <8 x i8> }, ptr %5, i32 0, i32 0
  %9 = load i8, ptr %8, align 1
  switch i8 %9, label %switch_default_4 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.String#_0"
  ]

"switch_branch_ToplevelVariableNoImport.String#_0": ; preds = %switch_branch_ToplevelVariableNoImport.Nothing_0
  %10 = getelementptr { i8, { ptr } }, ptr %5, i32 0, i32 1
  %11 = getelementptr { ptr }, ptr %10, i32 0, i32 0
  %12 = load ptr, ptr %11, align 8
  %13 = call ptr @malgo_print_string(ptr %12)
  %14 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 0
  store ptr null, ptr %const_capture_0, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %14, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_0, align 8
  %15 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 0
  store ptr null, ptr %identity_capture_0, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %15, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_0, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %15, ptr %cast_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_545", ptr %let_func_0, align 8
  %17 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 0
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %16, i32 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = call ptr %20(ptr %18, ptr %14)
  %22 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %23 = getelementptr { i8, <4 x i8> }, ptr %22, i32 0, i32 0
  %24 = load i8, ptr %23, align 1
  switch i8 %24, label %switch_default_3 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %"switch_branch_ToplevelVariableNoImport.String#_0"
  %25 = getelementptr { i8, { i32 } }, ptr %22, i32 0, i32 1
  %26 = getelementptr { i32 }, ptr %25, i32 0, i32 0
  %27 = load i32, ptr %26, align 4
  %28 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %29 = getelementptr { i8, <4 x i8> }, ptr %28, i32 0, i32 0
  %30 = load i8, ptr %29, align 1
  switch i8 %30, label %switch_default_2 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_1": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  %31 = getelementptr { i8, { i32 } }, ptr %28, i32 0, i32 1
  %32 = getelementptr { i32 }, ptr %31, i32 0, i32 0
  %33 = load i32, ptr %32, align 4
  %34 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_2, i32 0, i32 0
  store i32 %27, ptr %p_0, align 4
  %let_capture_3 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  store ptr %let_capture_2, ptr %let_capture_3, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_546", ptr %let_func_1, align 8
  %35 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 0
  %36 = load ptr, ptr %35, align 8
  %37 = getelementptr { ptr, ptr }, ptr %34, i32 0, i32 1
  %38 = load ptr, ptr %37, align 8
  %39 = call i32 %38(ptr %36, i32 %33)
  %40 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %41 = getelementptr { i8, { i32 } }, ptr %40, i32 0, i32 0
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { i32 } }, ptr %40, i32 0, i32 1, i32 0
  store i32 %39, ptr %42, align 4
  %43 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 0
  %44 = load ptr, ptr %43, align 8
  %45 = getelementptr { ptr, ptr }, ptr %21, i32 0, i32 1
  %46 = load ptr, ptr %45, align 8
  %47 = call ptr %46(ptr %44, ptr %40)
  %48 = getelementptr { i8, <4 x i8> }, ptr %47, i32 0, i32 0
  %49 = load i8, ptr %48, align 1
  switch i8 %49, label %switch_default_1 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_2"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_2": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  %50 = getelementptr { i8, { i32 } }, ptr %47, i32 0, i32 1
  %51 = getelementptr { i32 }, ptr %50, i32 0, i32 0
  %52 = load i32, ptr %51, align 4
  %53 = call ptr @malgo_int32_t_to_string(i32 %52)
  %54 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %55 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 0
  store i8 0, ptr %55, align 1
  %56 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 1, i32 0
  store ptr %53, ptr %56, align 8
  %57 = getelementptr { i8, <8 x i8> }, ptr %54, i32 0, i32 0
  %58 = load i8, ptr %57, align 1
  switch i8 %58, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.String#_1"
  ]

"switch_branch_ToplevelVariableNoImport.String#_1": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_2"
  %59 = getelementptr { i8, { ptr } }, ptr %54, i32 0, i32 1
  %60 = getelementptr { ptr }, ptr %59, i32 0, i32 0
  %61 = load ptr, ptr %60, align 8
  %62 = call ptr @malgo_print_string(ptr %61)
  ret ptr %62

switch_default_0:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_2"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  unreachable

switch_default_2:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  unreachable

switch_default_3:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.String#_0"
  unreachable

switch_default_4:                                 ; preds = %switch_branch_ToplevelVariableNoImport.Nothing_0
  unreachable

switch_branch_ToplevelVariableNoImport.Just_0:    ; preds = %1
  %63 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1
  %64 = getelementptr { ptr }, ptr %63, i32 0, i32 0
  %65 = load ptr, ptr %64, align 8
  %66 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %const_capture_1 = getelementptr { ptr, ptr }, ptr %66, i32 0, i32 0
  store ptr null, ptr %const_capture_1, align 8
  %const_func_1 = getelementptr { ptr, ptr }, ptr %66, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_1, align 8
  %67 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %identity_capture_1 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 0
  store ptr null, ptr %identity_capture_1, align 8
  %identity_func_1 = getelementptr { ptr, ptr }, ptr %67, i32 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_1, align 8
  %68 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %cast_1 = getelementptr { ptr }, ptr %let_capture_4, i32 0, i32 0
  store ptr %67, ptr %cast_1, align 8
  %let_capture_5 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 0
  store ptr %let_capture_4, ptr %let_capture_5, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_547", ptr %let_func_2, align 8
  %69 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 0
  %70 = load ptr, ptr %69, align 8
  %71 = getelementptr { ptr, ptr }, ptr %68, i32 0, i32 1
  %72 = load ptr, ptr %71, align 8
  %73 = call ptr %72(ptr %70, ptr %66)
  %74 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %75 = getelementptr { i8, <4 x i8> }, ptr %74, i32 0, i32 0
  %76 = load i8, ptr %75, align 1
  switch i8 %76, label %switch_default_8 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_3"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_3": ; preds = %switch_branch_ToplevelVariableNoImport.Just_0
  %77 = getelementptr { i8, { i32 } }, ptr %74, i32 0, i32 1
  %78 = getelementptr { i32 }, ptr %77, i32 0, i32 0
  %79 = load i32, ptr %78, align 4
  %80 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %81 = getelementptr { i8, <4 x i8> }, ptr %80, i32 0, i32 0
  %82 = load i8, ptr %81, align 1
  switch i8 %82, label %switch_default_7 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_4"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_4": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_3"
  %83 = getelementptr { i8, { i32 } }, ptr %80, i32 0, i32 1
  %84 = getelementptr { i32 }, ptr %83, i32 0, i32 0
  %85 = load i32, ptr %84, align 4
  %86 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_1 = getelementptr { i32 }, ptr %let_capture_6, i32 0, i32 0
  store i32 %79, ptr %p_1, align 4
  %let_capture_7 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 0
  store ptr %let_capture_6, ptr %let_capture_7, align 8
  %let_func_3 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_548", ptr %let_func_3, align 8
  %87 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 0
  %88 = load ptr, ptr %87, align 8
  %89 = getelementptr { ptr, ptr }, ptr %86, i32 0, i32 1
  %90 = load ptr, ptr %89, align 8
  %91 = call i32 %90(ptr %88, i32 %85)
  %92 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %93 = getelementptr { i8, { i32 } }, ptr %92, i32 0, i32 0
  store i8 0, ptr %93, align 1
  %94 = getelementptr { i8, { i32 } }, ptr %92, i32 0, i32 1, i32 0
  store i32 %91, ptr %94, align 4
  %95 = getelementptr { ptr, ptr }, ptr %73, i32 0, i32 0
  %96 = load ptr, ptr %95, align 8
  %97 = getelementptr { ptr, ptr }, ptr %73, i32 0, i32 1
  %98 = load ptr, ptr %97, align 8
  %99 = call ptr %98(ptr %96, ptr %92)
  %100 = getelementptr { i8, <4 x i8> }, ptr %99, i32 0, i32 0
  %101 = load i8, ptr %100, align 1
  switch i8 %101, label %switch_default_6 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_5"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_5": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_4"
  %102 = getelementptr { i8, { i32 } }, ptr %99, i32 0, i32 1
  %103 = getelementptr { i32 }, ptr %102, i32 0, i32 0
  %104 = load i32, ptr %103, align 4
  %105 = call ptr @malgo_int32_t_to_string(i32 %104)
  %106 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %107 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 0
  store i8 0, ptr %107, align 1
  %108 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 1, i32 0
  store ptr %105, ptr %108, align 8
  %109 = getelementptr { i8, <8 x i8> }, ptr %106, i32 0, i32 0
  %110 = load i8, ptr %109, align 1
  switch i8 %110, label %switch_default_5 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.String#_2"
  ]

"switch_branch_ToplevelVariableNoImport.String#_2": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_5"
  %111 = getelementptr { i8, { ptr } }, ptr %106, i32 0, i32 1
  %112 = getelementptr { ptr }, ptr %111, i32 0, i32 0
  %113 = load ptr, ptr %112, align 8
  %114 = call ptr @malgo_print_string(ptr %113)
  ret ptr %114

switch_default_5:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_5"
  unreachable

switch_default_6:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_4"
  unreachable

switch_default_7:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_3"
  unreachable

switch_default_8:                                 ; preds = %switch_branch_ToplevelVariableNoImport.Just_0
  unreachable

switch_default_9:                                 ; preds = %1
  unreachable
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_549"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
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
  store ptr @"ToplevelVariableNoImport.#let_closure_549", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_550"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
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
  store ptr @"ToplevelVariableNoImport.#let_closure_550", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.malgo_int32_t_to_string(ptr %0, i32 %"ToplevelVariableNoImport.$p_181_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"ToplevelVariableNoImport.$p_181_0")
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_532"(ptr %0, ptr %"ToplevelVariableNoImport.$a_201_0", ptr %"ToplevelVariableNoImport.$__202_0") {
  ret ptr %"ToplevelVariableNoImport.$a_201_0"
}

define internal i32 @"ToplevelVariableNoImport.$addInt32#_curry_228"(ptr %0, i32 %"ToplevelVariableNoImport.$x_229_0", i32 %"ToplevelVariableNoImport.$y_230_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"ToplevelVariableNoImport.$x_229_0", i32 %"ToplevelVariableNoImport.$y_230_0")
  ret i32 %2
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
  %7 = call ptr @malgo_print_string(ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_533"(ptr %0, ptr %"ToplevelVariableNoImport.$cast_207_0", ptr %"ToplevelVariableNoImport.$__321_0") {
  ret ptr %"ToplevelVariableNoImport.$cast_207_0"
}

define internal ptr @ToplevelVariableNoImport.Just(ptr %0, ptr %"ToplevelVariableNoImport.$p_172_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"ToplevelVariableNoImport.$p_172_0", ptr %4, align 8
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_551"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"ToplevelVariableNoImport.$malgo_add_int32_t_curry_176"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_534"(ptr %0, ptr %"ToplevelVariableNoImport.$int32#_235_0", ptr %"ToplevelVariableNoImport.$int32#_236_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$int32#_235_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_235_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$int32#_236_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_1"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_1": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_236_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %6, ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_551", ptr %let_func_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %19 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 0
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %18, i32 0, i32 1, i32 0
  store i32 %17, ptr %20, align 4
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
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
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @ToplevelVariableNoImport.identity(ptr %0, ptr %"ToplevelVariableNoImport.$x_200_0") {
  ret ptr %"ToplevelVariableNoImport.$x_200_0"
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_536"(ptr %0, ptr %"ToplevelVariableNoImport.$cast_383_0", ptr %"ToplevelVariableNoImport.$__396_0") {
  ret ptr %"ToplevelVariableNoImport.$cast_383_0"
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_537"(ptr %0, ptr %"ToplevelVariableNoImport.$cast_383_0", ptr %"ToplevelVariableNoImport.$__396_0") {
  ret ptr %"ToplevelVariableNoImport.$cast_383_0"
}

define internal i32 @"ToplevelVariableNoImport.$raw_let_535"(ptr %0, i32 %"ToplevelVariableNoImport.$p_359_0", i32 %"ToplevelVariableNoImport.$y_368_0") {
  %2 = call ptr @ToplevelVariableNoImport.malgo_add_int32_t(ptr null, i32 %"ToplevelVariableNoImport.$p_359_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"ToplevelVariableNoImport.$y_368_0")
  ret i32 %7
}

define internal ptr @"ToplevelVariableNoImport.toStringInt32#"(ptr %0, i32 %"ToplevelVariableNoImport.$x_182_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"ToplevelVariableNoImport.$x_182_0")
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.printInt32(ptr %0, ptr %"ToplevelVariableNoImport.$i_194_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"ToplevelVariableNoImport.$i_194_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  ]

"switch_branch_ToplevelVariableNoImport.Int32#_0": ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$i_194_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %9 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 0
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, <8 x i8> }, ptr %8, i32 0, i32 0
  %12 = load i8, ptr %11, align 1
  switch i8 %12, label %switch_default_0 [
    i8 0, label %"switch_branch_ToplevelVariableNoImport.String#_0"
  ]

"switch_branch_ToplevelVariableNoImport.String#_0": ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  %13 = getelementptr { i8, { ptr } }, ptr %8, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_print_string(ptr %15)
  ret ptr %16

switch_default_0:                                 ; preds = %"switch_branch_ToplevelVariableNoImport.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_538"(ptr %0, ptr %"ToplevelVariableNoImport.$cast_383_0", ptr %"ToplevelVariableNoImport.$__408_0") {
  ret ptr %"ToplevelVariableNoImport.$cast_383_0"
}

define internal i32 @"ToplevelVariableNoImport.$malgo_add_int32_t_curry_176"(ptr %0, i32 %"ToplevelVariableNoImport.$p_177_0", i32 %"ToplevelVariableNoImport.$p_178_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"ToplevelVariableNoImport.$p_177_0", i32 %"ToplevelVariableNoImport.$p_178_0")
  ret i32 %2
}

define internal ptr @"ToplevelVariableNoImport.$raw_let_539"(ptr %0, ptr %"ToplevelVariableNoImport.$cast_383_0", ptr %"ToplevelVariableNoImport.$__420_0") {
  ret ptr %"ToplevelVariableNoImport.$cast_383_0"
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

define internal void @malgo_load_ToplevelVariableNoImport() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %1, i32 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %1, ptr @ToplevelVariableNoImport.one, align 8
  %4 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %5 = getelementptr { i8, {} }, ptr %4, i32 0, i32 0
  store i8 0, ptr %5, align 1
  %6 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %7 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 0
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1, i32 0
  store ptr %4, ptr %8, align 8
  %9 = getelementptr { i8, <8 x i8> }, ptr %6, i32 0, i32 0
  %10 = load i8, ptr %9, align 1
  switch i8 %10, label %switch_default_0 [
    i8 0, label %switch_branch_ToplevelVariableNoImport.Nothing_0
    i8 1, label %switch_branch_ToplevelVariableNoImport.Just_0
  ]

switch_branch_ToplevelVariableNoImport.Nothing_0: ; preds = %0
  %11 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %12 = getelementptr { i8, {} }, ptr %11, i32 0, i32 0
  store i8 0, ptr %12, align 1
  store ptr %11, ptr @ToplevelVariableNoImport.comp, align 8
  ret void

switch_branch_ToplevelVariableNoImport.Just_0:    ; preds = %0
  %13 = getelementptr { i8, { ptr } }, ptr %6, i32 0, i32 1
  %14 = getelementptr { ptr }, ptr %13, i32 0, i32 0
  %15 = load ptr, ptr %14, align 8
  %16 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %17 = getelementptr { i8, {} }, ptr %16, i32 0, i32 0
  store i8 0, ptr %17, align 1
  store ptr %16, ptr @ToplevelVariableNoImport.comp, align 8
  ret void

switch_default_0:                                 ; preds = %0
  unreachable
}
