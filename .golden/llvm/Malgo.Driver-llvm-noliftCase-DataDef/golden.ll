; ModuleID = './test/testcases/malgo/DataDef.mlg'
source_filename = "./test/testcases/malgo/DataDef.mlg"

declare void @GC_init()

declare ptr @malgo_int64_t_to_string(i64)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_malloc(i64)

define internal ptr @"DataDef.Int#"(ptr %0, i64 %"DataDef.$p_18_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"DataDef.$p_18_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @DataDef.Nil(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal ptr @"DataDef.#let_closure_50"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr, ptr } }, ptr null, i32 1) to i64))
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 0
  store i8 1, ptr %4, align 1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 1, i32 0
  store ptr %p_0, ptr %5, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %3, i32 0, i32 1, i32 1
  store ptr %1, ptr %6, align 8
  ret ptr %3
}

define internal ptr @DataDef.Cons(ptr %0, ptr %"DataDef.$p_21_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"DataDef.$p_21_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"DataDef.#let_closure_50", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @DataDef.malgo_int64_t_to_string(ptr %0, i64 %"DataDef.$p_29_0") {
  %2 = call ptr @malgo_int64_t_to_string(i64 %"DataDef.$p_29_0")
  ret ptr %2
}

define internal ptr @DataDef.malgo_print_string(ptr %0, ptr %"DataDef.$p_30_0") {
  %2 = call ptr @malgo_print_string(ptr %"DataDef.$p_30_0")
  ret ptr %2
}

define internal ptr @DataDef.main(ptr %0, ptr %"DataDef.$$__31_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = getelementptr { i8, <8 x i8> }, ptr %2, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  switch i8 %6, label %switch_default_0 [
    i8 0, label %"switch_branch_DataDef.Int#_0"
  ]

"switch_branch_DataDef.Int#_0":                   ; preds = %1
  %7 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1
  %8 = getelementptr { i64 }, ptr %7, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  %10 = call ptr @malgo_int64_t_to_string(i64 %9)
  %11 = call ptr @malgo_print_string(ptr %10)
  ret ptr %11

switch_default_0:                                 ; preds = %1
  unreachable
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @koriel_load_DataDef()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @DataDef.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @koriel_load_DataDef() {
  ret void
}
