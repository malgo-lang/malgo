; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TuplePattern.ll'
source_filename = "test/testcases/malgo/TuplePattern.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str54 = unnamed_addr constant [1 x i8] zeroinitializer
@str74 = unnamed_addr constant [10 x i8] c"no branch\00"
@str252 = unnamed_addr constant [2 x i8] c"A\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_60"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"runtime/malgo/Prelude.mlg.|>"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$x_699_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$x_699_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_60", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.String#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString#"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$x_2399_0")
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Builtin.mlg.$string#_2401_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_251"(ptr nocapture nofree readonly %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_Tuple#_0":
  %2 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = load i8, ptr %3, align 1
  switch i8 %4, label %switch_default_0 [
    i8 0, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0"
    i8 1, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0"
    i8 2, label %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0"
  ]

common.ret:                                       ; preds = %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0", %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0"
  %common.ret.op = phi ptr [ %12, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0" ], [ %13, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0" ], [ %14, %"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0" ]
  ret ptr %common.ret.op

"switch_branch_test/testcases/malgo/TuplePattern.mlg.A_0": ; preds = %"switch_branch_Tuple#_0"
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %printString_0 = load ptr, ptr %0, align 8
  %5 = load ptr, ptr %"String#_0", align 8
  %6 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr nonnull @str252)
  %9 = load ptr, ptr %printString_0, align 8
  %10 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr %11(ptr %9, ptr %8)
  br label %common.ret

"switch_branch_test/testcases/malgo/TuplePattern.mlg.B_0": ; preds = %"switch_branch_Tuple#_0"
  %13 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  br label %common.ret

"switch_branch_test/testcases/malgo/TuplePattern.mlg.C_0": ; preds = %"switch_branch_Tuple#_0"
  %14 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  br label %common.ret

switch_default_0:                                 ; preds = %"switch_branch_Tuple#_0"
  unreachable
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast", ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 1
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.|>", ptr %"|>_func_0.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %12, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#let_closure_60", ptr %let_func_0.i.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i, align 8
  store ptr %14, ptr %fun_capture_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %"String#_0.i" = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %15, ptr %"String#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %13, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/TuplePattern.mlg.#fun_closure_251", ptr %fun_func_0.i, align 8
  %16 = load ptr, ptr %12, align 8
  %17 = load ptr, ptr %let_func_0.i.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %13)
  ret i32 0
}
