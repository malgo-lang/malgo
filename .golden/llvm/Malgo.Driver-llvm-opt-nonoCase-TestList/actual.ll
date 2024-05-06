; ModuleID = '.malgo-work/test/testcases/malgo/TestList.ll'
source_filename = "test/testcases/malgo/TestList.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str244 = unnamed_addr constant [1 x i8] zeroinitializer
@str264 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %3, align 4
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

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$p_2155_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2160_0") {
  %2 = tail call ptr @malgo_exit_failure(ptr %"runtime/malgo/Builtin.mlg.$p_2160_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_newline"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2161_0") {
  %2 = tail call ptr @malgo_newline(ptr %"runtime/malgo/Builtin.mlg.$p_2161_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$x_2179_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Builtin.mlg.$x_2179_0")
  ret ptr %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.toStringInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Builtin.mlg.$int32#_2181_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Builtin.mlg.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
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

define internal ptr @"runtime/malgo/Builtin.mlg.newline"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"runtime/malgo/Builtin.mlg.$__2420_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0, align 8
  %4 = tail call ptr @malgo_newline(ptr noundef nonnull %2)
  ret ptr %4
}

define internal ptr @"runtime/malgo/Builtin.mlg.exitFailure"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"runtime/malgo/Builtin.mlg.$__3571_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_exit_failure_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0, align 8
  %4 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %2)
  ret ptr %4
}

define internal noundef ptr @"test/testcases/malgo/TestList.mlg.#let_closure_249"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.Cons"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$p_691_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$p_691_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestList.mlg.#let_closure_249", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %newline_func_0 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_newline_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0.i, align 8
  %12 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  ret ptr %12
}

define internal ptr @"runtime/malgo/Prelude.mlg.head"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"runtime/malgo/Prelude.mlg.$cons_916_0") {
  %2 = load i8, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %7, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %9, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %exitFailure_func_0 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.exitFailure", ptr %exitFailure_func_0, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_exit_failure_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0.i, align 8
  %7 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %5)
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %1
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %"runtime/malgo/Prelude.mlg.$cons_916_0", i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  br label %common.ret
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
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 1, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestList.mlg.#let_closure_249", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1, i32 0
  store i32 2, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %Cons_func_1.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_1.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %15, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestList.mlg.#let_closure_249", ptr %let_func_0.i2.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %"Int32#_func_2.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_2.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1, i32 0
  store i32 3, ptr %18, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %Cons_func_2.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_2.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %20, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestList.mlg.#let_closure_249", ptr %let_func_0.i4.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = load ptr, ptr %20, align 8
  %23 = load ptr, ptr %let_func_0.i4.i, align 8
  %24 = tail call ptr %23(ptr %22, ptr nonnull %21)
  %25 = load ptr, ptr %15, align 8
  %26 = load ptr, ptr %let_func_0.i2.i, align 8
  %27 = tail call ptr %26(ptr %25, ptr %24)
  %28 = load ptr, ptr %10, align 8
  %29 = load ptr, ptr %let_func_0.i.i, align 8
  %30 = tail call ptr %29(ptr %28, ptr %27)
  %31 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %31, align 1
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %32, align 8
  %head_func_0.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.head", ptr %head_func_0.i, align 8
  %33 = load i8, ptr %30, align 1
  %switch.i.i = icmp eq i8 %33, 0
  br i1 %switch.i.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i.i": ; preds = %1
  %34 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %34, align 1
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %exitFailure_func_0.i.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.exitFailure", ptr %exitFailure_func_0.i.i, align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %36, align 1
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %37, align 8
  %malgo_exit_failure_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_exit_failure", ptr %malgo_exit_failure_func_0.i.i.i, align 8
  %38 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %36)
  br label %"test/testcases/malgo/TestList.mlg.main.exit"

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i.i": ; preds = %1
  %39 = getelementptr { i8, { ptr, ptr } }, ptr %30, i64 0, i32 1
  %40 = load ptr, ptr %39, align 8
  br label %"test/testcases/malgo/TestList.mlg.main.exit"

"test/testcases/malgo/TestList.mlg.main.exit":    ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i.i", %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i.i"
  %common.ret.op.i.i = phi ptr [ %38, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0.i.i" ], [ %40, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0.i.i" ]
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %41, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32", ptr %toStringInt32_func_0.i, align 8
  %42 = getelementptr { i8, { i32 } }, ptr %common.ret.op.i.i, i64 0, i32 1
  %43 = load i32, ptr %42, align 4
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %44, align 8
  %"toStringInt32#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringInt32#", ptr %"toStringInt32#_func_0.i.i", align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %45, align 8
  %malgo_int32_t_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %45, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int32_t_to_string", ptr %malgo_int32_t_to_string_func_0.i.i.i, align 8
  %46 = tail call ptr @malgo_int32_t_to_string(i32 %43)
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %47, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %47, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i.i", align 8
  %48 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %48, align 1
  %49 = getelementptr { i8, { ptr } }, ptr %48, i64 0, i32 1, i32 0
  store ptr %46, ptr %49, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %50, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %50, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %51, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i.i, align 8
  %52 = load ptr, ptr %49, align 8
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %53, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %54 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %54, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %54, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %55 = tail call ptr @malgo_print_string(ptr %52)
  %56 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %56, align 1
  %57 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %57, align 8
  %newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.newline", ptr %newline_func_0.i.i, align 8
  %58 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %58, align 1
  %59 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %59, align 8
  %malgo_newline_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %59, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_newline", ptr %malgo_newline_func_0.i.i.i, align 8
  %60 = tail call ptr @malgo_newline(ptr noundef nonnull %58)
  ret i32 0
}
