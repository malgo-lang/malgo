; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestEither.ll'
source_filename = "test/testcases/malgo/TestEither.mlg"

@str4995 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str5076 = unnamed_addr constant [6 x i8] c"error\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_716_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal noundef ptr @"test/testcases/malgo/TestEither.mlg.Right"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/TestEither.mlg.$p_251_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"test/testcases/malgo/TestEither.mlg.$p_251_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_723_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5006"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5018"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal i32 @"test/testcases/malgo/TestEither.mlg.#let_closure_5055"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5070"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_5071"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str4995, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str4995)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_newline(ptr noundef nonnull %8)
  br label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4672.exit"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i": ; preds = %2
  %10 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = tail call ptr @malgo_int32_t_to_string(i32 %13)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %14, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  br label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4672.exit"

"test/testcases/malgo/TestEither.mlg.$raw_fun_4672.exit": ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"
  %common.ret.op.i = phi ptr [ %9, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_0.i" ], [ %17, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i" ]
  ret ptr %common.ret.op.i
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5099"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5100"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %cast_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5101"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_5102"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4733.exit", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i": ; preds = %2
  %4 = getelementptr { i8, { i32 } }, ptr %.val1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = tail call ptr @malgo_int32_t_to_string(i32 %5)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %9, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5018", ptr %let_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  br label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4733.exit"

"test/testcases/malgo/TestEither.mlg.$raw_fun_4733.exit": ; preds = %2, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"
  %d_0.i.sink.i = phi ptr [ %d_0.i.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i" ], [ %.val1, %2 ]
  %11 = getelementptr { i8, { ptr } }, ptr %d_0.i.sink.i, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr @malgo_print_string(ptr %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_newline(ptr noundef nonnull %15)
  ret ptr %16
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5105"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_5106"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4738.exit", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i": ; preds = %2
  %4 = getelementptr { i8, { i32 } }, ptr %.val1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = tail call ptr @malgo_int32_t_to_string(i32 %5)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %6, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %9, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5006", ptr %let_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  br label %"test/testcases/malgo/TestEither.mlg.$raw_fun_4738.exit"

"test/testcases/malgo/TestEither.mlg.$raw_fun_4738.exit": ; preds = %2, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i"
  %d_0.i.sink.i = phi ptr [ %d_0.i.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_0.i" ], [ %.val1, %2 ]
  %11 = getelementptr { i8, { ptr } }, ptr %d_0.i.sink.i, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr @malgo_print_string(ptr %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_newline(ptr noundef nonnull %15)
  ret ptr %16
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5070", ptr %let_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %8, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#fun_closure_5071", ptr %fun_func_0.i, align 8
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %let_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i32 } }, ptr %12, i64 0, i32 1, i32 0
  store i32 1, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %12, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = load i32, ptr %13, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %18, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %19, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5055", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %20 = tail call i32 @malgo_add_int32_t(i32 %x_0.i.i, i32 noundef 1)
  %21 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { i32 } }, ptr %21, i64 0, i32 1, i32 0
  store i32 %20, ptr %22, align 4
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %23, align 1
  %24 = getelementptr { i8, { ptr } }, ptr %23, i64 0, i32 1, i32 0
  store ptr %21, ptr %24, align 8
  %.val.i = load i32, ptr %22, align 4
  %25 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %25, ptr %27, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_30.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %26, ptr %let_capture_30.i, align 8
  store ptr %let_capture_30.i, ptr %28, align 8
  %let_func_15.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5099", ptr %let_func_15.i, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %putStr_func_1.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_1.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_30.i, align 8
  %30 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = tail call ptr @malgo_print_string(ptr %31)
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_32.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %32, ptr %let_capture_32.i, align 8
  store ptr %let_capture_32.i, ptr %33, align 8
  %let_func_16.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5100", ptr %let_func_16.i, align 8
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %34, align 8
  %Right_func_1.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.Right", ptr %Right_func_1.i, align 8
  %35 = load ptr, ptr %33, align 8
  %36 = load ptr, ptr %let_func_16.i, align 8
  %37 = tail call ptr %36(ptr %35, ptr nonnull %34)
  %38 = load i8, ptr %14, align 1
  %switch.i = icmp eq i8 %38, 0
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i", label %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i"

"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i": ; preds = %1
  %39 = load ptr, ptr %15, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { ptr } }, ptr %40, i64 0, i32 1, i32 0
  store ptr %39, ptr %41, align 8
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr } }, ptr %42, i64 0, i32 1, i32 0
  store ptr %39, ptr %43, align 8
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_34.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %42, ptr %let_capture_34.i, align 8
  store ptr %let_capture_34.i, ptr %44, align 8
  %let_func_17.i = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5101", ptr %let_func_17.i, align 8
  br label %"test/testcases/malgo/TestEither.mlg.main.exit"

"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i": ; preds = %1
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr @str5076, ptr %46, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %45, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %49, align 1
  %50 = getelementptr { i8, { ptr } }, ptr %49, i64 0, i32 1, i32 0
  store ptr %45, ptr %50, align 8
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_38.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %49, ptr %let_capture_38.i, align 8
  store ptr %let_capture_38.i, ptr %51, align 8
  %let_func_19.i = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestEither.mlg.#let_closure_5105", ptr %let_func_19.i, align 8
  br label %"test/testcases/malgo/TestEither.mlg.main.exit"

"test/testcases/malgo/TestEither.mlg.main.exit":  ; preds = %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i"
  %"test/testcases/malgo/TestEither.mlg.#fun_closure_5106.sink.i" = phi ptr [ @"test/testcases/malgo/TestEither.mlg.#fun_closure_5106", %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i" ], [ @"test/testcases/malgo/TestEither.mlg.#fun_closure_5102", %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i" ]
  %.sink3.i = phi ptr [ %51, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i" ], [ %44, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i" ]
  %let_func_19.sink.i = phi ptr [ %let_func_19.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Right_13.i" ], [ %let_func_17.i, %"switch_branch_test/testcases/malgo/TestEither.mlg.Left_12.i" ]
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_30.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_30.i, ptr %52, align 8
  %fun_func_15.i = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr %"test/testcases/malgo/TestEither.mlg.#fun_closure_5106.sink.i", ptr %fun_func_15.i, align 8
  %53 = load ptr, ptr %.sink3.i, align 8
  %54 = load ptr, ptr %let_func_19.sink.i, align 8
  %55 = tail call ptr %54(ptr %53, ptr nonnull %52)
  ret i32 0
}
