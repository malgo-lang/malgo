; ModuleID = '/workspaces/malgo/.malgo-work/TestEither.ll'
source_filename = "./test/testcases/malgo/TestEither.mlg"

@str3748 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str3768 = unnamed_addr constant [6 x i8] c"error\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Prelude.putStr(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Prelude.$str_723_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_723_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %3
}

define internal ptr @"TestEither.#let_closure_3710"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestEither.#let_closure_3716"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @Prelude.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Prelude.$str_716_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_716_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @"TestEither.#let_closure_3735"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestEither.#let_closure_3736"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %cast_0)
  ret ptr %6
}

define internal i32 @"TestEither.#let_closure_3739"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"TestEither.#let_closure_3762"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestEither.#fun_closure_3763"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_TestEither.Left_0.i, label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Left_0.i:                ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str3748, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3748)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_newline(ptr noundef nonnull %8)
  br label %"TestEither.$raw_fun_3621.exit"

switch_branch_TestEither.Right_0.i:               ; preds = %2
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
  br label %"TestEither.$raw_fun_3621.exit"

"TestEither.$raw_fun_3621.exit":                  ; preds = %switch_branch_TestEither.Left_0.i, %switch_branch_TestEither.Right_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_TestEither.Left_0.i ], [ %17, %switch_branch_TestEither.Right_0.i ]
  ret ptr %common.ret.op.i
}

define internal ptr @"TestEither.#let_closure_3773"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestEither.#fun_closure_3774"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"TestEither.$raw_fun_3637.exit", label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Right_0.i:               ; preds = %2
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
  store ptr @"TestEither.#let_closure_3710", ptr %let_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  br label %"TestEither.$raw_fun_3637.exit"

"TestEither.$raw_fun_3637.exit":                  ; preds = %2, %switch_branch_TestEither.Right_0.i
  %d_0.i.sink.i = phi ptr [ %d_0.i.i, %switch_branch_TestEither.Right_0.i ], [ %.val1, %2 ]
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

define internal ptr @"TestEither.#let_closure_3777"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestEither.#fun_closure_3778"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"TestEither.$raw_fun_3642.exit", label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Right_0.i:               ; preds = %2
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
  store ptr @"TestEither.#let_closure_3716", ptr %let_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  br label %"TestEither.$raw_fun_3642.exit"

"TestEither.$raw_fun_3642.exit":                  ; preds = %2, %switch_branch_TestEither.Right_0.i
  %d_0.i.sink.i = phi ptr [ %d_0.i.i, %switch_branch_TestEither.Right_0.i ], [ %.val1, %2 ]
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

define internal noundef ptr @TestEither.Right(ptr nocapture nofree readnone %0, ptr nofree %"TestEither.$p_251_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"TestEither.$p_251_0", ptr %3, align 8
  ret ptr %2
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
  store ptr @"TestEither.#let_closure_3762", ptr %let_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %8, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_3763", ptr %fun_func_0.i, align 8
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
  store ptr @"TestEither.#let_closure_3739", ptr %let_func_0.i.i, align 8
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
  %25 = load i32, ptr %22, align 4
  %26 = tail call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %26, ptr %28, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %27, ptr %let_capture_0.i2.i, align 8
  store ptr %let_capture_0.i2.i, ptr %29, align 8
  %let_func_0.i3.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @"TestEither.#let_closure_3735", ptr %let_func_0.i3.i, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %putStr_func_0.i.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @Prelude.putStr, ptr %putStr_func_0.i.i, align 8
  %d_0.i.i.i = load ptr, ptr %let_capture_0.i2.i, align 8
  %31 = getelementptr { i8, { ptr } }, ptr %d_0.i.i.i, i64 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = tail call ptr @malgo_print_string(ptr %32)
  %34 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %33, ptr %let_capture_2.i.i, align 8
  store ptr %let_capture_2.i.i, ptr %34, align 8
  %let_func_1.i.i = getelementptr { ptr, ptr }, ptr %34, i64 0, i32 1
  store ptr @"TestEither.#let_closure_3736", ptr %let_func_1.i.i, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %Right_func_0.i.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @TestEither.Right, ptr %Right_func_0.i.i, align 8
  %36 = load ptr, ptr %34, align 8
  %37 = load ptr, ptr %let_func_1.i.i, align 8
  %38 = tail call ptr %37(ptr %36, ptr nonnull %35)
  %39 = load i8, ptr %14, align 1
  %switch.i = icmp eq i8 %39, 0
  br i1 %switch.i, label %switch_branch_TestEither.Left_4.i, label %switch_branch_TestEither.Right_5.i

switch_branch_TestEither.Left_4.i:                ; preds = %1
  %40 = load ptr, ptr %15, align 8
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %41, align 1
  %42 = getelementptr { i8, { ptr } }, ptr %41, i64 0, i32 1, i32 0
  store ptr %40, ptr %42, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %43, align 1
  %44 = getelementptr { i8, { ptr } }, ptr %43, i64 0, i32 1, i32 0
  store ptr %40, ptr %44, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_10.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %43, ptr %let_capture_10.i, align 8
  store ptr %let_capture_10.i, ptr %45, align 8
  %let_func_5.i = getelementptr { ptr, ptr }, ptr %45, i64 0, i32 1
  store ptr @"TestEither.#let_closure_3773", ptr %let_func_5.i, align 8
  br label %TestEither.main.exit

switch_branch_TestEither.Right_5.i:               ; preds = %1
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %46, align 1
  %47 = getelementptr { i8, { ptr } }, ptr %46, i64 0, i32 1, i32 0
  store ptr @str3768, ptr %47, align 8
  %48 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %48, align 1
  %49 = getelementptr { i8, { ptr } }, ptr %48, i64 0, i32 1, i32 0
  store ptr %46, ptr %49, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %50, align 1
  %51 = getelementptr { i8, { ptr } }, ptr %50, i64 0, i32 1, i32 0
  store ptr %46, ptr %51, align 8
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_14.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %50, ptr %let_capture_14.i, align 8
  store ptr %let_capture_14.i, ptr %52, align 8
  %let_func_7.i = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr @"TestEither.#let_closure_3777", ptr %let_func_7.i, align 8
  br label %TestEither.main.exit

TestEither.main.exit:                             ; preds = %switch_branch_TestEither.Left_4.i, %switch_branch_TestEither.Right_5.i
  %"TestEither.#fun_closure_3778.sink.i" = phi ptr [ @"TestEither.#fun_closure_3778", %switch_branch_TestEither.Right_5.i ], [ @"TestEither.#fun_closure_3774", %switch_branch_TestEither.Left_4.i ]
  %.sink6.i = phi ptr [ %52, %switch_branch_TestEither.Right_5.i ], [ %45, %switch_branch_TestEither.Left_4.i ]
  %let_func_7.sink.i = phi ptr [ %let_func_7.i, %switch_branch_TestEither.Right_5.i ], [ %let_func_5.i, %switch_branch_TestEither.Left_4.i ]
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_14.i, ptr %53, align 8
  %fun_func_7.i = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr %"TestEither.#fun_closure_3778.sink.i", ptr %fun_func_7.i, align 8
  %54 = load ptr, ptr %.sink6.i, align 8
  %55 = load ptr, ptr %let_func_7.sink.i, align 8
  %56 = tail call ptr %55(ptr %54, ptr nonnull %53)
  ret i32 0
}
