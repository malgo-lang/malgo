; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestEither.ll'
source_filename = "test/testcases/malgo/TestEither.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str473 = unnamed_addr constant [12 x i8] c"unreachable\00"
@str489 = unnamed_addr constant [1 x i8] zeroinitializer
@str509 = unnamed_addr constant [6 x i8] c"error\00"
@str638 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @Builtin.toStringInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int32#_2181_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  ret ptr %4
}

define internal ptr @Prelude.putStr(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Prelude.$str_723_0") {
  %2 = getelementptr i8, ptr %"Prelude.$str_723_0", i64 8
  %"Prelude.$str_723_0.val" = load ptr, ptr %2, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Prelude.$str_723_0.val")
  ret ptr %3
}

define internal ptr @"TestEither.#let_closure_513"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @Prelude.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Prelude.$str_716_0") {
  %2 = getelementptr i8, ptr %"Prelude.$str_716_0", i64 8
  %"Prelude.$str_716_0.val" = load ptr, ptr %2, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Prelude.$str_716_0.val")
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @"TestEither.#let_closure_533"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 8
  %switch.i = icmp eq i8 %3, 0
  %4 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  br i1 %switch.i, label %switch_branch_TestEither.Left_0.i, label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Left_0.i:                ; preds = %2
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  br label %"TestEither.$andThen_curry_260.exit"

switch_branch_TestEither.Right_0.i:               ; preds = %2
  %__0 = load ptr, ptr %0, align 8
  %8 = load ptr, ptr %__0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %__0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %5)
  br label %"TestEither.$andThen_curry_260.exit"

"TestEither.$andThen_curry_260.exit":             ; preds = %switch_branch_TestEither.Left_0.i, %switch_branch_TestEither.Right_0.i
  %common.ret.op.i = phi ptr [ %6, %switch_branch_TestEither.Left_0.i ], [ %11, %switch_branch_TestEither.Right_0.i ]
  ret ptr %common.ret.op.i
}

define internal noundef ptr @"TestEither.#let_closure_562"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestEither.#let_closure_583", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestEither.#let_closure_667", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"TestEither.#let_closure_583"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestEither.#let_closure_667", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal ptr @"TestEither.#fun_closure_644"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_TestEither.Left_0.i, label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Left_0.i:                ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str473, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr noundef nonnull @str473)
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_newline(ptr noundef nonnull %8)
  br label %"TestEither.$raw_fun_435.exit"

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
  br label %"TestEither.$raw_fun_435.exit"

"TestEither.$raw_fun_435.exit":                   ; preds = %switch_branch_TestEither.Left_0.i, %switch_branch_TestEither.Right_0.i
  %common.ret.op.i = phi ptr [ %9, %switch_branch_TestEither.Left_0.i ], [ %17, %switch_branch_TestEither.Right_0.i ]
  ret ptr %common.ret.op.i
}

define internal noundef ptr @"TestEither.#fun_closure_645"(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestEither.#let_closure_562", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  ret ptr %9
}

define internal ptr @"TestEither.#fun_closure_646"(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %5 = getelementptr { i8, { i32 } }, ptr %x_0.i.i, i64 0, i32 1
  %6 = load i32, ptr %5, align 4
  %7 = tail call ptr @malgo_int32_t_to_string(i32 %6)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %10, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i2.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %putStr_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Prelude.putStr, ptr %putStr_func_0.i, align 8
  %x_0.i5.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %12 = getelementptr i8, ptr %x_0.i5.i, i64 8
  %"Prelude.$str_723_0.val.i.i" = load ptr, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr %"Prelude.$str_723_0.val.i.i")
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %13, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %14, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i4.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %Right_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @TestEither.Right, ptr %Right_func_0.i, align 8
  %16 = load ptr, ptr %14, align 8
  %17 = load ptr, ptr %let_func_0.i4.i, align 8
  %18 = tail call ptr %17(ptr %16, ptr nonnull %15)
  ret ptr %18
}

define internal noundef ptr @"TestEither.#fun_closure_647"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str509, ptr %4, align 8
  %5 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %3, ptr %6, align 8
  ret ptr %5
}

define internal noundef ptr @"TestEither.#fun_closure_648"(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestEither.#let_closure_562", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  ret ptr %9
}

define internal ptr @"TestEither.#fun_closure_649"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"TestEither.$raw_fun_466.exit", label %switch_branch_TestEither.Right_0.i

switch_branch_TestEither.Right_0.i:               ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %.val1, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = getelementptr { i8, { i32 } }, ptr %x_0.i.i, i64 0, i32 1
  %7 = load i32, ptr %6, align 4
  %8 = tail call ptr @malgo_int32_t_to_string(i32 %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %11, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i2.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %x_0.i3.i = load ptr, ptr %let_capture_0.i1.i, align 8
  br label %"TestEither.$raw_fun_466.exit"

"TestEither.$raw_fun_466.exit":                   ; preds = %2, %switch_branch_TestEither.Right_0.i
  %x_0.i3.sink.i = phi ptr [ %x_0.i3.i, %switch_branch_TestEither.Right_0.i ], [ %.val1, %2 ]
  %13 = getelementptr i8, ptr %x_0.i3.sink.i, i64 8
  %"Prelude.$str_716_0.val.i1.i" = load ptr, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr %"Prelude.$str_716_0.val.i1.i")
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  %17 = tail call ptr @malgo_newline(ptr noundef nonnull %16)
  ret ptr %17
}

define internal noundef ptr @TestEither.Right(ptr nocapture nofree readnone %0, ptr nofree %"TestEither.$p_251_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"TestEither.$p_251_0", ptr %3, align 8
  ret ptr %2
}

define internal i32 @"TestEither.#let_closure_667"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 1, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %9, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_644", ptr %fun_func_0.i, align 8
  %11 = load ptr, ptr %9, align 8
  %12 = load ptr, ptr %let_func_0.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr nonnull %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { i32 } }, ptr %14, i64 0, i32 1, i32 0
  store i32 1, ptr %15, align 4
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %14, ptr %17, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %16, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %18, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i2.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %19, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_645", ptr %fun_func_1.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %19, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %20, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"TestEither.#let_closure_533", ptr %let_func_0.i4.i, align 8
  %21 = load ptr, ptr %18, align 8
  %22 = load ptr, ptr %let_func_0.i2.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr nonnull %20)
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i5.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %23, ptr %let_capture_0.i5.i, align 8
  store ptr %let_capture_0.i5.i, ptr %24, align 8
  %let_func_0.i6.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i6.i, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_4.i, ptr %25, align 8
  %fun_func_2.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_646", ptr %fun_func_2.i, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i7.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %25, ptr %let_capture_0.i7.i, align 8
  store ptr %let_capture_0.i7.i, ptr %26, align 8
  %let_func_0.i8.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"TestEither.#let_closure_533", ptr %let_func_0.i8.i, align 8
  %27 = load ptr, ptr %24, align 8
  %28 = load ptr, ptr %let_func_0.i6.i, align 8
  %29 = tail call ptr %28(ptr %27, ptr nonnull %26)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i9.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %16, ptr %let_capture_0.i9.i, align 8
  store ptr %let_capture_0.i9.i, ptr %30, align 8
  %let_func_0.i10.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i10.i, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_6.i, ptr %31, align 8
  %fun_func_3.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_647", ptr %fun_func_3.i, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i11.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %31, ptr %let_capture_0.i11.i, align 8
  store ptr %let_capture_0.i11.i, ptr %32, align 8
  %let_func_0.i12.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"TestEither.#let_closure_533", ptr %let_func_0.i12.i, align 8
  %33 = load ptr, ptr %30, align 8
  %34 = load ptr, ptr %let_func_0.i10.i, align 8
  %35 = tail call ptr %34(ptr %33, ptr nonnull %32)
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i13.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %35, ptr %let_capture_0.i13.i, align 8
  store ptr %let_capture_0.i13.i, ptr %36, align 8
  %let_func_0.i14.i = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i14.i, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_8.i, ptr %37, align 8
  %fun_func_4.i = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_648", ptr %fun_func_4.i, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i15.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %37, ptr %let_capture_0.i15.i, align 8
  store ptr %let_capture_0.i15.i, ptr %38, align 8
  %let_func_0.i16.i = getelementptr { ptr, ptr }, ptr %38, i64 0, i32 1
  store ptr @"TestEither.#let_closure_533", ptr %let_func_0.i16.i, align 8
  %39 = load ptr, ptr %36, align 8
  %40 = load ptr, ptr %let_func_0.i14.i, align 8
  %41 = tail call ptr %40(ptr %39, ptr nonnull %38)
  %42 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i17.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %41, ptr %let_capture_0.i17.i, align 8
  store ptr %let_capture_0.i17.i, ptr %42, align 8
  %let_func_0.i18.i = getelementptr { ptr, ptr }, ptr %42, i64 0, i32 1
  store ptr @"TestEither.#let_closure_513", ptr %let_func_0.i18.i, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_10.i, ptr %43, align 8
  %fun_func_5.i = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @"TestEither.#fun_closure_649", ptr %fun_func_5.i, align 8
  %44 = load ptr, ptr %42, align 8
  %45 = load ptr, ptr %let_func_0.i18.i, align 8
  %46 = tail call ptr %45(ptr %44, ptr nonnull %43)
  ret i32 0
}
