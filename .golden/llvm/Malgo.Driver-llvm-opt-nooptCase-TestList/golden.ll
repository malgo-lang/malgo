; ModuleID = '/workspaces/malgo/.malgo-work/TestList.ll'
source_filename = "./test/testcases/malgo/TestList.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str118 = unnamed_addr constant [1 x i8] zeroinitializer
@str263 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"TestList.#let_closure_254"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
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
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"TestList.#let_closure_254", ptr %let_func_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 2, ptr %9, align 4
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i2.i, align 8
  store ptr %let_capture_0.i2.i, ptr %10, align 8
  %let_func_0.i3.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestList.#let_closure_254", ptr %let_func_0.i3.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 3, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i4.i, align 8
  store ptr %let_capture_0.i4.i, ptr %13, align 8
  %let_func_0.i5.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"TestList.#let_closure_254", ptr %let_func_0.i5.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = load ptr, ptr %13, align 8
  %16 = load ptr, ptr %let_func_0.i5.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr nonnull %14)
  %18 = load ptr, ptr %10, align 8
  %19 = load ptr, ptr %let_func_0.i3.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr %17)
  %21 = load ptr, ptr %7, align 8
  %22 = load ptr, ptr %let_func_0.i.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr %20)
  %24 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %24, align 1
  %25 = load i8, ptr %23, align 1
  %switch.i.i = icmp eq i8 %25, 0
  br i1 %switch.i.i, label %switch_branch_Prelude.Nil_0.i.i, label %switch_branch_Prelude.Cons_0.i.i

switch_branch_Prelude.Nil_0.i.i:                  ; preds = %1
  %26 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %26, align 1
  %27 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %27, align 1
  %28 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %27)
  br label %TestList.main.exit

switch_branch_Prelude.Cons_0.i.i:                 ; preds = %1
  %29 = getelementptr { i8, { ptr, ptr } }, ptr %23, i64 0, i32 1
  %30 = load ptr, ptr %29, align 8
  br label %TestList.main.exit

TestList.main.exit:                               ; preds = %switch_branch_Prelude.Nil_0.i.i, %switch_branch_Prelude.Cons_0.i.i
  %common.ret.op.i.i = phi ptr [ %28, %switch_branch_Prelude.Nil_0.i.i ], [ %30, %switch_branch_Prelude.Cons_0.i.i ]
  %31 = getelementptr i8, ptr %common.ret.op.i.i, i64 4
  %.val.i = load i32, ptr %31, align 4
  %32 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %33, align 1
  %34 = getelementptr { i8, { ptr } }, ptr %33, i64 0, i32 1, i32 0
  store ptr %32, ptr %34, align 8
  %35 = tail call ptr @malgo_print_string(ptr %32)
  %36 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %36, align 1
  %37 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %37, align 1
  %38 = tail call ptr @malgo_newline(ptr noundef nonnull %37)
  ret i32 0
}
