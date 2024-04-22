; ModuleID = './test/tmp/malgo_test/normal/TestList.ll'
source_filename = "./test/testcases/malgo/TestList.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 2, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 3, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %10, align 1
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %10, i64 0, i32 1, i32 0
  store ptr %7, ptr %11, align 8
  %12 = getelementptr { i8, { ptr, ptr } }, ptr %10, i64 0, i32 1, i32 1
  store ptr %9, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %13, align 1
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %5, ptr %14, align 8
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1, i32 1
  store ptr %10, ptr %15, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %16, align 1
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %3, ptr %17, align 8
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %16, i64 0, i32 1, i32 1
  store ptr %13, ptr %18, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = load i8, ptr %16, align 1
  %switch.i = icmp eq i8 %20, 0
  br i1 %switch.i, label %switch_branch_Prelude.Nil_0.i, label %switch_branch_Prelude.Cons_0.i

switch_branch_Prelude.Nil_0.i:                    ; preds = %1
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %21)
  br label %TestList.main.exit

switch_branch_Prelude.Cons_0.i:                   ; preds = %1
  %23 = load ptr, ptr %17, align 8
  br label %TestList.main.exit

TestList.main.exit:                               ; preds = %switch_branch_Prelude.Nil_0.i, %switch_branch_Prelude.Cons_0.i
  %.sink.i = phi ptr [ %23, %switch_branch_Prelude.Cons_0.i ], [ %22, %switch_branch_Prelude.Nil_0.i ]
  %24 = getelementptr { i8, { i32 } }, ptr %.sink.i, i64 0, i32 1
  %25 = load i32, ptr %24, align 4
  %26 = tail call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %26, ptr %28, align 8
  %29 = tail call ptr @malgo_print_string(ptr %26)
  %30 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %30, align 1
  %31 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %31, align 1
  %32 = tail call ptr @malgo_newline(ptr noundef nonnull %31)
  ret i32 0
}
