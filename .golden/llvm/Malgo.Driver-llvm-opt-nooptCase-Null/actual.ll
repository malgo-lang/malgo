; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Null.ll'
source_filename = "test/testcases/malgo/Null.mlg"

@str184 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Null.mlg.#let_closure_181"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal noundef ptr @"test/testcases/malgo/Null.mlg.#let_closure_182"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Null.mlg.#fun_closure_183"(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %.val = load i8, ptr %1, align 1
  %switch.i = icmp eq i8 %.val, 0
  br i1 %switch.i, label %"switch_branch_test/testcases/malgo/Null.mlg.False_0.i", label %"switch_branch_test/testcases/malgo/Null.mlg.True_0.i"

"switch_branch_test/testcases/malgo/Null.mlg.False_0.i": ; preds = %2
  %3 = tail call ptr @malgo_print_string(ptr noundef nonnull @str184)
  br label %"test/testcases/malgo/Null.mlg.$raw_fun_180.exit"

"switch_branch_test/testcases/malgo/Null.mlg.True_0.i": ; preds = %2
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_exit_failure(ptr noundef nonnull %4)
  br label %"test/testcases/malgo/Null.mlg.$raw_fun_180.exit"

"test/testcases/malgo/Null.mlg.$raw_fun_180.exit": ; preds = %"switch_branch_test/testcases/malgo/Null.mlg.False_0.i", %"switch_branch_test/testcases/malgo/Null.mlg.True_0.i"
  %common.ret.op.i = phi ptr [ %3, %"switch_branch_test/testcases/malgo/Null.mlg.False_0.i" ], [ %5, %"switch_branch_test/testcases/malgo/Null.mlg.True_0.i" ]
  ret ptr %common.ret.op.i
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_182", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %6, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_182", ptr %let_func_0.i2.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = load ptr, ptr %6, align 8
  %9 = load ptr, ptr %let_func_0.i2.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %7)
  %11 = load ptr, ptr %4, align 8
  %12 = load ptr, ptr %let_func_0.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr %10)
  %14 = load i8, ptr %13, align 1
  %switch.i.i.i = icmp eq i8 %14, 0
  br i1 %switch.i.i.i, label %"switch_branch_test/testcases/malgo/Null.mlg.Nil_0.i.i.i", label %"switch_branch_test/testcases/malgo/Null.mlg.Cons_0.i.i.i"

"switch_branch_test/testcases/malgo/Null.mlg.Nil_0.i.i.i": ; preds = %1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  br label %"test/testcases/malgo/Null.mlg.main.exit"

"switch_branch_test/testcases/malgo/Null.mlg.Cons_0.i.i.i": ; preds = %1
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %13, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %18, i64 0, i32 1, i32 0
  store ptr %17, ptr %19, align 8
  br label %"test/testcases/malgo/Null.mlg.main.exit"

"test/testcases/malgo/Null.mlg.main.exit":        ; preds = %"switch_branch_test/testcases/malgo/Null.mlg.Nil_0.i.i.i", %"switch_branch_test/testcases/malgo/Null.mlg.Cons_0.i.i.i"
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i.i = zext i1 %switch.i.i.i to i8
  store i8 %spec.select.i.i.i, ptr %20, align 1
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %21, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#let_closure_181", ptr %let_func_0.i4.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %22, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/Null.mlg.#fun_closure_183", ptr %fun_func_0.i, align 8
  %23 = load ptr, ptr %21, align 8
  %24 = load ptr, ptr %let_func_0.i4.i, align 8
  %25 = tail call ptr %24(ptr %23, ptr nonnull %22)
  ret i32 0
}
