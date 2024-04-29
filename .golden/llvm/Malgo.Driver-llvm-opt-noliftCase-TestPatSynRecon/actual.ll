; ModuleID = '/workspaces/malgo/.malgo-work/TestPatSynRecon.ll'
source_filename = "./test/testcases/malgo/TestPatSynRecon.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"TestPatSynRecon.#let_closure_2990"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal noundef ptr @TestPatSynRecon.sum(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"TestPatSynRecon.$cons_95_0") {
  %2 = load i8, ptr %"TestPatSynRecon.$cons_95_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %common.ret1, label %switch_branch_Prelude.Cons_0

common.ret1:                                      ; preds = %1, %switch_branch_Prelude.Cons_0
  %.sink = phi i64 [ %16, %switch_branch_Prelude.Cons_0 ], [ 0, %1 ]
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 %.sink, ptr %4, align 4
  ret ptr %3

switch_branch_Prelude.Cons_0:                     ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"TestPatSynRecon.$cons_95_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %"TestPatSynRecon.$cons_95_0", i64 0, i32 1, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { i8, { i64 } }, ptr %6, i64 0, i32 1
  %10 = load i64, ptr %9, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %sum_func_0 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @TestPatSynRecon.sum, ptr %sum_func_0, align 8
  %12 = tail call ptr @TestPatSynRecon.sum(ptr poison, ptr nocapture nofree readonly %8)
  %13 = getelementptr { i8, { i64 } }, ptr %12, i64 0, i32 1
  %14 = load i64, ptr %13, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %10, ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %15, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"TestPatSynRecon.#let_closure_2990", ptr %let_func_0, align 8
  %p_0.i = load i64, ptr %let_capture_0, align 4
  %16 = tail call i64 @malgo_add_int64_t(i64 %p_0.i, i64 %14)
  br label %common.ret1
}

define internal ptr @"TestPatSynRecon.#let_closure_2991"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @"TestPatSynRecon.#fun_closure_2992"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_Builtin.Int64#_0":
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %3)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %7
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 2, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %5, ptr %9, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 1
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %11, align 8
  %12 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %3, ptr %12, align 8
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 1
  store ptr %8, ptr %13, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %sum_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @TestPatSynRecon.sum, ptr %sum_func_0.i, align 8
  %15 = tail call ptr @TestPatSynRecon.sum(ptr poison, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %11)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %16, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"TestPatSynRecon.#let_closure_2991", ptr %let_func_0.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %17, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"TestPatSynRecon.#fun_closure_2992", ptr %fun_func_0.i, align 8
  %18 = load ptr, ptr %16, align 8
  %19 = load ptr, ptr %let_func_0.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr nonnull %17)
  ret i32 0
}
