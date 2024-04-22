; ModuleID = './test/tmp/malgo_test/nono/Null.ll'
source_filename = "./test/testcases/malgo/Null.mlg"

@str183 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_exit_failure(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @Null.Just(ptr nocapture nofree readnone %0, ptr nofree %"Null.$p_120_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Null.$p_120_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @"Null.#let_closure_180"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @Null.Cons(ptr nocapture nofree readnone %0, ptr nofree %"Null.$p_125_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Null.$p_125_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Null.#let_closure_180", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Null.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Null.$p_133_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Null.$p_133_0")
  ret ptr %2
}

define internal ptr @Null.malgo_exit_failure(ptr nocapture nofree readnone %0, ptr %"Null.$p_134_0") {
  %2 = tail call ptr @malgo_exit_failure(ptr %"Null.$p_134_0")
  ret ptr %2
}

define internal ptr @"Null.#let_closure_181"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"Null.|>"(ptr nocapture nofree readnone %0, ptr nofree %"Null.$x_135_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Null.$x_135_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Null.#let_closure_181", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @Null.mHead(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"Null.$nil_143_0") {
  %2 = load i8, ptr %"Null.$nil_143_0", align 1
  %switch = icmp eq i8 %2, 0
  br i1 %switch, label %switch_branch_Null.Nil_0, label %switch_branch_Null.Cons_0

common.ret:                                       ; preds = %switch_branch_Null.Cons_0, %switch_branch_Null.Nil_0
  %common.ret.op = phi ptr [ %3, %switch_branch_Null.Nil_0 ], [ %7, %switch_branch_Null.Cons_0 ]
  ret ptr %common.ret.op

switch_branch_Null.Nil_0:                         ; preds = %1
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  br label %common.ret

switch_branch_Null.Cons_0:                        ; preds = %1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"Null.$nil_143_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %Just_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Null.Just, ptr %Just_func_0, align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  br label %common.ret
}

define internal noundef ptr @Null.isNothing(ptr nocapture nofree readnone %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"Null.$nothing_147_0") {
common.ret:
  %1 = load i8, ptr %"Null.$nothing_147_0", align 1
  %switch = icmp eq i8 %1, 0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %switch to i8
  store i8 %spec.select, ptr %2, align 1
  ret ptr %2
}

define internal noundef ptr @Null.null(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"Null.$as_149_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %mHead_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Null.mHead, ptr %mHead_func_0, align 8
  %3 = load i8, ptr %"Null.$as_149_0", align 1
  %switch.i = icmp eq i8 %3, 0
  br i1 %switch.i, label %switch_branch_Null.Nil_0.i, label %switch_branch_Null.Cons_0.i

switch_branch_Null.Nil_0.i:                       ; preds = %1
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  br label %Null.mHead.exit

switch_branch_Null.Cons_0.i:                      ; preds = %1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %"Null.$as_149_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %Just_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Null.Just, ptr %Just_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  br label %Null.mHead.exit

Null.mHead.exit:                                  ; preds = %switch_branch_Null.Nil_0.i, %switch_branch_Null.Cons_0.i
  %common.ret.op.i = phi ptr [ %4, %switch_branch_Null.Nil_0.i ], [ %8, %switch_branch_Null.Cons_0.i ]
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %isNothing_func_0 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Null.isNothing, ptr %isNothing_func_0, align 8
  %11 = load i8, ptr %common.ret.op.i, align 1
  %switch.i1 = icmp eq i8 %11, 0
  %12 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %switch.i1 to i8
  store i8 %spec.select.i, ptr %12, align 1
  ret ptr %12
}

define internal ptr @"Null.#fun_closure_182"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %switch_branch_Null.False_0, label %switch_branch_Null.True_0

common.ret:                                       ; preds = %switch_branch_Null.True_0, %switch_branch_Null.False_0
  %malgo_exit_failure_0.sink4 = phi ptr [ %malgo_exit_failure_0, %switch_branch_Null.True_0 ], [ %malgo_print_string_0, %switch_branch_Null.False_0 ]
  %.sink1 = phi ptr [ %8, %switch_branch_Null.True_0 ], [ @str183, %switch_branch_Null.False_0 ]
  %4 = load ptr, ptr %malgo_exit_failure_0.sink4, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_exit_failure_0.sink4, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %.sink1)
  ret ptr %7

switch_branch_Null.False_0:                       ; preds = %2
  %malgo_print_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %malgo_print_string_0 = load ptr, ptr %malgo_print_string_addr_0, align 8
  br label %common.ret

switch_branch_Null.True_0:                        ; preds = %2
  %malgo_exit_failure_0 = load ptr, ptr %0, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Null.Cons, ptr %Cons_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Null.#let_closure_180", ptr %let_func_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %Cons_func_1.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Null.Cons, ptr %Cons_func_1.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %6, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %8, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Null.#let_closure_180", ptr %let_func_0.i2.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = load ptr, ptr %8, align 8
  %11 = load ptr, ptr %let_func_0.i2.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %9)
  %13 = load ptr, ptr %5, align 8
  %14 = load ptr, ptr %let_func_0.i.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr %12)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %null_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @Null.null, ptr %null_func_0.i, align 8
  %17 = tail call ptr @Null.null(ptr poison, ptr nocapture nofree readonly %15)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Null.|>", ptr %"|>_func_0.i", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %19, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"Null.#let_closure_181", ptr %let_func_0.i4.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %malgo_exit_failure_func_0.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @Null.malgo_exit_failure, ptr %malgo_exit_failure_func_0.i, align 8
  store ptr %21, ptr %fun_capture_0.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @Null.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %malgo_print_string_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %22, ptr %malgo_print_string_0.i, align 8
  store ptr %fun_capture_0.i, ptr %20, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"Null.#fun_closure_182", ptr %fun_func_0.i, align 8
  %23 = load ptr, ptr %19, align 8
  %24 = load ptr, ptr %let_func_0.i4.i, align 8
  %25 = tail call ptr %24(ptr %23, ptr nonnull %20)
  ret i32 0
}
