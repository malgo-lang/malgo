; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Factorial.ll'
source_filename = "test/testcases/malgo/Factorial.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_sub_int64_t(i64, i64) local_unnamed_addr

declare i64 @malgo_mul_int64_t(i64, i64) local_unnamed_addr

declare i32 @malgo_eq_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3929"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3932"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3933"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

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

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3934"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %x_0, i64 %1)
  ret i32 %3
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3935"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3934", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call i32 @malgo_eq_int64_t(i64 %x_0.i.i, i64 %.val)
  %cond.i.i = icmp eq i32 %6, 1
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %7, align 1
  ret ptr %7
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3936"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %acc_0 = load ptr, ptr %0, align 8
  ret ptr %acc_0
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3937"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %n_0 = load ptr, ptr %0, align 8
  %acc_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3942", ptr %let_func_0.i, align 8
  %6 = getelementptr i8, ptr %n_0, i64 4
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.val.i" = load i64, ptr %6, align 4
  %.val.i = load i64, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val.i", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3929", ptr %let_func_0.i.i, align 8
  %8 = tail call i64 @malgo_sub_int64_t(i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val.i", i64 %.val.i)
  %9 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 4
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %8, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %11, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3943", ptr %let_func_1.i, align 8
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.val1.i" = load i64, ptr %6, align 4
  %12 = getelementptr i8, ptr %acc_0, i64 4
  %"test/testcases/malgo/Factorial.mlg.$acc_168_0.val.i" = load i64, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val1.i", ptr %let_capture_0.i2.i, align 4
  store ptr %let_capture_0.i2.i, ptr %13, align 8
  %let_func_0.i3.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3933", ptr %let_func_0.i3.i, align 8
  %14 = tail call i64 @malgo_mul_int64_t(i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val1.i", i64 %"test/testcases/malgo/Factorial.mlg.$acc_168_0.val.i")
  %15 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %15, align 4
  %16 = getelementptr { i8, { i64 } }, ptr %15, i64 0, i32 1, i32 0
  store i64 %14, ptr %16, align 4
  %17 = tail call fastcc noundef ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr nofree noundef nonnull align 4 dereferenceable(1) %9, ptr nofree noundef nonnull align 4 dereferenceable(1) %15)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3938"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3929", ptr %let_func_0.i, align 8
  %6 = tail call i64 @malgo_sub_int64_t(i64 %n_0.val, i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3939"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3933", ptr %let_func_0.i, align 8
  %6 = tail call i64 @malgo_mul_int64_t(i64 %n_0.val, i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal fastcc noundef ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr nofree noundef nonnull align 4 dereferenceable(1) %"test/testcases/malgo/Factorial.mlg.$n_167_0", ptr nofree noundef nonnull align 4 dereferenceable(1) %"test/testcases/malgo/Factorial.mlg.$acc_168_0") unnamed_addr {
  br label %tailrecurse

tailrecurse:                                      ; preds = %"switch_branch_runtime/malgo/Builtin.mlg.False_0", %0
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr" = phi ptr [ %"test/testcases/malgo/Factorial.mlg.$n_167_0", %0 ], [ %18, %"switch_branch_runtime/malgo/Builtin.mlg.False_0" ]
  %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr" = phi ptr [ %"test/testcases/malgo/Factorial.mlg.$acc_168_0", %0 ], [ %25, %"switch_branch_runtime/malgo/Builtin.mlg.False_0" ]
  %1 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %1, align 1
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1, i32 0
  store i64 0, ptr %2, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3935", ptr %let_func_0, align 8
  %4 = getelementptr i8, ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", i64 4
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.val" = load i64, ptr %4, align 4
  %.val = load i64, ptr %2, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val", ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3940", ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_eq_int64_t(i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val", i64 %.val)
  %cond.i = icmp eq i32 %6, 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %cond.i to i8
  store i8 %spec.select.i, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %8, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3936", ptr %fun_func_0, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %fun_capture_2, align 8
  %acc_1 = getelementptr { ptr, ptr }, ptr %fun_capture_2, i64 0, i32 1
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", ptr %acc_1, align 8
  store ptr %fun_capture_2, ptr %9, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3937", ptr %fun_func_1, align 8
  %10 = load i8, ptr %7, align 1
  %switch = icmp eq i8 %10, 0
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  br i1 %switch, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0", label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %tailrecurse
  %12 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { i64 } }, ptr %12, i64 0, i32 1, i32 0
  store i64 1, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_2, align 8
  store ptr %let_capture_2, ptr %14, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3938", ptr %let_func_1, align 8
  %n_0.i = load ptr, ptr %let_capture_2, align 8
  %15 = getelementptr i8, ptr %n_0.i, i64 4
  %n_0.val.i = load i64, ptr %15, align 4
  %.val.i = load i64, ptr %13, align 4
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val.i, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %16, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3929", ptr %let_func_0.i.i, align 8
  %17 = tail call i64 @malgo_sub_int64_t(i64 %n_0.val.i, i64 %.val.i)
  %18 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { i64 } }, ptr %18, i64 0, i32 1, i32 0
  store i64 %17, ptr %19, align 4
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_4, align 8
  store ptr %let_capture_4, ptr %20, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3939", ptr %let_func_2, align 8
  %n_0.i1 = load ptr, ptr %let_capture_4, align 8
  %21 = getelementptr i8, ptr %n_0.i1, i64 4
  %n_0.val.i2 = load i64, ptr %21, align 4
  %22 = getelementptr i8, ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", i64 4
  %.val.i3 = load i64, ptr %22, align 4
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val.i2, ptr %let_capture_0.i.i4, align 4
  store ptr %let_capture_0.i.i4, ptr %23, align 8
  %let_func_0.i.i5 = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3933", ptr %let_func_0.i.i5, align 8
  %24 = tail call i64 @malgo_mul_int64_t(i64 %n_0.val.i2, i64 %.val.i3)
  %25 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %25, align 1
  %26 = getelementptr { i8, { i64 } }, ptr %25, i64 0, i32 1, i32 0
  store i64 %24, ptr %26, align 4
  br label %tailrecurse

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %tailrecurse
  ret ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr"
}

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3940"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3941"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3942"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3941", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call i64 @malgo_sub_int64_t(i64 %x_0.i.i, i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3943"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3944", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call i64 @malgo_mul_int64_t(i64 %x_0.i.i, i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3944"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 4
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 5, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 4
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 1, ptr %6, align 4
  %7 = tail call fastcc ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr nofree noundef nonnull align 4 dereferenceable(1) %3, ptr nofree noundef nonnull align 4 dereferenceable(1) %5)
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1
  %9 = load i64, ptr %8, align 4
  %10 = tail call ptr @malgo_int64_t_to_string(i64 %9)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %13, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3932", ptr %let_func_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %15 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr @malgo_print_string(ptr %16)
  %18 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %18, align 1
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = tail call ptr @malgo_newline(ptr noundef nonnull %19)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
