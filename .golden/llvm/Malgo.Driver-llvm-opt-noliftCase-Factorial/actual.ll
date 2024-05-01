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

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3263"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3264"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3265"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$p_1794_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"runtime/malgo/Builtin.mlg.$p_1794_0", ptr %3, align 4
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$p_2156_0") {
  %2 = tail call ptr @malgo_int64_t_to_string(i64 %"runtime/malgo/Builtin.mlg.$p_2156_0")
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3266"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.subInt64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$x_2255_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"runtime/malgo/Builtin.mlg.$x_2255_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3266", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"test/testcases/malgo/Factorial.mlg.#let_closure_3267"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.mulInt64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$x_2496_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"runtime/malgo/Builtin.mlg.$x_2496_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3267", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.isTrue#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0") {
common.ret:
  %cond = icmp eq i32 %"runtime/malgo/Builtin.mlg.$unboxed_2777_0", 1
  %1 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select = zext i1 %cond to i8
  store i8 %spec.select, ptr %1, align 1
  ret ptr %1
}

define internal i32 @"test/testcases/malgo/Factorial.mlg.#let_closure_3268"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.eqInt64#"(ptr nocapture nofree readnone %0, i64 %"runtime/malgo/Builtin.mlg.$x_3607_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"runtime/malgo/Builtin.mlg.$x_3607_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3268", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3269"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %n_0 = load ptr, ptr %0, align 8
  %"eqInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"eqInt64#_0" = load ptr, ptr %"eqInt64#_addr_0", align 8
  %"isTrue#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"isTrue#_0" = load ptr, ptr %"isTrue#_addr_0", align 8
  %2 = getelementptr { i8, { i64 } }, ptr %n_0, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %5 = load i64, ptr %4, align 4
  %6 = load ptr, ptr %"eqInt64#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"eqInt64#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i64 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i32 %12(ptr %10, i64 %5)
  %14 = load ptr, ptr %"isTrue#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"isTrue#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i32 %13)
  ret ptr %17
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3270"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %acc_0 = load ptr, ptr %0, align 8
  ret ptr %acc_0
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3272"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %n_0 = load ptr, ptr %0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"subInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"subInt64#_0" = load ptr, ptr %"subInt64#_addr_0", align 8
  %2 = getelementptr { i8, { i64 } }, ptr %n_0, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %5 = load i64, ptr %4, align 4
  %6 = load ptr, ptr %"subInt64#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"subInt64#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i64 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i64 %12(ptr %10, i64 %5)
  %14 = load ptr, ptr %"Int64#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i64 %13)
  ret ptr %17
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3273"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int64#_0":
  %n_0 = load ptr, ptr %0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"mulInt64#_addr_0" = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"mulInt64#_0" = load ptr, ptr %"mulInt64#_addr_0", align 8
  %2 = getelementptr { i8, { i64 } }, ptr %n_0, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %5 = load i64, ptr %4, align 4
  %6 = load ptr, ptr %"mulInt64#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"mulInt64#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, i64 %3)
  %10 = load ptr, ptr %9, align 8
  %11 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call i64 %12(ptr %10, i64 %5)
  %14 = load ptr, ptr %"Int64#_0", align 8
  %15 = getelementptr { ptr, ptr }, ptr %"Int64#_0", i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, i64 %13)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3271"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nocapture nofree readnone %1) {
  %n_0 = load ptr, ptr %0, align 8
  %"Int64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"Int64#_0" = load ptr, ptr %"Int64#_addr_0", align 8
  %"subInt64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %"subInt64#_0" = load ptr, ptr %"subInt64#_addr_0", align 8
  %"mulInt64#_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %"mulInt64#_0" = load ptr, ptr %"mulInt64#_addr_0", align 8
  %acc_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %acc_0 = load ptr, ptr %acc_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %n_0, ptr %let_capture_0, align 8
  %"Int64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %"Int64#_0", ptr %"Int64#_1", align 8
  %"subInt64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %"subInt64#_0", ptr %"subInt64#_1", align 8
  store ptr %let_capture_0, ptr %5, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3272", ptr %let_func_0, align 8
  %6 = getelementptr i8, ptr %n_0, i64 4
  %n_0.val = load i64, ptr %6, align 4
  %.val = load i64, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3265", ptr %let_func_0.i, align 8
  %8 = tail call i64 @malgo_sub_int64_t(i64 %n_0.val, i64 %.val)
  %9 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 4
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %8, ptr %10, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %n_0, ptr %let_capture_2, align 8
  %"Int64#_2" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 1
  store ptr %"Int64#_0", ptr %"Int64#_2", align 8
  %"mulInt64#_1" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_2, i64 0, i32 2
  store ptr %"mulInt64#_0", ptr %"mulInt64#_1", align 8
  store ptr %let_capture_2, ptr %11, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3273", ptr %let_func_1, align 8
  %n_0.val1 = load i64, ptr %6, align 4
  %12 = getelementptr i8, ptr %acc_0, i64 4
  %acc_0.val = load i64, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val1, ptr %let_capture_0.i2, align 4
  store ptr %let_capture_0.i2, ptr %13, align 8
  %let_func_0.i3 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3264", ptr %let_func_0.i3, align 8
  %14 = tail call i64 @malgo_mul_int64_t(i64 %n_0.val1, i64 %acc_0.val)
  %15 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %15, align 4
  %16 = getelementptr { i8, { i64 } }, ptr %15, i64 0, i32 1, i32 0
  store i64 %14, ptr %16, align 4
  %17 = tail call fastcc noundef ptr @"test/testcases/malgo/Factorial.mlg.$factAcc_curry_166"(ptr nofree noundef nonnull align 4 dereferenceable(1) %9, ptr nofree noundef nonnull align 4 dereferenceable(1) %15)
  ret ptr %17
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3274"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3265", ptr %let_func_0.i, align 8
  %6 = tail call i64 @malgo_sub_int64_t(i64 %n_0.val, i64 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3275"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3264", ptr %let_func_0.i, align 8
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
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr" = phi ptr [ %"test/testcases/malgo/Factorial.mlg.$n_167_0", %0 ], [ %23, %"switch_branch_runtime/malgo/Builtin.mlg.False_0" ]
  %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr" = phi ptr [ %"test/testcases/malgo/Factorial.mlg.$acc_168_0", %0 ], [ %30, %"switch_branch_runtime/malgo/Builtin.mlg.False_0" ]
  %1 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %1, align 1
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1, i32 0
  store i64 0, ptr %2, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"eqInt64#_func_0" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.eqInt64#", ptr %"eqInt64#_func_0", align 8
  %"eqInt64#_0" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %4, ptr %"eqInt64#_0", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"isTrue#_func_0" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.isTrue#", ptr %"isTrue#_func_0", align 8
  %"isTrue#_0" = getelementptr { ptr, ptr, ptr }, ptr %let_capture_0, i64 0, i32 2
  store ptr %5, ptr %"isTrue#_0", align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3269", ptr %let_func_0, align 8
  %6 = getelementptr i8, ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", i64 4
  %"test/testcases/malgo/Factorial.mlg.$n_167_0.val" = load i64, ptr %6, align 4
  %.val = load i64, ptr %2, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val", ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %7, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3263", ptr %let_func_0.i, align 8
  %8 = tail call i32 @malgo_eq_int64_t(i64 %"test/testcases/malgo/Factorial.mlg.$n_167_0.val", i64 %.val)
  %cond.i = icmp eq i32 %8, 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %cond.i to i8
  store i8 %spec.select.i, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %10, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3270", ptr %fun_func_0, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 40)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %fun_capture_2, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"Int64#_func_0" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int64#", ptr %"Int64#_func_0", align 8
  %"Int64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 1
  store ptr %12, ptr %"Int64#_0", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"subInt64#_func_0" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.subInt64#", ptr %"subInt64#_func_0", align 8
  %"subInt64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 2
  store ptr %13, ptr %"subInt64#_0", align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"mulInt64#_func_0" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.mulInt64#", ptr %"mulInt64#_func_0", align 8
  %"mulInt64#_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 3
  store ptr %14, ptr %"mulInt64#_0", align 8
  %acc_1 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_2, i64 0, i32 4
  store ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", ptr %acc_1, align 8
  store ptr %fun_capture_2, ptr %11, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#fun_closure_3271", ptr %fun_func_1, align 8
  %15 = load i8, ptr %9, align 1
  %switch = icmp eq i8 %15, 0
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  br i1 %switch, label %"switch_branch_runtime/malgo/Builtin.mlg.False_0", label %"switch_branch_runtime/malgo/Builtin.mlg.True_0"

"switch_branch_runtime/malgo/Builtin.mlg.False_0": ; preds = %tailrecurse
  %17 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i64 } }, ptr %17, i64 0, i32 1, i32 0
  store i64 1, ptr %18, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_2, align 8
  store ptr %let_capture_2, ptr %19, align 8
  %let_func_1 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3274", ptr %let_func_1, align 8
  %n_0.i = load ptr, ptr %let_capture_2, align 8
  %20 = getelementptr i8, ptr %n_0.i, i64 4
  %n_0.val.i = load i64, ptr %20, align 4
  %.val.i = load i64, ptr %18, align 4
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val.i, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %21, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3265", ptr %let_func_0.i.i, align 8
  %22 = tail call i64 @malgo_sub_int64_t(i64 %n_0.val.i, i64 %.val.i)
  %23 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { i64 } }, ptr %23, i64 0, i32 1, i32 0
  store i64 %22, ptr %24, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/Factorial.mlg.$n_167_0.tr", ptr %let_capture_4, align 8
  store ptr %let_capture_4, ptr %25, align 8
  %let_func_2 = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3275", ptr %let_func_2, align 8
  %n_0.i1 = load ptr, ptr %let_capture_4, align 8
  %26 = getelementptr i8, ptr %n_0.i1, i64 4
  %n_0.val.i2 = load i64, ptr %26, align 4
  %27 = getelementptr i8, ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr", i64 4
  %.val.i3 = load i64, ptr %27, align 4
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %n_0.val.i2, ptr %let_capture_0.i.i4, align 4
  store ptr %let_capture_0.i.i4, ptr %28, align 8
  %let_func_0.i.i5 = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3264", ptr %let_func_0.i.i5, align 8
  %29 = tail call i64 @malgo_mul_int64_t(i64 %n_0.val.i2, i64 %.val.i3)
  %30 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { i64 } }, ptr %30, i64 0, i32 1, i32 0
  store i64 %29, ptr %31, align 4
  br label %tailrecurse

"switch_branch_runtime/malgo/Builtin.mlg.True_0": ; preds = %tailrecurse
  ret ptr %"test/testcases/malgo/Factorial.mlg.$acc_168_0.tr"
}

define internal ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3283"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
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
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %malgo_int64_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_int64_t_to_string", ptr %malgo_int64_t_to_string_func_0.i, align 8
  %11 = tail call ptr @malgo_int64_t_to_string(i64 %9)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %11, ptr %13, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %14, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/Factorial.mlg.#let_closure_3283", ptr %let_func_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %16 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr @malgo_print_string(ptr %17)
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %19, align 1
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %20, align 1
  %21 = tail call ptr @malgo_newline(ptr noundef nonnull %20)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
