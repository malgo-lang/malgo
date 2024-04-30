; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Factorial.ll'
source_filename = "test/testcases/malgo/Factorial.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str270 = unnamed_addr constant [1 x i8] zeroinitializer
@str420 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_sub_int64_t(i64, i64) local_unnamed_addr

declare i64 @malgo_mul_int64_t(i64, i64) local_unnamed_addr

declare i32 @malgo_eq_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"Factorial.#fun_closure_256"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %acc_0 = load ptr, ptr %0, align 8
  ret ptr %acc_0
}

define internal ptr @"Factorial.#fun_closure_257"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %acc_0 = load ptr, ptr %0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_337", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i64 } }, ptr %4, i64 0, i32 1, i32 0
  store i64 1, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %9, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"Factorial.#let_closure_409", ptr %let_func_0.i2.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %10, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Factorial.#let_closure_431", ptr %let_func_0.i4.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i3.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %11, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Factorial.#let_closure_275", ptr %let_func_0.i.i.i.i, align 8
  %"int64#_0.i.i.i.i" = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %12 = getelementptr i8, ptr %"int64#_0.i.i.i.i", i64 4
  %"int64#_0.val.i.i.i.i" = load i64, ptr %12, align 4
  %13 = getelementptr i8, ptr %acc_0, i64 4
  %.val.i.i.i.i = load i64, ptr %13, align 4
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val.i.i.i.i", ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %14, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Factorial.#let_closure_440", ptr %let_func_0.i.i.i.i.i.i, align 8
  %x_0.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i.i.i, ptr %15, align 8
  %let_func_0.i.i.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Factorial.#let_closure_319", ptr %let_func_0.i.i.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i.i.i, align 4
  %16 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i.i.i.i.i.i.i, i64 %.val.i.i.i.i)
  %17 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i64 } }, ptr %17, i64 0, i32 1, i32 0
  store i64 %16, ptr %18, align 4
  %19 = load ptr, ptr %9, align 8
  %20 = load ptr, ptr %let_func_0.i2.i, align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %17)
  ret ptr %21
}

define internal noundef ptr @"Factorial.#let_closure_275"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Factorial.#let_closure_440", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_319", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 %7, ptr %9, align 4
  ret ptr %8
}

define internal noundef ptr @Builtin.toStringInt64(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int64#_2174_0") {
"switch_branch_Builtin.Int64#_0":
  %1 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2174_0", i64 0, i32 1
  %2 = load i64, ptr %1, align 4
  %3 = tail call ptr @malgo_int64_t_to_string(i64 %2)
  %4 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  ret ptr %4
}

define internal ptr @"Factorial.#let_closure_293"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal noundef ptr @"Factorial.#let_closure_298"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Factorial.#let_closure_302", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_349", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i64 @malgo_sub_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 %7, ptr %9, align 4
  ret ptr %8
}

define internal noundef ptr @"Factorial.#let_closure_301"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int64#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int64#_0", i64 4
  %"int64#_0.val" = load i64, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Factorial.#let_closure_331", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_355", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_eq_int64_t(i64 %p_0.i.i.i.i, i64 %.val)
  %cond.i.i = icmp eq i32 %7, 1
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i = zext i1 %cond.i.i to i8
  store i8 %spec.select.i.i, ptr %8, align 1
  ret ptr %8
}

define internal i64 @"Factorial.#let_closure_302"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_349", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_sub_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
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

define internal ptr @"Factorial.#let_closure_316"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %true_0, ptr %let_capture_0.i, align 8
  %t_0.i = getelementptr { ptr, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %t_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_407", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal i64 @"Factorial.#let_closure_319"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_mul_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal i32 @"Factorial.#let_closure_331"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_355", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_eq_int64_t(i64 %p_0.i.i, i64 %1)
  ret i32 %4
}

define internal noundef ptr @"Factorial.#let_closure_337"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_298", ptr %let_func_0.i.i, align 8
  %"int64#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int64#_0.i.i", i64 4
  %"int64#_0.val.i.i" = load i64, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i64, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_302", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Factorial.#let_closure_349", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i64 @malgo_sub_int64_t(i64 %p_0.i.i.i.i.i.i, i64 %.val.i.i)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %8, ptr %10, align 4
  ret ptr %9
}

define internal i64 @"Factorial.#let_closure_349"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_sub_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal noundef ptr @"Factorial.#let_closure_351"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_301", ptr %let_func_0.i.i, align 8
  %"int64#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int64#_0.i.i", i64 4
  %"int64#_0.val.i.i" = load i64, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i64, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_331", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Factorial.#let_closure_355", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i32 @malgo_eq_int64_t(i64 %p_0.i.i.i.i.i.i, i64 %.val.i.i)
  %cond.i.i.i.i = icmp eq i32 %8, 1
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i.i.i = zext i1 %cond.i.i.i.i to i8
  store i8 %spec.select.i.i.i.i, ptr %9, align 1
  ret ptr %9
}

define internal i32 @"Factorial.#let_closure_355"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i32 @malgo_eq_int64_t(i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Factorial.#let_closure_407"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %true_0 = load ptr, ptr %0, align 8
  %t_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %t_0 = load ptr, ptr %t_addr_0, align 8
  %true_0.val = load i8, ptr %true_0, align 1
  %switch.i = icmp eq i8 %true_0.val, 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %spec.select.i = select i1 %switch.i, ptr %1, ptr %t_0
  %4 = load ptr, ptr %spec.select.i, align 8
  %5 = getelementptr { ptr, ptr }, ptr %spec.select.i, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
}

define internal ptr @"Factorial.#let_closure_409"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %n_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_351", ptr %let_func_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i64 } }, ptr %4, i64 0, i32 1, i32 0
  store i64 0, ptr %5, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %4)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %9, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"Factorial.#let_closure_316", ptr %let_func_0.i2.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Factorial.#fun_closure_256", ptr %fun_func_0.i, align 8
  %11 = load ptr, ptr %9, align 8
  %12 = load ptr, ptr %let_func_0.i2.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr nonnull %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %fun_capture_2.i, align 8
  %n_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %n_0, ptr %n_0.i, align 8
  store ptr %fun_capture_2.i, ptr %14, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Factorial.#fun_closure_257", ptr %fun_func_1.i, align 8
  %15 = load ptr, ptr %13, align 8
  %16 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %17 = load ptr, ptr %16, align 8
  %18 = tail call ptr %17(ptr %15, ptr nonnull %14)
  ret ptr %18
}

define internal noundef ptr @"Factorial.#let_closure_431"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_275", ptr %let_func_0.i.i, align 8
  %"int64#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %4 = getelementptr i8, ptr %"int64#_0.i.i", i64 4
  %"int64#_0.val.i.i" = load i64, ptr %4, align 4
  %5 = getelementptr i8, ptr %1, i64 4
  %.val.i.i = load i64, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"int64#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Factorial.#let_closure_440", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Factorial.#let_closure_319", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %8 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i.i.i.i.i, i64 %.val.i.i)
  %9 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i64 } }, ptr %9, i64 0, i32 1, i32 0
  store i64 %8, ptr %10, align 4
  ret ptr %9
}

define internal i64 @"Factorial.#let_closure_440"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Factorial.#let_closure_319", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_mul_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 5, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Factorial.#let_closure_409", ptr %let_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 1, ptr %9, align 4
  %10 = load ptr, ptr %7, align 8
  %11 = load ptr, ptr %let_func_0.i.i.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %8)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Factorial.#let_closure_293", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %toStringInt64_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @Builtin.toStringInt64, ptr %toStringInt64_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %15 = getelementptr { i8, { i64 } }, ptr %x_0.i.i, i64 0, i32 1
  %16 = load i64, ptr %15, align 4
  %17 = tail call ptr @malgo_int64_t_to_string(i64 %16)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { ptr } }, ptr %18, i64 0, i32 1, i32 0
  store ptr %17, ptr %19, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %20, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"Factorial.#let_closure_293", ptr %let_func_0.i2.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %x_0.i3.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %22 = getelementptr i8, ptr %x_0.i3.i, i64 8
  %"Prelude.$str_716_0.val.i.i" = load ptr, ptr %22, align 8
  %23 = tail call ptr @malgo_print_string(ptr %"Prelude.$str_716_0.val.i.i")
  %24 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %24, align 1
  %25 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %25, align 1
  %26 = tail call ptr @malgo_newline(ptr noundef nonnull %25)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
