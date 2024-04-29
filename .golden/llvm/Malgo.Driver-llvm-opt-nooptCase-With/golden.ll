; ModuleID = '/workspaces/malgo/.malgo-work/With.ll'
source_filename = "./test/testcases/malgo/With.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str208 = unnamed_addr constant [1 x i8] zeroinitializer
@str227 = unnamed_addr constant [4 x i8] c"end\00"
@str280 = unnamed_addr constant [4 x i8] c"foo\00"
@str366 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"With.#let_closure_243"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %str_0 = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %str_0, i64 8
  %"With.$str_93_0.val.i" = load ptr, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr %"With.$str_93_0.val.i")
  %5 = load ptr, ptr %1, align 8
  %6 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr %str_0)
  ret ptr %8
}

define internal ptr @"With.#let_closure_259"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %finalizer_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %1, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %finalizer_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %finalizer_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
}

define internal ptr @"With.#fun_closure_274"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str280, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"With.#let_closure_243", ptr %let_func_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"With.#fun_closure_281", ptr %fun_func_0.i, align 8
  %7 = load ptr, ptr %5, align 8
  %8 = load ptr, ptr %let_func_0.i.i, align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %6)
  ret ptr %9
}

define internal ptr @"With.#fun_closure_281"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr %.val)
  ret ptr %4
}

define internal ptr @"With.#fun_closure_347"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str227, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str227)
  ret ptr %5
}

define internal ptr @"With.#fun_closure_348"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"With.#fun_closure_274", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %3, align 8
  %6 = load ptr, ptr %fun_func_0.i, align 8
  %7 = tail call ptr %6(ptr %5, ptr nonnull %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %fun_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  ret ptr %11
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"With.#fun_closure_347", ptr %fun_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"With.#let_closure_259", ptr %let_func_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %7, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"With.#fun_closure_348", ptr %fun_func_1.i, align 8
  %8 = load ptr, ptr %6, align 8
  %9 = load ptr, ptr %let_func_0.i.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %7)
  ret i32 0
}
