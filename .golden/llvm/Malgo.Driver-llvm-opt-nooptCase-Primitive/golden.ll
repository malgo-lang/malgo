; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Primitive.ll'
source_filename = "test/testcases/malgo/Primitive.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"test/testcases/malgo/Primitive.mlg.#let_closure_23"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Primitive.mlg.#let_closure_85", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define internal i64 @"test/testcases/malgo/Primitive.mlg.#let_closure_85"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 40, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Primitive.mlg.#let_closure_23", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/Primitive.mlg.#let_closure_85", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i.i, i64 noundef 2)
  %8 = tail call ptr @malgo_int64_t_to_string(i64 %7)
  %9 = tail call ptr @malgo_print_string(ptr %8)
  ret i32 0
}
