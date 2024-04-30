; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestCast.ll'
source_filename = "test/testcases/malgo/TestCast.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str103 = unnamed_addr constant [1 x i8] zeroinitializer
@str254 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_new_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_read_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"TestCast.#let_closure_125"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, ptr %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_new_vector(i64 %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @"TestCast.#let_closure_211"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, ptr %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_read_vector(i64 %p_0, ptr %1)
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
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 2, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestCast.#let_closure_211", ptr %let_func_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 10, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %6, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestCast.#let_closure_125", ptr %let_func_0.i2.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 5, ptr %8, align 4
  %9 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %7)
  %10 = load ptr, ptr %6, align 8
  %11 = load ptr, ptr %let_func_0.i2.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr %9)
  %13 = load ptr, ptr %5, align 8
  %14 = load ptr, ptr %let_func_0.i.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr %12)
  %16 = tail call ptr @malgo_unsafe_cast(ptr %15)
  %17 = getelementptr i8, ptr %16, i64 4
  %.val.i = load i32, ptr %17, align 4
  %18 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %18, ptr %20, align 8
  %21 = tail call ptr @malgo_print_string(ptr %18)
  ret i32 0
}
