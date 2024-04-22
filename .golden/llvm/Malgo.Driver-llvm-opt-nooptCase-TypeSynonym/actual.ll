; ModuleID = './test/tmp/malgo_test/noopt/TypeSynonym.ll'
source_filename = "./test/testcases/malgo/TypeSynonym.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@TypeSynonym.hello = local_unnamed_addr global ptr undef
@str105 = unnamed_addr constant [1 x i8] zeroinitializer
@str253 = unnamed_addr constant [10 x i8] c"no branch\00"
@str283 = unnamed_addr constant [2 x i8] c" \00"
@str290 = unnamed_addr constant [6 x i8] c"hello\00"
@str291 = unnamed_addr constant [6 x i8] c"world\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"TypeSynonym.#let_closure_130"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"TypeSynonym.#fun_closure_236"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
  %3 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 16
  %.val1 = load ptr, ptr %4, align 8
  %5 = getelementptr i8, ptr %.val, i64 8
  %.val.val = load ptr, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr %.val.val)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str283, ptr %8, align 8
  %9 = tail call ptr @malgo_print_string(ptr noundef nonnull @str283)
  %10 = getelementptr i8, ptr %.val1, i64 8
  %.val2.i = load ptr, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %.val2.i)
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  %14 = tail call ptr @malgo_newline(ptr noundef nonnull %13)
  ret ptr %14
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str290, ptr %5, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr @str291, ptr %7, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %4, ptr %9, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 1
  store ptr %6, ptr %10, align 8
  store ptr %8, ptr @TypeSynonym.hello, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = load ptr, ptr @TypeSynonym.hello, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %13, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"TypeSynonym.#let_closure_130", ptr %let_func_0.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %14, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"TypeSynonym.#fun_closure_236", ptr %fun_func_0.i, align 8
  %15 = load ptr, ptr %13, align 8
  %16 = load ptr, ptr %let_func_0.i.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr nonnull %14)
  ret i32 0
}
