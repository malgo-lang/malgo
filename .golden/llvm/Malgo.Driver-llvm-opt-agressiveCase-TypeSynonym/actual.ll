; ModuleID = './test/tmp/malgo_test/aggressive/TypeSynonym.ll'
source_filename = "./test/testcases/malgo/TypeSynonym.mlg"

@TypeSynonym.hello = local_unnamed_addr global ptr undef
@str3490 = unnamed_addr constant [2 x i8] c" \00"
@str3497 = unnamed_addr constant [6 x i8] c"hello\00"
@str3498 = unnamed_addr constant [6 x i8] c"world\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"TypeSynonym.#let_closure_3491"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %cast_0)
  ret ptr %6
}

define internal ptr @"TypeSynonym.#fun_closure_3492"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
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
  store ptr @str3490, ptr %8, align 8
  %9 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3490)
  %10 = getelementptr { i8, { ptr } }, ptr %.val1, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr @malgo_print_string(ptr %11)
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_newline(ptr noundef nonnull %14)
  ret ptr %15
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str3497, ptr %3, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str3498, ptr %5, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %2, ptr %7, align 8
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %6, i64 0, i32 1, i32 1
  store ptr %4, ptr %8, align 8
  store ptr %6, ptr @TypeSynonym.hello, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = load ptr, ptr @TypeSynonym.hello, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %11, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"TypeSynonym.#let_closure_3491", ptr %let_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %12, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"TypeSynonym.#fun_closure_3492", ptr %fun_func_0.i, align 8
  %13 = load ptr, ptr %11, align 8
  %14 = load ptr, ptr %let_func_0.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr nonnull %12)
  ret i32 0
}
