; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/RecordTest.ll'
source_filename = "test/testcases/malgo/RecordTest.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str131 = unnamed_addr constant [1 x i8] zeroinitializer
@str158 = unnamed_addr constant [2 x i8] c"a\00"
@str159 = unnamed_addr constant [2 x i8] c"b\00"
@str292 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_226"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_247", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_233", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_233"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_247"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_233", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 32, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 10, ptr %8, align 4
  %9 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str158, ptr noundef nonnull %5)
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str159, ptr noundef nonnull %7)
  %10 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str158)
  %11 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str159)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %12, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_226", ptr %let_func_0.i.i, align 8
  %13 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str158)
  %14 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str159)
  %15 = load ptr, ptr %12, align 8
  %16 = load ptr, ptr %let_func_0.i.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr %14)
  %18 = getelementptr i8, ptr %17, i64 4
  %.val.i = load i32, ptr %18, align 4
  %19 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr %19, ptr %21, align 8
  %22 = tail call ptr @malgo_print_string(ptr %19)
  ret i32 0
}
