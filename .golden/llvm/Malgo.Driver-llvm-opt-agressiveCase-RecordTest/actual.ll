; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/RecordTest.ll'
source_filename = "test/testcases/malgo/RecordTest.mlg"

@str3535 = unnamed_addr constant [2 x i8] c"a\00"
@str3536 = unnamed_addr constant [2 x i8] c"b\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_3534"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal i32 @"test/testcases/malgo/RecordTest.mlg.#let_closure_3537"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_3534", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 10, ptr %6, align 4
  %7 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str3535, ptr noundef nonnull %3)
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str3536, ptr noundef nonnull %5)
  %8 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3535)
  %9 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3536)
  %10 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3535)
  %11 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3536)
  %12 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1
  %15 = load i32, ptr %14, align 4
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %13, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %16, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/RecordTest.mlg.#let_closure_3537", ptr %let_func_0.i, align 8
  %17 = tail call i32 @malgo_add_int32_t(i32 %13, i32 %15)
  %18 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %18, align 1
  %19 = getelementptr { i8, { i32 } }, ptr %18, i64 0, i32 1, i32 0
  store i32 %17, ptr %19, align 4
  %20 = tail call ptr @malgo_int32_t_to_string(i32 %17)
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { ptr } }, ptr %21, i64 0, i32 1, i32 0
  store ptr %20, ptr %22, align 8
  %23 = tail call ptr @malgo_print_string(ptr %20)
  ret i32 0
}
