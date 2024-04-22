; ModuleID = './test/tmp/malgo_test/nolift/RecordTest.ll'
source_filename = "./test/testcases/malgo/RecordTest.mlg"

@str2905 = unnamed_addr constant [2 x i8] c"a\00"
@str2906 = unnamed_addr constant [2 x i8] c"b\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i32 @"RecordTest.#let_closure_2904"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_add_int32_t(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1808_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"Builtin.$p_1808_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"RecordTest.#let_closure_2904", ptr %let_func_0, align 8
  ret ptr %2
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal i32 @"RecordTest.#let_closure_2907"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 8
  %malgo_add_int32_t_addr_0 = getelementptr { i32, ptr }, ptr %0, i64 0, i32 1
  %malgo_add_int32_t_0 = load ptr, ptr %malgo_add_int32_t_addr_0, align 8
  %3 = load ptr, ptr %malgo_add_int32_t_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_add_int32_t_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 %p_0)
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call i32 %9(ptr %7, i32 %1)
  ret i32 %10
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
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str2905, ptr noundef nonnull %3)
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str2906, ptr noundef nonnull %5)
  %8 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2905)
  %9 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2906)
  %10 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2905)
  %11 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2906)
  %12 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1
  %15 = load i32, ptr %14, align 4
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store i32 %13, ptr %let_capture_0.i, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %malgo_add_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i, align 8
  %malgo_add_int32_t_0.i = getelementptr { i32, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %17, ptr %malgo_add_int32_t_0.i, align 8
  store ptr %let_capture_0.i, ptr %16, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"RecordTest.#let_closure_2907", ptr %let_func_0.i, align 8
  %18 = tail call i32 @malgo_add_int32_t(i32 %13, i32 %15)
  %19 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { i32 } }, ptr %19, i64 0, i32 1, i32 0
  store i32 %18, ptr %20, align 4
  %21 = tail call ptr @malgo_int32_t_to_string(i32 %18)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %21, ptr %23, align 8
  %24 = tail call ptr @malgo_print_string(ptr %21)
  ret i32 0
}
