; ModuleID = './test/tmp/malgo_test/normal/TestPolySynonym.ll'
source_filename = "./test/testcases/malgo/TestPolySynonym.mlg"

@str2837 = unnamed_addr constant [4 x i8] c"snd\00"
@str2838 = unnamed_addr constant [4 x i8] c"fst\00"
@str2839 = unnamed_addr constant [5 x i8] c"hoge\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Prelude.printInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Prelude.$i_773_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"Prelude.$i_773_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %6
}

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

define internal ptr @"TestPolySynonym.#let_closure_2840"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %p_0)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str2839, ptr %6, align 8
  %7 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str2837, ptr noundef nonnull %5)
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str2838, ptr noundef nonnull %3)
  %8 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2837)
  %9 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str2838)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %10, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestPolySynonym.#let_closure_2840", ptr %let_func_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Prelude.printInt32, ptr %printInt32_func_0.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %12 = getelementptr { i8, { i32 } }, ptr %p_0.i.i, i64 0, i32 1
  %13 = load i32, ptr %12, align 4
  %14 = tail call ptr @malgo_int32_t_to_string(i32 %13)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %14, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr %14)
  ret i32 0
}
