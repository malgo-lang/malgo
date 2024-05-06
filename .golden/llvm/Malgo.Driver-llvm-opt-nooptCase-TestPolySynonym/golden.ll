; ModuleID = '.malgo-work/test/testcases/malgo/TestPolySynonym.ll'
source_filename = "test/testcases/malgo/TestPolySynonym.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str211 = unnamed_addr constant [5 x i8] c"hoge\00"
@str212 = unnamed_addr constant [4 x i8] c"fst\00"
@str213 = unnamed_addr constant [4 x i8] c"snd\00"
@str214 = unnamed_addr constant [1 x i8] zeroinitializer
@str278 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @"test/testcases/malgo/TestPolySynonym.mlg.#let_closure_217"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Prelude.mlg.$i_773_0") {
  %2 = getelementptr i8, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i64 4
  %"runtime/malgo/Prelude.mlg.$i_773_0.val" = load i32, ptr %2, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Prelude.mlg.$i_773_0.val")
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %6
}

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
  store i32 1, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str211, ptr %8, align 8
  %9 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str212, ptr noundef nonnull %5)
  tail call void @malgo_hash_table_insert(ptr %9, ptr noundef nonnull @str213, ptr noundef nonnull %7)
  %10 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str212)
  %11 = tail call ptr @malgo_hash_table_get(ptr %9, ptr noundef nonnull @str213)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %12, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPolySynonym.mlg.#let_closure_217", ptr %let_func_0.i.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %14 = getelementptr i8, ptr %x_0.i.i, i64 4
  %"runtime/malgo/Prelude.mlg.$i_773_0.val.i.i" = load i32, ptr %14, align 4
  %15 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Prelude.mlg.$i_773_0.val.i.i")
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  %18 = tail call ptr @malgo_print_string(ptr %15)
  ret i32 0
}
