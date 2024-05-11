; ModuleID = '.malgo-work/test/testcases/malgo/RecordTest.ll'
source_filename = "test/testcases/malgo/RecordTest.mlg"

@str3662 = unnamed_addr constant [2 x i8] c"a\00"
@str3663 = unnamed_addr constant [2 x i8] c"b\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal noundef ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3664(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3665, ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_add_int32_t(i32 %"int32#_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3665(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
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
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str3662, ptr noundef nonnull %3)
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str3663, ptr noundef nonnull %5)
  %8 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3662)
  %9 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3663)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M14let_x5Fclosure47test_x2Ftestcases_x2Fmalgo_x2FRecordTest_x2Emlg12Internal3664, ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3662)
  %12 = tail call ptr @malgo_hash_table_get(ptr %7, ptr noundef nonnull @str3663)
  %13 = load ptr, ptr %10, align 8
  %14 = load ptr, ptr %let_func_0.i.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr %12)
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1
  %17 = load i32, ptr %16, align 4
  %18 = tail call ptr @malgo_int32_t_to_string(i32 %17)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %18, ptr %20, align 8
  %21 = tail call ptr @malgo_print_string(ptr %18)
  ret i32 0
}
