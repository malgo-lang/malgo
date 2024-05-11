; ModuleID = '.malgo-work/test/testcases/malgo/TestDot.ll'
source_filename = "test/testcases/malgo/TestDot.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3737(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal i64 @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3738(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3737, ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define internal i64 @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3739(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3737, ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 0, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 0, ptr %let_capture_0.i, align 4
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3738, ptr %let_func_0.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %p_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3737, ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i.i, i64 noundef 1)
  %8 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 %7, ptr %9, align 4
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %7, ptr %let_capture_2.i, align 4
  store ptr %let_capture_2.i, ptr %10, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3739, ptr %let_func_1.i, align 8
  %p_0.i1.i = load i64, ptr %let_capture_2.i, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %p_0.i1.i, ptr %let_capture_0.i.i.i2.i, align 4
  store ptr %let_capture_0.i.i.i2.i, ptr %11, align 8
  %let_func_0.i.i.i3.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @_M14let_x5Fclosure44test_x2Ftestcases_x2Fmalgo_x2FTestDot_x2Emlg12Internal3737, ptr %let_func_0.i.i.i3.i, align 8
  %p_0.i.i.i4.i = load i64, ptr %let_capture_0.i.i.i2.i, align 4
  %12 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i4.i, i64 noundef 1)
  %13 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i64 } }, ptr %13, i64 0, i32 1, i32 0
  store i64 %12, ptr %14, align 4
  %15 = tail call ptr @malgo_int64_t_to_string(i64 %12)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  %18 = tail call ptr @malgo_print_string(ptr %15)
  ret i32 0
}
