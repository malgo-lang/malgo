; ModuleID = '.malgo-work/test/testcases/malgo/Test0.ll'
source_filename = "test/testcases/malgo/Test0.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M34let_x2439_x24d22_x5Fclosure_x24d5342test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$3b_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$3b_0")
  ret ptr %6
}

define internal ptr @_M27fun_x2445_x5Fclosure_x24d5442test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %3, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %.val)
  %5 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %5
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$39$d22_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %"let$39$d22_capture_0.i", align 8
  store ptr %"let$39$d22_capture_0.i", ptr %5, align 8
  %"let$39$d22_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M34let_x2439_x24d22_x5Fclosure_x24d5342test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"let$39$d22_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$45_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$45_capture_0.i", ptr %6, align 8
  %"fun$45_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M27fun_x2445_x5Fclosure_x24d5442test_x2Ftestcases_x2Fmalgo_x2FTest0_x2Emlg8Internal, ptr %"fun$45_func_0.i", align 8
  %7 = load ptr, ptr %5, align 8
  %8 = load ptr, ptr %"let$39$d22_func_0.i", align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %6)
  ret i32 0
}
