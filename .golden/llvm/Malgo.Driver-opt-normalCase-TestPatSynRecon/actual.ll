; ModuleID = '.malgo-work/test/testcases/malgo/TestPatSynRecon.ll'
source_filename = "test/testcases/malgo/TestPatSynRecon.mlg"

declare void @GC_init() local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M35let_x242c2_x24ee6_x5Fclosure_x24f0452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %"d$83_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %"d$83_0")
  ret ptr %6
}

define internal ptr @_M27fun_x2489_x5Fclosure_x24f0552test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %3, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %.val)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %7
}

define internal i64 @_M35let_x2472d_x24ed2_x5Fclosure_x24f0652test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %"p$63_0" = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %"p$63_0", i64 %1)
  ret i64 %3
}

define internal fastcc noundef ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0) unnamed_addr {
  %1 = load i8, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %common.ret1, label %switch_branch_Cons_0

common.ret1:                                      ; preds = %0, %switch_branch_Cons_0
  %.sink = phi i64 [ %14, %switch_branch_Cons_0 ], [ 0, %0 ]
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %.sink, ptr %3, align 4
  ret ptr %2

switch_branch_Cons_0:                             ; preds = %0
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %_M10cons_x245f52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Temporal_0, i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1
  %9 = load i64, ptr %8, align 4
  %10 = tail call fastcc ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr nocapture nofree readonly %7)
  %11 = getelementptr { i8, { i64 } }, ptr %10, i64 0, i32 1
  %12 = load i64, ptr %11, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$72d$ed2_capture_0" = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %9, ptr %"let$72d$ed2_capture_0", align 4
  store ptr %"let$72d$ed2_capture_0", ptr %13, align 8
  %"let$72d$ed2_func_0" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @_M35let_x2472d_x24ed2_x5Fclosure_x24f0652test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$72d$ed2_func_0", align 8
  %"p$63_0.i" = load i64, ptr %"let$72d$ed2_capture_0", align 4
  %14 = tail call i64 @malgo_add_int64_t(i64 %"p$63_0.i", i64 %12)
  br label %common.ret1
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %3, i64 0, i32 1, i32 0
  store i64 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 2, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %5, ptr %9, align 8
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %8, i64 0, i32 1, i32 1
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %11, align 8
  %12 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %3, ptr %12, align 8
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %11, i64 0, i32 1, i32 1
  store ptr %8, ptr %13, align 8
  %14 = tail call fastcc ptr @_M3sum52test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8External(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(1) %11)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$2c2$ee6_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %"let$2c2$ee6_capture_0.i", align 8
  store ptr %"let$2c2$ee6_capture_0.i", ptr %15, align 8
  %"let$2c2$ee6_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M35let_x242c2_x24ee6_x5Fclosure_x24f0452test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"let$2c2$ee6_func_0.i", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"fun$89_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %"fun$89_capture_0.i", ptr %16, align 8
  %"fun$89_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M27fun_x2489_x5Fclosure_x24f0552test_x2Ftestcases_x2Fmalgo_x2FTestPatSynRecon_x2Emlg8Internal, ptr %"fun$89_func_0.i", align 8
  %17 = load ptr, ptr %15, align 8
  %18 = load ptr, ptr %"let$2c2$ee6_func_0.i", align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %16)
  ret i32 0
}
