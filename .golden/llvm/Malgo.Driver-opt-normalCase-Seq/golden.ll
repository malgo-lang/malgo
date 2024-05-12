; ModuleID = '.malgo-work/test/testcases/malgo/Seq.ll'
source_filename = "test/testcases/malgo/Seq.mlg"

@_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @_M28let_x24fda_x5Fclosure_x24e3040test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#$fc7_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#$fc7_0", i64 4
  %"int32#$fc7_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$c94_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$fc7_0.val", ptr %"let$715$c94_capture_0.i", align 4
  store ptr %"let$715$c94_capture_0.i", ptr %5, align 8
  %"let$715$c94_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M35let_x24715_x24c94_x5Fclosure_x24e3140test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal, ptr %"let$715$c94_func_0.i", align 8
  %6 = tail call i32 @malgo_add_int32_t(i32 %"int32#$fc7_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal i32 @_M35let_x24715_x24c94_x5Fclosure_x24e3140test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %"p$fc9$fd3$c77_0" = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %"p$fc9$fd3$c77_0", i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 2, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$fda_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %2, ptr %"let$fda_capture_0.i.i", align 8
  store ptr %"let$fda_capture_0.i.i", ptr %6, align 8
  %"let$fda_func_0.i.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M28let_x24fda_x5Fclosure_x24e3040test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal, ptr %"let$fda_func_0.i.i", align 8
  %"int32#$fc7_0.i.i" = load ptr, ptr %"let$fda_capture_0.i.i", align 8
  %7 = getelementptr i8, ptr %"int32#$fc7_0.i.i", i64 4
  %"int32#$fc7_0.val.i.i" = load i32, ptr %7, align 4
  %.val.i.i = load i32, ptr %5, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$c94_capture_0.i.i.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$fc7_0.val.i.i", ptr %"let$715$c94_capture_0.i.i.i", align 4
  store ptr %"let$715$c94_capture_0.i.i.i", ptr %8, align 8
  %"let$715$c94_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @_M35let_x24715_x24c94_x5Fclosure_x24e3140test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal, ptr %"let$715$c94_func_0.i.i.i", align 8
  %9 = tail call i32 @malgo_add_int32_t(i32 %"int32#$fc7_0.val.i.i", i32 %.val.i.i)
  %10 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %10, align 1
  %11 = getelementptr { i8, { i32 } }, ptr %10, i64 0, i32 1, i32 0
  store i32 %9, ptr %11, align 4
  %12 = tail call ptr @malgo_int32_t_to_string(i32 %9)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %12, ptr %14, align 8
  %15 = tail call ptr @malgo_print_string(ptr %12)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$fda_capture_0.i1.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %2, ptr %"let$fda_capture_0.i1.i", align 8
  store ptr %"let$fda_capture_0.i1.i", ptr %16, align 8
  %"let$fda_func_0.i2.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M28let_x24fda_x5Fclosure_x24e3040test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal, ptr %"let$fda_func_0.i2.i", align 8
  %"int32#$fc7_0.i3.i" = load ptr, ptr %"let$fda_capture_0.i1.i", align 8
  %17 = getelementptr i8, ptr %"int32#$fc7_0.i3.i", i64 4
  %"int32#$fc7_0.val.i4.i" = load i32, ptr %17, align 4
  %.val.i5.i = load i32, ptr %5, align 4
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$715$c94_capture_0.i.i6.i" = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#$fc7_0.val.i4.i", ptr %"let$715$c94_capture_0.i.i6.i", align 4
  store ptr %"let$715$c94_capture_0.i.i6.i", ptr %18, align 8
  %"let$715$c94_func_0.i.i7.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @_M35let_x24715_x24c94_x5Fclosure_x24e3140test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8Internal, ptr %"let$715$c94_func_0.i.i7.i", align 8
  %19 = tail call i32 @malgo_add_int32_t(i32 %"int32#$fc7_0.val.i4.i", i32 %.val.i5.i)
  %20 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { i32 } }, ptr %20, i64 0, i32 1, i32 0
  store i32 %19, ptr %21, align 4
  store ptr %20, ptr @_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %22, align 1
  %23 = load ptr, ptr @_M17executeWhenLoaded40test_x2Ftestcases_x2Fmalgo_x2FSeq_x2Emlg8External, align 8
  %24 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1
  %25 = load i32, ptr %24, align 4
  %26 = tail call ptr @malgo_int32_t_to_string(i32 %25)
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %26, ptr %28, align 8
  %29 = tail call ptr @malgo_print_string(ptr %26)
  ret i32 0
}
