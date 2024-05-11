; ModuleID = '.malgo-work/test/testcases/malgo/InlineFunction.ll'
source_filename = "test/testcases/malgo/InlineFunction.mlg"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_sub_int32_t(i32, i32) local_unnamed_addr

declare i32 @malgo_le_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3989(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %f_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3989, ptr %let_func_0.i.i, align 8
  %4 = load ptr, ptr %f_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %7, align 8
  %9 = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %1)
  ret ptr %11
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3990(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4000, ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %6, align 4
  %.val.i.i = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4004, ptr %let_func_0.i.i.i, align 8
  %8 = tail call i32 @malgo_le_int32_t(i32 %"int32#_0.val.i.i", i32 %.val.i.i)
  %cond.i.i.i = icmp eq i32 %8, 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i.i.i = zext i1 %cond.i.i.i to i8
  store i8 %spec.select.i.i.i, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %10, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3995, ptr %let_func_0.i2.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %11, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3991, ptr %fun_func_0.i, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = load ptr, ptr %let_func_0.i2.i, align 8
  %14 = tail call ptr %13(ptr %12, ptr nonnull %11)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0, ptr %fun_capture_2.i, align 8
  %n_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_2.i, i64 0, i32 1
  store ptr %1, ptr %n_0.i, align 8
  store ptr %fun_capture_2.i, ptr %15, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3992, ptr %fun_func_1.i, align 8
  %16 = load ptr, ptr %14, align 8
  %17 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr %18(ptr %16, ptr nonnull %15)
  ret ptr %19
}

define internal noundef ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3991(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3992(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readnone %1) {
  %f_0 = load ptr, ptr %0, align 8
  %n_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %n_0 = load ptr, ptr %n_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4001, ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %6 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %6, align 4
  %.val.i.i = load i32, ptr %4, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i, align 4
  store ptr %let_capture_0.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4003, ptr %let_func_0.i.i.i, align 8
  %8 = tail call i32 @malgo_sub_int32_t(i32 %"int32#_0.val.i.i", i32 %.val.i.i)
  %9 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1, i32 0
  store i32 %8, ptr %10, align 4
  %11 = load ptr, ptr %f_0, align 8
  %12 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %13 = load ptr, ptr %12, align 8
  %14 = tail call ptr %13(ptr %11, ptr nonnull %9)
  %15 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { i32 } }, ptr %15, i64 0, i32 1, i32 0
  store i32 2, ptr %16, align 4
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %n_0, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %17, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4001, ptr %let_func_0.i2.i, align 8
  %"int32#_0.i5.i" = load ptr, ptr %let_capture_0.i1.i, align 8
  %18 = getelementptr i8, ptr %"int32#_0.i5.i", i64 4
  %"int32#_0.val.i6.i" = load i32, ptr %18, align 4
  %.val.i7.i = load i32, ptr %16, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i8.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i6.i", ptr %let_capture_0.i.i8.i, align 4
  store ptr %let_capture_0.i.i8.i, ptr %19, align 8
  %let_func_0.i.i9.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4003, ptr %let_func_0.i.i9.i, align 8
  %20 = tail call i32 @malgo_sub_int32_t(i32 %"int32#_0.val.i6.i", i32 %.val.i7.i)
  %21 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { i32 } }, ptr %21, i64 0, i32 1, i32 0
  store i32 %20, ptr %22, align 4
  %23 = load ptr, ptr %f_0, align 8
  %24 = load ptr, ptr %12, align 8
  %25 = tail call ptr %24(ptr %23, ptr nonnull %21)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %26, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3999, ptr %let_func_0.i4.i, align 8
  %"int32#_0.i10.i" = load ptr, ptr %let_capture_0.i3.i, align 8
  %27 = getelementptr i8, ptr %"int32#_0.i10.i", i64 4
  %"int32#_0.val.i11.i" = load i32, ptr %27, align 4
  %28 = getelementptr i8, ptr %25, i64 4
  %.val.i12.i = load i32, ptr %28, align 4
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i13.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i11.i", ptr %let_capture_0.i.i13.i, align 4
  store ptr %let_capture_0.i.i13.i, ptr %29, align 8
  %let_func_0.i.i14.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4005, ptr %let_func_0.i.i14.i, align 8
  %30 = tail call i32 @malgo_add_int32_t(i32 %"int32#_0.val.i11.i", i32 %.val.i12.i)
  %31 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %31, align 1
  %32 = getelementptr { i8, { i32 } }, ptr %31, i64 0, i32 1, i32 0
  store i32 %30, ptr %32, align 4
  ret ptr %31
}

define internal ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3993(ptr nocapture nofree readnone %0, ptr nofree %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %1, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3990, ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3994(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %1, align 8
  %4 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %d_0)
  ret ptr %6
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3995(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %let_capture_0.i, align 8
  %true_0.i = getelementptr { ptr, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %true_0, ptr %true_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4002, ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal noundef ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3999(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4005, ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_add_int32_t(i32 %"int32#_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal noundef ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4000(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4004, ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_le_int32_t(i32 %"int32#_0.val", i32 %.val)
  %cond.i = icmp eq i32 %6, 1
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  %spec.select.i = zext i1 %cond.i to i8
  store i8 %spec.select.i, ptr %7, align 1
  ret ptr %7
}

define internal noundef ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4001(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4003, ptr %let_func_0.i, align 8
  %6 = tail call i32 @malgo_sub_int32_t(i32 %"int32#_0.val", i32 %.val)
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 %6, ptr %8, align 4
  ret ptr %7
}

define internal ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0) {
"switch_branch_String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %_M3str34runtime_x2Fmalgo_x2FPrelude_x2Emlg11Temporal716_0, i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_print_string(ptr %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4002(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %t_0 = load ptr, ptr %0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %true_0.val = load i8, ptr %true_0, align 1
  %switch.i = icmp eq i8 %true_0.val, 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %spec.select.i = select i1 %switch.i, ptr %1, ptr %t_0
  %4 = load ptr, ptr %spec.select.i, align 8
  %5 = getelementptr { ptr, ptr }, ptr %spec.select.i, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4003(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_sub_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4004(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_le_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal4005(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14fun_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3993, ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3989, ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 5, ptr %6, align 4
  %7 = load ptr, ptr %4, align 8
  %8 = load ptr, ptr %let_func_0.i.i, align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %5)
  %10 = getelementptr { i8, { i32 } }, ptr %9, i64 0, i32 1
  %11 = load i32, ptr %10, align 4
  %12 = tail call ptr @malgo_int32_t_to_string(i32 %11)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %12, ptr %14, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %13, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %15, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @_M14let_x5Fclosure51test_x2Ftestcases_x2Fmalgo_x2FInlineFunction_x2Emlg12Internal3994, ptr %let_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @_M8putStrLn34runtime_x2Fmalgo_x2FPrelude_x2Emlg8External, ptr %putStrLn_func_0.i, align 8
  %d_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %17 = getelementptr { i8, { ptr } }, ptr %d_0.i.i, i64 0, i32 1
  %18 = load ptr, ptr %17, align 8
  %19 = tail call ptr @malgo_print_string(ptr %18)
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %20, align 1
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_newline(ptr noundef nonnull %21)
  ret i32 0
}
