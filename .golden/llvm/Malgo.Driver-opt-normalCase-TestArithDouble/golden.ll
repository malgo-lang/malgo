; ModuleID = '.malgo-work/test/testcases/malgo/TestArithDouble.ll'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

declare void @GC_init() local_unnamed_addr

declare double @malgo_add_double(double, double) local_unnamed_addr

declare double @malgo_mul_double(double, double) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4107(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4108(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4117(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %d_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %d_0, double %1)
  ret double %3
}

define internal double @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4118(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %d_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %d_0, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4117, ptr %let_func_0.i, align 8
  %d_0.i.i = load double, ptr %let_capture_0.i, align 8
  %4 = tail call double @malgo_mul_double(double %d_0.i.i, double %1)
  ret double %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double 5.000000e-01, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4107, ptr %let_func_0.i.i, align 8
  %p_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %4 = tail call double @malgo_add_double(double %p_0.i.i, double noundef 0.000000e+00)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %4, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %5, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4118, ptr %let_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %4, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %6, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M14let_x5Fclosure52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg12Internal4108, ptr %let_func_0.i2.i, align 8
  %p_0.i3.i = load double, ptr %let_capture_0.i1.i, align 8
  %7 = tail call double @malgo_mul_double(double %p_0.i3.i, double noundef 5.000000e-01)
  %8 = tail call ptr @malgo_double_to_string(double %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %8, ptr %10, align 8
  %11 = tail call ptr @malgo_print_string(ptr %8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  %14 = tail call ptr @malgo_newline(ptr noundef nonnull %13)
  ret i32 0
}
