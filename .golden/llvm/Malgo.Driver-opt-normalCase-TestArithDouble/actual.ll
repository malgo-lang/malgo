; ModuleID = '.malgo-work/test/testcases/malgo/TestArithDouble.ll'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

declare void @GC_init() local_unnamed_addr

declare double @malgo_add_double(double, double) local_unnamed_addr

declare double @malgo_mul_double(double, double) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal double @_M29let_x2475d_x5Fclosure_x24100b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %"p$758_0" = load double, ptr %0, align 8
  %3 = tail call double @malgo_add_double(double %"p$758_0", double %1)
  ret double %3
}

define internal double @_M29let_x24769_x5Fclosure_x24100c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %"p$764_0" = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %"p$764_0", double %1)
  ret double %3
}

define internal double @_M36let_x24769_x24ffd_x5Fclosure_x24101552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %"d$9b$f9e_0" = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %"d$9b$f9e_0", double %1)
  ret double %3
}

define internal double @_M36let_x24a2b_x24fe9_x5Fclosure_x24101652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %"d$9b$f9e_0" = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$769$ffd_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"d$9b$f9e_0", ptr %"let$769$ffd_capture_0.i", align 8
  store ptr %"let$769$ffd_capture_0.i", ptr %3, align 8
  %"let$769$ffd_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M36let_x24769_x24ffd_x5Fclosure_x24101552test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769$ffd_func_0.i", align 8
  %"d$9b$f9e_0.i.i" = load double, ptr %"let$769$ffd_capture_0.i", align 8
  %4 = tail call double @malgo_mul_double(double %"d$9b$f9e_0.i.i", double %1)
  ret double %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$75d_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store double 5.000000e-01, ptr %"let$75d_capture_0.i.i", align 8
  store ptr %"let$75d_capture_0.i.i", ptr %3, align 8
  %"let$75d_func_0.i.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @_M29let_x2475d_x5Fclosure_x24100b52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$75d_func_0.i.i", align 8
  %"p$758_0.i.i" = load double, ptr %"let$75d_capture_0.i.i", align 8
  %4 = tail call double @malgo_add_double(double %"p$758_0.i.i", double noundef 0.000000e+00)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$a2b$fe9_capture_0.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %4, ptr %"let$a2b$fe9_capture_0.i", align 8
  store ptr %"let$a2b$fe9_capture_0.i", ptr %5, align 8
  %"let$a2b$fe9_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @_M36let_x24a2b_x24fe9_x5Fclosure_x24101652test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$a2b$fe9_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %"let$769_capture_0.i.i" = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %4, ptr %"let$769_capture_0.i.i", align 8
  store ptr %"let$769_capture_0.i.i", ptr %6, align 8
  %"let$769_func_0.i.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @_M29let_x24769_x5Fclosure_x24100c52test_x2Ftestcases_x2Fmalgo_x2FTestArithDouble_x2Emlg8Internal, ptr %"let$769_func_0.i.i", align 8
  %"p$764_0.i.i" = load double, ptr %"let$769_capture_0.i.i", align 8
  %7 = tail call double @malgo_mul_double(double %"p$764_0.i.i", double noundef 5.000000e-01)
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
