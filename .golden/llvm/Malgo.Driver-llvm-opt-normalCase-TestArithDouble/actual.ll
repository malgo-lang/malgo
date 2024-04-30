; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestArithDouble.ll'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

declare void @GC_init() local_unnamed_addr

declare double @malgo_add_double(double, double) local_unnamed_addr

declare double @malgo_mul_double(double, double) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal double @"TestArithDouble.#let_closure_3148"(ptr nocapture nofree readnone %0, double %1) {
  %3 = tail call double @malgo_add_double(double noundef 5.000000e-01, double %1)
  ret double %3
}

define internal double @"TestArithDouble.#let_closure_3149"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %d_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %d_0, double %1)
  ret double %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3148", ptr %let_func_0.i, align 8
  %4 = tail call double @malgo_add_double(double noundef 5.000000e-01, double noundef 0.000000e+00)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %4, ptr %let_capture_2.i, align 8
  store ptr %let_capture_2.i, ptr %5, align 8
  %let_func_1.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_3149", ptr %let_func_1.i, align 8
  %d_0.i.i = load double, ptr %let_capture_2.i, align 8
  %6 = tail call double @malgo_mul_double(double %d_0.i.i, double noundef 5.000000e-01)
  %7 = tail call ptr @malgo_double_to_string(double %6)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = tail call ptr @malgo_print_string(ptr %7)
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  %13 = tail call ptr @malgo_newline(ptr noundef nonnull %12)
  ret i32 0
}
