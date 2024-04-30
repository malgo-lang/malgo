; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestArithDouble.ll'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str214 = unnamed_addr constant [1 x i8] zeroinitializer
@str365 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare double @malgo_add_double(double, double) local_unnamed_addr

declare double @malgo_mul_double(double, double) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal double @"TestArithDouble.#let_closure_193"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal double @"TestArithDouble.#let_closure_260"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_305", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %4, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_193", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i, align 8
  %5 = tail call double @malgo_mul_double(double %p_0.i.i.i.i, double %1)
  ret double %5
}

define internal double @"TestArithDouble.#let_closure_278"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_286", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %4, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_349", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i, align 8
  %5 = tail call double @malgo_add_double(double %p_0.i.i.i.i, double %1)
  ret double %5
}

define internal double @"TestArithDouble.#let_closure_286"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_349", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %4 = tail call double @malgo_add_double(double %p_0.i.i, double %1)
  ret double %4
}

define internal double @"TestArithDouble.#let_closure_305"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_193", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %4 = tail call double @malgo_mul_double(double %p_0.i.i, double %1)
  ret double %4
}

define internal double @"TestArithDouble.#let_closure_349"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double 5.000000e-01, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %5, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_278", ptr %let_func_0.i.i.i, align 8
  %x_0.i.i.i = load double, ptr %let_capture_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i.i, ptr %let_capture_0.i.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_286", ptr %let_func_0.i.i.i.i.i, align 8
  %x_0.i.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_349", ptr %let_func_0.i.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i.i.i.i, align 8
  %8 = tail call double @malgo_add_double(double %p_0.i.i.i.i.i.i.i, double noundef 0.000000e+00)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %8, ptr %let_capture_0.i1.i.i, align 8
  store ptr %let_capture_0.i1.i.i, ptr %9, align 8
  %let_func_0.i2.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_260", ptr %let_func_0.i2.i.i, align 8
  %x_0.i3.i.i = load double, ptr %let_capture_0.i1.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i4.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i3.i.i, ptr %let_capture_0.i.i.i4.i.i, align 8
  store ptr %let_capture_0.i.i.i4.i.i, ptr %10, align 8
  %let_func_0.i.i.i5.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_305", ptr %let_func_0.i.i.i5.i.i, align 8
  %x_0.i.i.i6.i.i = load double, ptr %let_capture_0.i.i.i4.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i7.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i.i6.i.i, ptr %let_capture_0.i.i.i.i.i7.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i7.i.i, ptr %11, align 8
  %let_func_0.i.i.i.i.i8.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_193", ptr %let_func_0.i.i.i.i.i8.i.i, align 8
  %p_0.i.i.i.i.i9.i.i = load double, ptr %let_capture_0.i.i.i.i.i7.i.i, align 8
  %12 = tail call double @malgo_mul_double(double %p_0.i.i.i.i.i9.i.i, double noundef 5.000000e-01)
  %13 = tail call ptr @malgo_double_to_string(double %12)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %13, ptr %15, align 8
  %16 = tail call ptr @malgo_print_string(ptr %13)
  %17 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %17, align 1
  %18 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %18, align 1
  %19 = tail call ptr @malgo_newline(ptr noundef nonnull %18)
  ret i32 0
}
