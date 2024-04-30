; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestArithDouble.ll'
source_filename = "test/testcases/malgo/TestArithDouble.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str170 = unnamed_addr constant [1 x i8] zeroinitializer
@str190 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare double @malgo_add_double(double, double) local_unnamed_addr

declare double @malgo_mul_double(double, double) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Prelude.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"Prelude.$str_716_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_716_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %newline_func_0 = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Builtin.newline, ptr %newline_func_0, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_newline_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0.i, align 8
  %12 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  ret ptr %12
}

define internal noundef ptr @"Builtin.String#"(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_208"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_add_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_add_double(ptr nocapture nofree readnone %0, double %"Builtin.$p_1880_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"Builtin.$p_1880_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_208", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_210"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %p_0 = load double, ptr %0, align 8
  %3 = tail call double @malgo_mul_double(double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_mul_double(ptr nocapture nofree readnone %0, double %"Builtin.$p_1892_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"Builtin.$p_1892_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_210", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_double_to_string(ptr nocapture nofree readnone %0, double %"Builtin.$p_2158_0") {
  %2 = tail call ptr @malgo_double_to_string(double %"Builtin.$p_2158_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_newline(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2161_0") {
  %2 = tail call ptr @malgo_newline(ptr %"Builtin.$p_2161_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringDouble#"(ptr nocapture nofree readnone %0, double %"Builtin.$x_2193_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_double_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_double_to_string, ptr %malgo_double_to_string_func_0, align 8
  %3 = tail call ptr @malgo_double_to_string(double %"Builtin.$x_2193_0")
  ret ptr %3
}

define internal ptr @"Builtin.printString#"(ptr nocapture nofree readnone %0, ptr %"Builtin.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Builtin.$x_2399_0")
  ret ptr %3
}

define internal ptr @Builtin.printString(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Builtin.$string#_2401_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @Builtin.newline(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"Builtin.$__2420_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0, align 8
  %4 = tail call ptr @malgo_newline(ptr noundef nonnull %2)
  ret ptr %4
}

define internal double @"TestArithDouble.#let_closure_277"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_mul_double_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_mul_double, ptr %malgo_mul_double_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_210", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %5 = tail call double @malgo_mul_double(double %p_0.i.i, double %1)
  ret double %5
}

define internal ptr @"Builtin.mulDouble#"(ptr nocapture nofree readnone %0, double %"Builtin.$x_2592_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"Builtin.$x_2592_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_277", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_365"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_add_double_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_add_double, ptr %malgo_add_double_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_208", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %5 = tail call double @malgo_add_double(double %p_0.i.i, double %1)
  ret double %5
}

define internal ptr @"Builtin.addDouble#"(ptr nocapture nofree readnone %0, double %"Builtin.$x_4091_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"Builtin.$x_4091_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_365", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @TestArithDouble.show(ptr nocapture nofree readnone %0, double %"TestArithDouble.$x_91_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %"toStringDouble#_func_0" = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Builtin.toStringDouble#", ptr %"toStringDouble#_func_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_double_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_double_to_string, ptr %malgo_double_to_string_func_0.i, align 8
  %4 = tail call ptr @malgo_double_to_string(double %"TestArithDouble.$x_91_0")
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %6 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %4, ptr %7, align 8
  ret ptr %6
}

define internal double @"TestArithDouble.#let_closure_369"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"addDouble#_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.addDouble#", ptr %"addDouble#_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_365", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_add_double_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @Builtin.malgo_add_double, ptr %malgo_add_double_func_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_208", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i, align 8
  %7 = tail call double @malgo_add_double(double %p_0.i.i.i.i, double %1)
  ret double %7
}

define internal ptr @"TestArithDouble.+"(ptr nocapture nofree readnone %0, double %"TestArithDouble.$x_128_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"TestArithDouble.$x_128_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_369", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"TestArithDouble.#let_closure_370"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, double %1) {
  %x_0 = load double, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"mulDouble#_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.mulDouble#", ptr %"mulDouble#_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_277", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load double, ptr %let_capture_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_mul_double_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @Builtin.malgo_mul_double, ptr %malgo_mul_double_func_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_210", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i, align 8
  %7 = tail call double @malgo_mul_double(double %p_0.i.i.i.i, double %1)
  ret double %7
}

define internal ptr @"TestArithDouble.*"(ptr nocapture nofree readnone %0, double %"TestArithDouble.$x_140_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"TestArithDouble.$x_140_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_370", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @TestArithDouble.f(ptr nocapture nofree readnone %0, double %"TestArithDouble.$x_152_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %"+_func_0" = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestArithDouble.+", ptr %"+_func_0", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %"TestArithDouble.$x_152_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_369", ptr %let_func_0.i, align 8
  %x_0.i = load double, ptr %let_capture_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"addDouble#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Builtin.addDouble#", ptr %"addDouble#_func_0.i.i", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %5, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_365", ptr %let_func_0.i.i.i, align 8
  %x_0.i.i.i = load double, ptr %let_capture_0.i.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_add_double_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Builtin.malgo_add_double, ptr %malgo_add_double_func_0.i.i.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i.i, ptr %let_capture_0.i.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i.i, ptr %7, align 8
  %let_func_0.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_208", ptr %let_func_0.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i = load double, ptr %let_capture_0.i.i.i.i.i, align 8
  %8 = tail call double @malgo_add_double(double %p_0.i.i.i.i.i, double noundef 0.000000e+00)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %"*_func_0" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestArithDouble.*", ptr %"*_func_0", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %8, ptr %let_capture_0.i1, align 8
  store ptr %let_capture_0.i1, ptr %10, align 8
  %let_func_0.i2 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_370", ptr %let_func_0.i2, align 8
  %x_0.i3 = load double, ptr %let_capture_0.i1, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"mulDouble#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Builtin.mulDouble#", ptr %"mulDouble#_func_0.i.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i3, ptr %let_capture_0.i.i.i4, align 8
  store ptr %let_capture_0.i.i.i4, ptr %12, align 8
  %let_func_0.i.i.i5 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_277", ptr %let_func_0.i.i.i5, align 8
  %x_0.i.i.i6 = load double, ptr %let_capture_0.i.i.i4, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %malgo_mul_double_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.malgo_mul_double, ptr %malgo_mul_double_func_0.i.i.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store double %x_0.i.i.i6, ptr %let_capture_0.i.i.i.i.i7, align 8
  store ptr %let_capture_0.i.i.i.i.i7, ptr %14, align 8
  %let_func_0.i.i.i.i.i8 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"TestArithDouble.#let_closure_210", ptr %let_func_0.i.i.i.i.i8, align 8
  %p_0.i.i.i.i.i9 = load double, ptr %let_capture_0.i.i.i.i.i7, align 8
  %15 = tail call double @malgo_mul_double(double %p_0.i.i.i.i.i9, double %"TestArithDouble.$x_152_0")
  ret double %15
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @Builtin.undefined, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %f_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @TestArithDouble.f, ptr %f_func_0.i, align 8
  %7 = tail call double @TestArithDouble.f(ptr poison, double noundef 5.000000e-01)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %show_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @TestArithDouble.show, ptr %show_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %"toStringDouble#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"Builtin.toStringDouble#", ptr %"toStringDouble#_func_0.i.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %malgo_double_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Builtin.malgo_double_to_string, ptr %malgo_double_to_string_func_0.i.i.i, align 8
  %11 = tail call ptr @malgo_double_to_string(double %7)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr %11, ptr %14, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i.i, align 8
  %17 = load ptr, ptr %14, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %20 = tail call ptr @malgo_print_string(ptr %17)
  %21 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %21, align 1
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @Builtin.newline, ptr %newline_func_0.i.i, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %23, align 1
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %24, align 8
  %malgo_newline_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @Builtin.malgo_newline, ptr %malgo_newline_func_0.i.i.i, align 8
  %25 = tail call ptr @malgo_newline(ptr noundef nonnull %23)
  ret i32 0
}
