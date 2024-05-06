; ModuleID = '.malgo-work/test/testcases/malgo/Double.ll'
source_filename = "test/testcases/malgo/Double.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str190 = unnamed_addr constant [1 x i8] zeroinitializer
@str210 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Double#"(ptr nocapture nofree readnone %0, double %"runtime/malgo/Builtin.mlg.$p_1798_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { double } }, ptr %2, i64 0, i32 1, i32 0
  store double %"runtime/malgo/Builtin.mlg.$p_1798_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.String#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string"(ptr nocapture nofree readnone %0, double %"runtime/malgo/Builtin.mlg.$p_2158_0") {
  %2 = tail call ptr @malgo_double_to_string(double %"runtime/malgo/Builtin.mlg.$p_2158_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.toStringDouble#"(ptr nocapture nofree readnone %0, double %"runtime/malgo/Builtin.mlg.$x_2193_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_double_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string", ptr %malgo_double_to_string_func_0, align 8
  %3 = tail call ptr @malgo_double_to_string(double %"runtime/malgo/Builtin.mlg.$x_2193_0")
  ret ptr %3
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.toStringDouble"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Builtin.mlg.$double#_2195_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Double#_0":
  %1 = getelementptr { i8, { double } }, ptr %"runtime/malgo/Builtin.mlg.$double#_2195_0", i64 0, i32 1
  %2 = load double, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringDouble#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringDouble#", ptr %"toStringDouble#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_double_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string", ptr %malgo_double_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_double_to_string(double %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString#"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$x_2399_0")
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Builtin.mlg.$string#_2401_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @"runtime/malgo/Prelude.mlg.putStr"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"runtime/malgo/Prelude.mlg.$str_723_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Prelude.mlg.$str_723_0", i64 0, i32 1
  %4 = load ptr, ptr %3, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %7
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast", ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Double#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Double#", ptr %"Double#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { double } }, ptr %7, i64 0, i32 1, i32 0
  store double 3.140000e+00, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %toStringDouble_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringDouble", ptr %toStringDouble_func_0.i, align 8
  %10 = load double, ptr %8, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"toStringDouble#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.toStringDouble#", ptr %"toStringDouble#_func_0.i.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %malgo_double_to_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_double_to_string", ptr %malgo_double_to_string_func_0.i.i.i, align 8
  %13 = tail call ptr @malgo_double_to_string(double %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"String#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i.i", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %putStr_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStr", ptr %putStr_func_0.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i.i, align 8
  %19 = load ptr, ptr %16, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %22 = tail call ptr @malgo_print_string(ptr %19)
  ret i32 0
}
