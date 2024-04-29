; ModuleID = '/workspaces/malgo/.malgo-work/HelloImport.ll'
source_filename = "./test/testcases/malgo/HelloImport.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str37 = unnamed_addr constant [1 x i8] zeroinitializer
@str57 = unnamed_addr constant [10 x i8] c"no branch\00"
@str234 = unnamed_addr constant [6 x i8] c"hello\00"
@str236 = unnamed_addr constant [7 x i8] c" world\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"HelloImport.#let_closure_43"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"Prelude.|>"(ptr nocapture nofree readnone %0, ptr nofree %"Prelude.$x_699_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Prelude.$x_699_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"HelloImport.#let_closure_43", ptr %let_func_0, align 8
  ret ptr %2
}

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

define internal ptr @Prelude.putStr(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"Prelude.$str_723_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0, align 8
  %3 = getelementptr { i8, { ptr } }, ptr %"Prelude.$str_723_0", i64 0, i32 1
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
  ret ptr %7
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

define internal ptr @Builtin.malgo_newline(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2161_0") {
  %2 = tail call ptr @malgo_newline(ptr %"Builtin.$p_2161_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
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

define internal ptr @"HelloImport.#fun_closure_235"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %"String#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"String#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str236)
  %7 = load ptr, ptr %1, align 8
  %8 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %9 = load ptr, ptr %8, align 8
  %10 = tail call ptr %9(ptr %7, ptr %6)
  ret ptr %10
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
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str234, ptr %8, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %putStr_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Prelude.putStr, ptr %putStr_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %printString_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i.i, align 8
  %11 = load ptr, ptr %8, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"printString#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i.i.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %malgo_print_string_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i.i.i, align 8
  %14 = tail call ptr @malgo_print_string(ptr %11)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @Prelude.putStrLn, ptr %putStrLn_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"Prelude.|>", ptr %"|>_func_0.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %17, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"HelloImport.#let_closure_43", ptr %let_func_0.i.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %"String#_func_1.i" = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_1.i", align 8
  store ptr %19, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %18, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"HelloImport.#fun_closure_235", ptr %fun_func_0.i, align 8
  %20 = load ptr, ptr %17, align 8
  %21 = load ptr, ptr %let_func_0.i.i, align 8
  %22 = tail call ptr %21(ptr %20, ptr nonnull %18)
  ret i32 0
}
