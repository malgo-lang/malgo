; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestLet.ll'
source_filename = "test/testcases/malgo/TestLet.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Builtin.Int32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal i32 @"TestLet.#let_closure_84"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_add_int32_t(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1808_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"Builtin.$p_1808_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestLet.#let_closure_84", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_int32_t_to_string(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$p_2155_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$x_2179_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$x_2179_0")
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

define internal i32 @"TestLet.#let_closure_249"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_add_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"TestLet.#let_closure_84", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %5
}

define internal ptr @"Builtin.addInt32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$x_4027_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"Builtin.$x_4027_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestLet.#let_closure_249", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"TestLet.#fun_closure_256"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_Builtin.Int32#_0":
  %"printString#_0" = load ptr, ptr %0, align 8
  %"toStringInt32#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"toStringInt32#_0" = load ptr, ptr %"toStringInt32#_addr_0", align 8
  %2 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = load ptr, ptr %"toStringInt32#_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, i32 %3)
  %8 = load ptr, ptr %"printString#_0", align 8
  %9 = getelementptr { ptr, ptr }, ptr %"printString#_0", i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
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
  %"addInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 1, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"TestLet.#let_closure_249", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_add_int32_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i.i.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %9, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestLet.#let_closure_84", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %10 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 noundef 2)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %13 = tail call ptr @malgo_int32_t_to_string(i32 %10)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %16 = tail call ptr @malgo_print_string(ptr %13)
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %"addInt32#_func_1.i" = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"Builtin.addInt32#", ptr %"addInt32#_func_1.i", align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 1, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %18, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"TestLet.#let_closure_249", ptr %let_func_0.i2.i, align 8
  %x_0.i3.i = load i32, ptr %let_capture_0.i1.i, align 4
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %malgo_add_int32_t_func_0.i.i4.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @Builtin.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i.i4.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i5.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i3.i, ptr %let_capture_0.i.i.i5.i, align 4
  store ptr %let_capture_0.i.i.i5.i, ptr %20, align 8
  %let_func_0.i.i.i6.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"TestLet.#let_closure_84", ptr %let_func_0.i.i.i6.i, align 8
  %p_0.i.i.i7.i = load i32, ptr %let_capture_0.i.i.i5.i, align 4
  %21 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i7.i, i32 noundef 2)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0.i", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1, i32 0
  store i32 %21, ptr %24, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %"printString#_func_1.i" = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_1.i", align 8
  store ptr %26, ptr %fun_capture_0.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %27, align 8
  %"toStringInt32#_func_1.i" = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_1.i", align 8
  %"toStringInt32#_0.i" = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %27, ptr %"toStringInt32#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %25, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"TestLet.#fun_closure_256", ptr %fun_func_0.i, align 8
  %"printString#_0.i.i" = load ptr, ptr %fun_capture_0.i, align 8
  %"toStringInt32#_0.i.i" = load ptr, ptr %"toStringInt32#_0.i", align 8
  %28 = load i32, ptr %24, align 4
  %29 = load ptr, ptr %"toStringInt32#_0.i.i", align 8
  %30 = getelementptr { ptr, ptr }, ptr %"toStringInt32#_0.i.i", i64 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = tail call ptr %31(ptr %29, i32 %28)
  %33 = load ptr, ptr %"printString#_0.i.i", align 8
  %34 = getelementptr { ptr, ptr }, ptr %"printString#_0.i.i", i64 0, i32 1
  %35 = load ptr, ptr %34, align 8
  %36 = tail call ptr %35(ptr %33, ptr %32)
  ret i32 0
}
