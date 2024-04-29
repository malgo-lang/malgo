; ModuleID = '/workspaces/malgo/.malgo-work/TestCast.ll'
source_filename = "./test/testcases/malgo/TestCast.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str59 = unnamed_addr constant [1 x i8] zeroinitializer
@str79 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_new_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_read_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Prelude.printInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"Prelude.$i_773_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0, align 8
  %3 = getelementptr { i8, { i32 } }, ptr %"Prelude.$i_773_0", i64 0, i32 1
  %4 = load i32, ptr %3, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_int32_t_to_string(i32 %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %15 = tail call ptr @malgo_print_string(ptr %12)
  ret ptr %15
}

define internal noundef ptr @"Builtin.Int32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %3, align 4
  ret ptr %2
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

define internal noundef ptr @Builtin.toStringInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int32#_2181_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
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

define internal ptr @"TestCast.#let_closure_256"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, ptr %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_new_vector(i64 %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @TestCast.malgo_new_vector(ptr nocapture nofree readnone %0, i64 %"TestCast.$p_27_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"TestCast.$p_27_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestCast.#let_closure_256", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"TestCast.#let_closure_257"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, ptr %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_read_vector(i64 %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @TestCast.malgo_read_vector(ptr nocapture nofree readnone %0, i64 %"TestCast.$p_33_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %"TestCast.$p_33_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"TestCast.#let_closure_257", ptr %let_func_0, align 8
  ret ptr %2
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
  %malgo_read_vector_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @TestCast.malgo_read_vector, ptr %malgo_read_vector_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 2, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"TestCast.#let_closure_257", ptr %let_func_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_new_vector_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @TestCast.malgo_new_vector, ptr %malgo_new_vector_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 10, ptr %let_capture_0.i1.i, align 4
  store ptr %let_capture_0.i1.i, ptr %9, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"TestCast.#let_closure_256", ptr %let_func_0.i2.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0.i", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 5, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %malgo_unsafe_cast_func_0.i1 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_0.i1, align 8
  %14 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %11)
  %15 = load ptr, ptr %9, align 8
  %16 = load ptr, ptr %let_func_0.i2.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr %14)
  %18 = load ptr, ptr %7, align 8
  %19 = load ptr, ptr %let_func_0.i.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr %17)
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %malgo_unsafe_cast_func_1.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_1.i, align 8
  %22 = tail call ptr @malgo_unsafe_cast(ptr %20)
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @Prelude.printInt32, ptr %printInt32_func_0.i, align 8
  %24 = tail call ptr @Prelude.printInt32(ptr poison, ptr nocapture nofree readonly %22)
  ret i32 0
}
