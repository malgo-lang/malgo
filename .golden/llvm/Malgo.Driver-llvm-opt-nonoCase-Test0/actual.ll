; ModuleID = '/workspaces/malgo/.malgo-work/Test0.ll'
source_filename = "./test/testcases/malgo/Test0.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Builtin.Int64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_1794_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"Builtin.$p_1794_0", ptr %3, align 4
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_int64_t_to_string(ptr nocapture nofree readnone %0, i64 %"Builtin.$p_2156_0") {
  %2 = tail call ptr @malgo_int64_t_to_string(i64 %"Builtin.$p_2156_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt64#"(ptr nocapture nofree readnone %0, i64 %"Builtin.$x_2172_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int64_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int64_t_to_string(i64 %"Builtin.$x_2172_0")
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

define internal ptr @"Test0.#let_closure_255"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal ptr @"Test0.|>"(ptr nocapture nofree readnone %0, ptr nofree %"Test0.$x_50_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Test0.$x_50_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Test0.#let_closure_255", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test0.#fun_closure_256"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_Builtin.Int64#_0":
  %"printString#_0" = load ptr, ptr %0, align 8
  %"toStringInt64#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"toStringInt64#_0" = load ptr, ptr %"toStringInt64#_addr_0", align 8
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = load ptr, ptr %"toStringInt64#_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"toStringInt64#_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, i64 %3)
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
  %"Int64#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.Int64#", ptr %"Int64#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 1, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %"|>_func_0.i" = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"Test0.|>", ptr %"|>_func_0.i", align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %7, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Test0.#let_closure_255", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i", align 8
  store ptr %12, ptr %fun_capture_0.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"toStringInt64#_func_0.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Builtin.toStringInt64#", ptr %"toStringInt64#_func_0.i", align 8
  %"toStringInt64#_0.i" = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %13, ptr %"toStringInt64#_0.i", align 8
  store ptr %fun_capture_0.i, ptr %11, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Test0.#fun_closure_256", ptr %fun_func_0.i, align 8
  %14 = load ptr, ptr %10, align 8
  %15 = load ptr, ptr %let_func_0.i.i, align 8
  %16 = tail call ptr %15(ptr %14, ptr nonnull %11)
  ret i32 0
}
