; ModuleID = '/workspaces/malgo/.malgo-work/ZeroArgs.ll'
source_filename = "./test/testcases/malgo/ZeroArgs.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@ZeroArgs.one = local_unnamed_addr global ptr undef
@str63 = unnamed_addr constant [1 x i8] zeroinitializer
@str83 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

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

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"ZeroArgs.#fun_closure_260"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %x_0 = load ptr, ptr %0, align 8
  ret ptr %x_0
}

define internal ptr @ZeroArgs.id(ptr nocapture nofree readnone %0, ptr nofree %"ZeroArgs.$x_36_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"ZeroArgs.$x_36_0", ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %2, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ZeroArgs.#fun_closure_260", ptr %fun_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %2, align 8
  %5 = load ptr, ptr %fun_func_0, align 8
  %6 = tail call ptr %5(ptr %4, ptr nonnull %3)
  ret ptr %6
}

define internal ptr @"ZeroArgs.#fun_closure_262"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %toStringInt32_0 = load ptr, ptr %0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %id_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %id_0 = load ptr, ptr %id_addr_0, align 8
  %3 = load ptr, ptr %id_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %id_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr @ZeroArgs.one, align 8
  %7 = tail call ptr %5(ptr %3, ptr %6)
  %8 = load ptr, ptr %toStringInt32_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %toStringInt32_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  %12 = load ptr, ptr %printString_0, align 8
  %13 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %14 = load ptr, ptr %13, align 8
  %15 = tail call ptr %14(ptr %12, ptr %11)
  ret ptr %15
}

define internal ptr @"ZeroArgs.#fun_closure_261"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(24) %0, ptr nocapture nofree readnone %1) {
  %toStringInt32_0 = load ptr, ptr %0, align 8
  %printString_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %printString_0 = load ptr, ptr %printString_addr_0, align 8
  %id_addr_0 = getelementptr { ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %id_0 = load ptr, ptr %id_addr_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 24)
  store ptr %toStringInt32_0, ptr %fun_capture_0, align 8
  %printString_1 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 1
  store ptr %printString_0, ptr %printString_1, align 8
  %id_1 = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0, i64 0, i32 2
  store ptr %id_0, ptr %id_1, align 8
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"ZeroArgs.#fun_closure_262", ptr %fun_func_0, align 8
  ret ptr %3
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
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0.i", align 8
  store ptr %6, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"ZeroArgs.#fun_closure_267", ptr %fun_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = load ptr, ptr %5, align 8
  %9 = load ptr, ptr %fun_func_0.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %7)
  store ptr %10, ptr @ZeroArgs.one, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 24)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0.i, align 8
  store ptr %13, ptr %fun_capture_0.i1, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i, align 8
  %printString_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 1
  store ptr %14, ptr %printString_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %id_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @ZeroArgs.id, ptr %id_func_0.i, align 8
  %id_0.i = getelementptr { ptr, ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 2
  store ptr %15, ptr %id_0.i, align 8
  store ptr %fun_capture_0.i1, ptr %12, align 8
  %fun_func_0.i2 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"ZeroArgs.#fun_closure_261", ptr %fun_func_0.i2, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  %17 = load ptr, ptr %12, align 8
  %18 = load ptr, ptr %fun_func_0.i2, align 8
  %19 = tail call ptr %18(ptr %17, ptr nonnull %16)
  %20 = load ptr, ptr %19, align 8
  %21 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  %22 = load ptr, ptr %21, align 8
  %23 = tail call ptr %22(ptr %20, ptr nonnull %11)
  ret i32 0
}

define internal ptr @"ZeroArgs.#fun_closure_267"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 1)
  ret ptr %6
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
