; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/FuncOverUnboxed.ll'
source_filename = "test/testcases/malgo/FuncOverUnboxed.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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

define internal ptr @FuncOverUnboxed.f(ptr nocapture nofree readnone %0, i64 %"FuncOverUnboxed.$unboxed_22_0") {
common.ret:
  %switch = icmp ult i64 %"FuncOverUnboxed.$unboxed_22_0", 2
  %spec.select = zext i1 %switch to i64
  %1 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %1, align 8
  %"toStringInt64#_func_2" = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  store ptr @"Builtin.toStringInt64#", ptr %"toStringInt64#_func_2", align 8
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int64_t_to_string_func_0.i3 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0.i3, align 8
  %3 = tail call ptr @malgo_int64_t_to_string(i64 %spec.select)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"printString#_func_2" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_2", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_print_string_func_0.i4 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i4, align 8
  %6 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %6
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
  store ptr @FuncOverUnboxed.f, ptr %f_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %"toStringInt64#_func_2.i.i" = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"Builtin.toStringInt64#", ptr %"toStringInt64#_func_2.i.i", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_int64_t_to_string_func_0.i3.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @Builtin.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0.i3.i.i, align 8
  %9 = tail call ptr @malgo_int64_t_to_string(i64 noundef 1)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"printString#_func_2.i.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_2.i.i", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_print_string_func_0.i4.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i4.i.i, align 8
  %12 = tail call ptr @malgo_print_string(ptr %9)
  ret i32 0
}
