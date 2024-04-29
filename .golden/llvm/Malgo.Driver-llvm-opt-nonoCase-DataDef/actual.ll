; ModuleID = '/workspaces/malgo/.malgo-work/DataDef.ll'
source_filename = "./test/testcases/malgo/DataDef.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"DataDef.Int#"(ptr nocapture nofree readnone %0, i64 %"DataDef.$p_18_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 %"DataDef.$p_18_0", ptr %3, align 4
  ret ptr %2
}

define internal ptr @DataDef.malgo_int64_t_to_string(ptr nocapture nofree readnone %0, i64 %"DataDef.$p_29_0") {
  %2 = tail call ptr @malgo_int64_t_to_string(i64 %"DataDef.$p_29_0")
  ret ptr %2
}

define internal ptr @DataDef.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"DataDef.$p_30_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"DataDef.$p_30_0")
  ret ptr %2
}

define internal ptr @"DataDef.#fun_closure_44"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_DataDef.Int#_0":
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %malgo_int64_t_to_string_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %malgo_int64_t_to_string_0 = load ptr, ptr %malgo_int64_t_to_string_addr_0, align 8
  %2 = getelementptr { i8, { i64 } }, ptr %1, i64 0, i32 1
  %3 = load i64, ptr %2, align 4
  %4 = load ptr, ptr %malgo_int64_t_to_string_0, align 8
  %5 = getelementptr { ptr, ptr }, ptr %malgo_int64_t_to_string_0, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, i64 %3)
  %8 = load ptr, ptr %malgo_print_string_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @DataDef.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %malgo_int64_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @DataDef.malgo_int64_t_to_string, ptr %malgo_int64_t_to_string_func_0.i, align 8
  %malgo_int64_t_to_string_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %5, ptr %malgo_int64_t_to_string_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"DataDef.#fun_closure_44", ptr %fun_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"DataDef.Int#", ptr %"Int#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i64 } }, ptr %7, i64 0, i32 1, i32 0
  store i64 1, ptr %8, align 4
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %fun_func_0.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %7)
  ret i32 0
}
