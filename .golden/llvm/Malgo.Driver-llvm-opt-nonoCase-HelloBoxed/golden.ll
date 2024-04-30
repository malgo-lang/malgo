; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/HelloBoxed.ll'
source_filename = "test/testcases/malgo/HelloBoxed.mlg"

@str39 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"HelloBoxed.String#"(ptr nocapture nofree readnone %0, ptr nofree %"HelloBoxed.$p_21_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"HelloBoxed.$p_21_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @HelloBoxed.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"HelloBoxed.$p_23_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"HelloBoxed.$p_23_0")
  ret ptr %2
}

define internal ptr @HelloBoxed.malgo_newline(ptr nocapture nofree readnone %0, ptr %"HelloBoxed.$p_24_0") {
  %2 = tail call ptr @malgo_newline(ptr %"HelloBoxed.$p_24_0")
  ret ptr %2
}

define internal ptr @HelloBoxed.putStrLn(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"HelloBoxed.$string#_27_0") {
"switch_branch_HelloBoxed.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"HelloBoxed.$string#_27_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @HelloBoxed.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %4 = tail call ptr @malgo_print_string(ptr %2)
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_newline_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @HelloBoxed.malgo_newline, ptr %malgo_newline_func_0, align 8
  %7 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %7
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"HelloBoxed.String#", ptr %"String#_func_0.i", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr @str39, ptr %5, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @HelloBoxed.putStrLn, ptr %putStrLn_func_0.i, align 8
  %7 = load ptr, ptr %5, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @HelloBoxed.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %9 = tail call ptr @malgo_print_string(ptr %7)
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %malgo_newline_func_0.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @HelloBoxed.malgo_newline, ptr %malgo_newline_func_0.i.i, align 8
  %12 = tail call ptr @malgo_newline(ptr noundef nonnull %10)
  ret i32 0
}
