; ModuleID = '/workspaces/malgo/.malgo-work/TestNestedLetFunc.ll'
source_filename = "./test/testcases/malgo/TestNestedLetFunc.mlg"

@str29 = unnamed_addr constant [5 x i8] c"PASS\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

define internal ptr @TestNestedLetFunc.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"TestNestedLetFunc.$p_13_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"TestNestedLetFunc.$p_13_0")
  ret ptr %2
}

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"TestNestedLetFunc.#fun_closure_28"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  ret ptr %3
}

define internal ptr @"TestNestedLetFunc.#fun_closure_27"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestNestedLetFunc.#fun_closure_28", ptr %fun_func_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %3, align 8
  %6 = load ptr, ptr %fun_func_0, align 8
  %7 = tail call ptr %6(ptr %5, ptr nonnull %4)
  ret ptr %7
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"TestNestedLetFunc.#fun_closure_27", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @TestNestedLetFunc.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str29)
  ret i32 0
}
