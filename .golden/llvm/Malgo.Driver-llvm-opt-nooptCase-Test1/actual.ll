; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Test1.ll'
source_filename = "test/testcases/malgo/Test1.mlg"

@str85 = unnamed_addr constant [5 x i8] c"True\00"
@str86 = unnamed_addr constant [6 x i8] c"False\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_83"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_print_string(ptr noundef nonnull @str85)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_84"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_print_string(ptr noundef nonnull @str86)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#let_closure_87"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %let_capture_0.i, align 8
  %true_0.i = getelementptr { ptr, ptr }, ptr %let_capture_0.i, i64 0, i32 1
  store ptr %true_0, ptr %true_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#let_closure_88", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#let_closure_88"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
  %t_0 = load ptr, ptr %0, align 8
  %true_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %true_0 = load ptr, ptr %true_addr_0, align 8
  %true_0.val = load i8, ptr %true_0, align 1
  %switch.i = icmp eq i8 %true_0.val, 0
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %spec.select.i = select i1 %switch.i, ptr %t_0, ptr %1
  %4 = load ptr, ptr %spec.select.i, align 8
  %5 = getelementptr { ptr, ptr }, ptr %spec.select.i, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  ret ptr %7
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#let_closure_87", ptr %let_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_83", ptr %fun_func_0.i, align 8
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %let_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %9, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_84", ptr %fun_func_1.i, align 8
  %10 = load ptr, ptr %8, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr nonnull %9)
  ret i32 0
}
