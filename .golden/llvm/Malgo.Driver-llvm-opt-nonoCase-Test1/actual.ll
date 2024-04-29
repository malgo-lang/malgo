; ModuleID = '/workspaces/malgo/.malgo-work/Test1.ll'
source_filename = "./test/testcases/malgo/Test1.mlg"

@str82 = unnamed_addr constant [5 x i8] c"True\00"
@str84 = unnamed_addr constant [6 x i8] c"False\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @Test1.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Test1.$p_41_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Test1.$p_41_0")
  ret ptr %2
}

define internal ptr @"Test1.#let_closure_80"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr nocapture nofree readonly %1) {
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

define internal ptr @"Test1.#let_closure_79"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %true_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %let_capture_0, align 8
  %true_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %true_0, ptr %true_1, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Test1.#let_closure_80", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @Test1.if(ptr nocapture nofree readnone %0, ptr nofree %"Test1.$true_42_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Test1.$true_42_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Test1.#let_closure_79", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Test1.#fun_closure_81"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %malgo_print_string_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str82)
  ret ptr %6
}

define internal ptr @"Test1.#fun_closure_83"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %malgo_print_string_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %malgo_print_string_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %malgo_print_string_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr nonnull @str84)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %if_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Test1.if, ptr %if_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Test1.#let_closure_79", ptr %let_func_0.i.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Test1.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  store ptr %7, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Test1.#fun_closure_81", ptr %fun_func_0.i, align 8
  %8 = load ptr, ptr %5, align 8
  %9 = load ptr, ptr %let_func_0.i.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %6)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %malgo_print_string_func_1.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @Test1.malgo_print_string, ptr %malgo_print_string_func_1.i, align 8
  store ptr %12, ptr %fun_capture_2.i, align 8
  store ptr %fun_capture_2.i, ptr %11, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Test1.#fun_closure_83", ptr %fun_func_1.i, align 8
  %13 = load ptr, ptr %10, align 8
  %14 = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  %15 = load ptr, ptr %14, align 8
  %16 = tail call ptr %15(ptr %13, ptr nonnull %11)
  ret i32 0
}
