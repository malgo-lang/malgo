; ModuleID = '/workspaces/malgo/.malgo-work/With.ll'
source_filename = "./test/testcases/malgo/With.mlg"

@str2966 = unnamed_addr constant [4 x i8] c"end\00"
@str2970 = unnamed_addr constant [4 x i8] c"foo\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"With.#fun_closure_2965"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
"switch_branch_Tuple#_0":
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr @str2966, ptr %3, align 8
  %4 = tail call ptr @malgo_print_string(ptr noundef nonnull @str2966)
  ret ptr %4
}

define internal ptr @"With.#let_closure_2967"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = load ptr, ptr %1, align 8
  %5 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull %3)
  %8 = load ptr, ptr %cast_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %cast_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  ret ptr %11
}

define internal ptr @"With.#let_closure_2971"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
"switch_branch_Builtin.String#_0":
  %d_0 = load ptr, ptr %0, align 8
  %2 = getelementptr { i8, { ptr } }, ptr %d_0, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = tail call ptr @malgo_print_string(ptr %3)
  %5 = load ptr, ptr %1, align 8
  %6 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr %d_0)
  ret ptr %8
}

define internal ptr @"With.#fun_closure_2972"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_Builtin.String#_0":
  %2 = getelementptr { i8, { ptr } }, ptr %1, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %4
}

define internal ptr @"With.#fun_closure_2969"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str2970, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %5, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"With.#let_closure_2971", ptr %let_func_0, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %6, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"With.#fun_closure_2972", ptr %fun_func_0, align 8
  %7 = load ptr, ptr %5, align 8
  %8 = load ptr, ptr %let_func_0, align 8
  %9 = tail call ptr %8(ptr %7, ptr nonnull %6)
  ret ptr %9
}

define internal ptr @"With.#fun_closure_2968"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"With.#fun_closure_2969", ptr %fun_func_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %3, align 8
  %6 = load ptr, ptr %fun_func_0, align 8
  %7 = tail call ptr %6(ptr %5, ptr nonnull %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %fun_func_0, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  ret ptr %11
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"With.#fun_closure_2965", ptr %fun_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"With.#let_closure_2967", ptr %let_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %5, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"With.#fun_closure_2968", ptr %fun_func_1.i, align 8
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %let_func_0.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  ret i32 0
}
