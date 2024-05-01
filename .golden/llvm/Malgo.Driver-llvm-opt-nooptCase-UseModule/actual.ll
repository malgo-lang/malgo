; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/UseModule.ll'
source_filename = "test/testcases/malgo/UseModule.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str103 = unnamed_addr constant [1 x i8] zeroinitializer
@str134 = unnamed_addr constant [14 x i8] c"Hello, world!\00"
@str267 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_171"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %f_0.val = load ptr, ptr %f_0, align 8
  %3 = getelementptr i8, ptr %f_0, i64 8
  %f_0.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %f_0.val1(ptr %f_0.val, ptr %1)
  ret ptr %4
}

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Prelude.mlg.$i_773_0") {
  %2 = getelementptr i8, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i64 4
  %"runtime/malgo/Prelude.mlg.$i_773_0.val" = load i32, ptr %2, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"runtime/malgo/Prelude.mlg.$i_773_0.val")
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.succ"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/UseModule.mlg.$x_36_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/UseModule.mlg.$x_36_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %2, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_200", ptr %let_func_0.i, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  %5 = load ptr, ptr %2, align 8
  %6 = load ptr, ptr %let_func_0.i, align 8
  %7 = tail call ptr %6(ptr %5, ptr nonnull %3)
  ret ptr %7
}

define internal noundef ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_200"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %5, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_221", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_208", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_208"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_221"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_208", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.#fun_closure_239"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %0, ptr %1) {
  %f_0 = load ptr, ptr %0, align 8
  %g_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %g_0 = load ptr, ptr %g_addr_0, align 8
  %g_0.val = load ptr, ptr %g_0, align 8
  %3 = getelementptr i8, ptr %g_0, i64 8
  %g_0.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %g_0.val1(ptr %g_0.val, ptr %1)
  %5 = load ptr, ptr %f_0, align 8
  %6 = getelementptr { ptr, ptr }, ptr %f_0, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr %4)
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_256"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %f_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0, ptr %fun_capture_0.i, align 8
  %g_0.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %1, ptr %g_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#fun_closure_239", ptr %fun_func_0.i, align 8
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str134, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr noundef nonnull @str134)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_newline(ptr noundef nonnull %9)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %12, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_171", ptr %let_func_0.i.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %succ_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.succ", ptr %succ_func_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %13, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %14, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_256", ptr %let_func_0.i2.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %succ_func_1.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.succ", ptr %succ_func_1.i, align 8
  %f_0.i.i = load ptr, ptr %let_capture_0.i1.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %f_0.i.i, ptr %fun_capture_0.i.i.i, align 8
  %g_0.i.i.i = getelementptr { ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %15, ptr %g_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %16, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#fun_closure_239", ptr %fun_func_0.i.i.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1, i32 0
  store i32 1, ptr %18, align 4
  %19 = load ptr, ptr %16, align 8
  %20 = load ptr, ptr %fun_func_0.i.i.i, align 8
  %21 = tail call ptr %20(ptr %19, ptr nonnull %17)
  %22 = load ptr, ptr %12, align 8
  %23 = load ptr, ptr %let_func_0.i.i, align 8
  %24 = tail call ptr %23(ptr %22, ptr %21)
  ret i32 0
}
