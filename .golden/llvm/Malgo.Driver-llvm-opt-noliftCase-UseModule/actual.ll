; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/UseModule.ll'
source_filename = "test/testcases/malgo/UseModule.mlg"

@str2938 = unnamed_addr constant [14 x i8] c"Hello, world!\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"runtime/malgo/Prelude.mlg.printInt32"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"runtime/malgo/Prelude.mlg.$i_773_0") {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"runtime/malgo/Prelude.mlg.$i_773_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %3, ptr %5, align 8
  %6 = tail call ptr @malgo_print_string(ptr %3)
  ret ptr %6
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/UseModule.mlg.#let_closure_2936"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal noundef ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
"switch_branch_runtime/malgo/Builtin.mlg.Int32#_0":
  %"int32#_0" = load ptr, ptr %0, align 8
  %2 = getelementptr { i8, { i32 } }, ptr %"int32#_0", i64 0, i32 1
  %3 = load i32, ptr %2, align 4
  %4 = getelementptr { i8, { i32 } }, ptr %1, i64 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %3, ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %6, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2936", ptr %let_func_0, align 8
  %p_0.i = load i32, ptr %let_capture_0, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i, i32 %5)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal ptr @"runtime/malgo/Builtin.mlg.addInt32"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$int32#_4039_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Builtin.mlg.$int32#_4039_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2939"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %cast_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %cast_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %cast_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  ret ptr %6
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str2938, ptr %4, align 8
  %5 = tail call ptr @malgo_print_string(ptr noundef nonnull @str2938)
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_newline(ptr noundef nonnull %7)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.printInt32", ptr %printInt32_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %10, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2939", ptr %let_func_0.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 1, ptr %12, align 4
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %addInt32_func_0.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_0.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %11, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %14, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935", ptr %let_func_0.i.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { i32 } }, ptr %16, i64 0, i32 1, i32 0
  store i32 1, ptr %17, align 4
  %18 = load ptr, ptr %14, align 8
  %19 = load ptr, ptr %let_func_0.i.i, align 8
  %20 = tail call ptr %19(ptr %18, ptr nonnull %16)
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %addInt32_func_1.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.addInt32", ptr %addInt32_func_1.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %22, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/UseModule.mlg.#let_closure_2935", ptr %let_func_0.i2.i, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1.i", align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { i32 } }, ptr %24, i64 0, i32 1, i32 0
  store i32 1, ptr %25, align 4
  %26 = load ptr, ptr %22, align 8
  %27 = load ptr, ptr %let_func_0.i2.i, align 8
  %28 = tail call ptr %27(ptr %26, ptr nonnull %24)
  %29 = load ptr, ptr %10, align 8
  %30 = load ptr, ptr %let_func_0.i, align 8
  %31 = tail call ptr %30(ptr %29, ptr %28)
  ret i32 0
}
