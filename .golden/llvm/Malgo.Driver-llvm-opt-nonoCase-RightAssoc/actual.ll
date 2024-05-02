; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/RightAssoc.ll'
source_filename = "test/testcases/malgo/RightAssoc.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str372 = unnamed_addr constant [1 x i8] zeroinitializer
@str392 = unnamed_addr constant [10 x i8] c"no branch\00"
@str406 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"runtime/malgo/Builtin.mlg.Int32#"(ptr nocapture nofree readnone %0, i32 %"runtime/malgo/Builtin.mlg.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"runtime/malgo/Builtin.mlg.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"runtime/malgo/Builtin.mlg.String#"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Builtin.mlg.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"runtime/malgo/Builtin.mlg.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"runtime/malgo/Builtin.mlg.$p_1807_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.malgo_print_string"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$p_2163_0")
  ret ptr %2
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString#"(ptr nocapture nofree readnone %0, ptr %"runtime/malgo/Builtin.mlg.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Builtin.mlg.$x_2399_0")
  ret ptr %3
}

define internal ptr @"runtime/malgo/Builtin.mlg.printString"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Builtin.mlg.$string#_2401_0") {
"switch_branch_runtime/malgo/Builtin.mlg.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"runtime/malgo/Builtin.mlg.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal ptr @"runtime/malgo/Prelude.mlg.Cons"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$p_691_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$p_691_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.<|>"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %Cons_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/RightAssoc.mlg.$eta_86_0", ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %3, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_398"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 1)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_399"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 2)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_400"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 3)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_401"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 4)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_402"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 1)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_403"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 2)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_404"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 3)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_405"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) {
  %"Int32#_0" = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %"Int32#_0", align 8
  %4 = getelementptr { ptr, ptr }, ptr %"Int32#_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, i32 4)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/RightAssoc.mlg.f"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"test/testcases/malgo/RightAssoc.mlg.$n_87_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"Int32#_func_0" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0", align 8
  store ptr %4, ptr %fun_capture_0, align 8
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_398", ptr %fun_func_0, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %2, ptr %6, align 8
  %7 = getelementptr { i8, { ptr, ptr } }, ptr %5, i64 0, i32 1, i32 1
  store ptr %3, ptr %7, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"<|>_func_0" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_0", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %Cons_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %10, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %11, align 1
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 8)
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"Int32#_func_1" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_1", align 8
  store ptr %13, ptr %fun_capture_2, align 8
  store ptr %fun_capture_2, ptr %12, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_399", ptr %fun_func_1, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %11, ptr %15, align 8
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %14, i64 0, i32 1, i32 1
  store ptr %12, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %"<|>_func_1" = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_1", align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %Cons_func_0.i1 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i1, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %14, ptr %let_capture_0.i.i2, align 8
  store ptr %let_capture_0.i.i2, ptr %19, align 8
  %let_func_0.i.i3 = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i3, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %20, align 1
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4 = tail call ptr @malgo_malloc(i64 noundef 8)
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %22, align 8
  %"Int32#_func_2" = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_2", align 8
  store ptr %22, ptr %fun_capture_4, align 8
  store ptr %fun_capture_4, ptr %21, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_400", ptr %fun_func_2, align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %23, align 1
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %23, i64 0, i32 1, i32 0
  store ptr %20, ptr %24, align 8
  %25 = getelementptr { i8, { ptr, ptr } }, ptr %23, i64 0, i32 1, i32 1
  store ptr %21, ptr %25, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %26, align 8
  %"<|>_func_2" = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_2", align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %27, align 8
  %Cons_func_0.i4 = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i4, align 8
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %23, ptr %let_capture_0.i.i5, align 8
  store ptr %let_capture_0.i.i5, ptr %28, align 8
  %let_func_0.i.i6 = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i6, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %29, align 1
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6 = tail call ptr @malgo_malloc(i64 noundef 8)
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %"Int32#_func_3" = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_3", align 8
  store ptr %31, ptr %fun_capture_6, align 8
  store ptr %fun_capture_6, ptr %30, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_401", ptr %fun_func_3, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %32, align 1
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr %29, ptr %33, align 8
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 1
  store ptr %30, ptr %34, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %"<|>_func_3" = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_3", align 8
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %36, align 8
  %Cons_func_0.i7 = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i7, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i8 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %32, ptr %let_capture_0.i.i8, align 8
  store ptr %let_capture_0.i.i8, ptr %37, align 8
  %let_func_0.i.i9 = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i9, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %38, align 1
  %39 = load ptr, ptr %37, align 8
  %40 = load ptr, ptr %let_func_0.i.i9, align 8
  %41 = tail call ptr %40(ptr %39, ptr nonnull %38)
  %42 = load ptr, ptr %28, align 8
  %43 = load ptr, ptr %let_func_0.i.i6, align 8
  %44 = tail call ptr %43(ptr %42, ptr %41)
  %45 = load ptr, ptr %19, align 8
  %46 = load ptr, ptr %let_func_0.i.i3, align 8
  %47 = tail call ptr %46(ptr %45, ptr %44)
  %48 = load ptr, ptr %10, align 8
  %49 = load ptr, ptr %let_func_0.i.i, align 8
  %50 = tail call ptr %49(ptr %48, ptr %47)
  %51 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %51, align 1
  %52 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8 = tail call ptr @malgo_malloc(i64 noundef 8)
  %53 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %53, align 8
  %"Int32#_func_4" = getelementptr { ptr, ptr }, ptr %53, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_4", align 8
  store ptr %53, ptr %fun_capture_8, align 8
  store ptr %fun_capture_8, ptr %52, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %52, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_402", ptr %fun_func_4, align 8
  %54 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %54, align 1
  %55 = getelementptr { i8, { ptr, ptr } }, ptr %54, i64 0, i32 1, i32 0
  store ptr %51, ptr %55, align 8
  %56 = getelementptr { i8, { ptr, ptr } }, ptr %54, i64 0, i32 1, i32 1
  store ptr %52, ptr %56, align 8
  %57 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %57, align 8
  %"<|>_func_4" = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_4", align 8
  %58 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %58, align 8
  %Cons_func_0.i10 = getelementptr { ptr, ptr }, ptr %58, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i10, align 8
  %59 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i11 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %54, ptr %let_capture_0.i.i11, align 8
  store ptr %let_capture_0.i.i11, ptr %59, align 8
  %let_func_0.i.i12 = getelementptr { ptr, ptr }, ptr %59, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i12, align 8
  %60 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %60, align 1
  %61 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10 = tail call ptr @malgo_malloc(i64 noundef 8)
  %62 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %62, align 8
  %"Int32#_func_5" = getelementptr { ptr, ptr }, ptr %62, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_5", align 8
  store ptr %62, ptr %fun_capture_10, align 8
  store ptr %fun_capture_10, ptr %61, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %61, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_403", ptr %fun_func_5, align 8
  %63 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %63, align 1
  %64 = getelementptr { i8, { ptr, ptr } }, ptr %63, i64 0, i32 1, i32 0
  store ptr %60, ptr %64, align 8
  %65 = getelementptr { i8, { ptr, ptr } }, ptr %63, i64 0, i32 1, i32 1
  store ptr %61, ptr %65, align 8
  %66 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %66, align 8
  %"<|>_func_5" = getelementptr { ptr, ptr }, ptr %66, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_5", align 8
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %67, align 8
  %Cons_func_0.i13 = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i13, align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i14 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %63, ptr %let_capture_0.i.i14, align 8
  store ptr %let_capture_0.i.i14, ptr %68, align 8
  %let_func_0.i.i15 = getelementptr { ptr, ptr }, ptr %68, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i15, align 8
  %69 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %69, align 1
  %70 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_12 = tail call ptr @malgo_malloc(i64 noundef 8)
  %71 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %71, align 8
  %"Int32#_func_6" = getelementptr { ptr, ptr }, ptr %71, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_6", align 8
  store ptr %71, ptr %fun_capture_12, align 8
  store ptr %fun_capture_12, ptr %70, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %70, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_404", ptr %fun_func_6, align 8
  %72 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %72, align 1
  %73 = getelementptr { i8, { ptr, ptr } }, ptr %72, i64 0, i32 1, i32 0
  store ptr %69, ptr %73, align 8
  %74 = getelementptr { i8, { ptr, ptr } }, ptr %72, i64 0, i32 1, i32 1
  store ptr %70, ptr %74, align 8
  %75 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %75, align 8
  %"<|>_func_6" = getelementptr { ptr, ptr }, ptr %75, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_6", align 8
  %76 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %76, align 8
  %Cons_func_0.i16 = getelementptr { ptr, ptr }, ptr %76, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i16, align 8
  %77 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i17 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %72, ptr %let_capture_0.i.i17, align 8
  store ptr %let_capture_0.i.i17, ptr %77, align 8
  %let_func_0.i.i18 = getelementptr { ptr, ptr }, ptr %77, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i18, align 8
  %78 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %78, align 1
  %79 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14 = tail call ptr @malgo_malloc(i64 noundef 8)
  %80 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %80, align 8
  %"Int32#_func_7" = getelementptr { ptr, ptr }, ptr %80, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_7", align 8
  store ptr %80, ptr %fun_capture_14, align 8
  store ptr %fun_capture_14, ptr %79, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %79, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_405", ptr %fun_func_7, align 8
  %81 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %81, align 1
  %82 = getelementptr { i8, { ptr, ptr } }, ptr %81, i64 0, i32 1, i32 0
  store ptr %78, ptr %82, align 8
  %83 = getelementptr { i8, { ptr, ptr } }, ptr %81, i64 0, i32 1, i32 1
  store ptr %79, ptr %83, align 8
  %84 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %84, align 8
  %"<|>_func_7" = getelementptr { ptr, ptr }, ptr %84, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.<|>", ptr %"<|>_func_7", align 8
  %85 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %85, align 8
  %Cons_func_0.i19 = getelementptr { ptr, ptr }, ptr %85, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.Cons", ptr %Cons_func_0.i19, align 8
  %86 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i20 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %81, ptr %let_capture_0.i.i20, align 8
  store ptr %let_capture_0.i.i20, ptr %86, align 8
  %let_func_0.i.i21 = getelementptr { ptr, ptr }, ptr %86, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_377", ptr %let_func_0.i.i21, align 8
  %87 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %87, align 1
  %88 = load ptr, ptr %86, align 8
  %89 = load ptr, ptr %let_func_0.i.i21, align 8
  %90 = tail call ptr %89(ptr %88, ptr nonnull %87)
  %91 = load ptr, ptr %77, align 8
  %92 = load ptr, ptr %let_func_0.i.i18, align 8
  %93 = tail call ptr %92(ptr %91, ptr %90)
  %94 = load ptr, ptr %68, align 8
  %95 = load ptr, ptr %let_func_0.i.i15, align 8
  %96 = tail call ptr %95(ptr %94, ptr %93)
  %97 = load ptr, ptr %59, align 8
  %98 = load ptr, ptr %let_func_0.i.i12, align 8
  %99 = tail call ptr %98(ptr %97, ptr %96)
  ret ptr %99
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_unsafe_cast", ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.Int32#", ptr %"Int32#_func_0.i", align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { i32 } }, ptr %7, i64 0, i32 1, i32 0
  store i32 4, ptr %8, align 4
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %f_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.f", ptr %f_func_0.i, align 8
  %10 = tail call ptr @"test/testcases/malgo/RightAssoc.mlg.f"(ptr poison, ptr poison)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.String#", ptr %"String#_func_0.i", align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr @str406, ptr %13, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString", ptr %printString_func_0.i, align 8
  %15 = load ptr, ptr %13, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %"printString#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.printString#", ptr %"printString#_func_0.i.i", align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %malgo_print_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"runtime/malgo/Builtin.mlg.malgo_print_string", ptr %malgo_print_string_func_0.i.i.i, align 8
  %18 = tail call ptr @malgo_print_string(ptr %15)
  ret i32 0
}
