; ModuleID = '.malgo-work/test/testcases/malgo/RightAssoc.ll'
source_filename = "test/testcases/malgo/RightAssoc.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str606 = unnamed_addr constant [3 x i8] c"OK\00"
@str607 = unnamed_addr constant [1 x i8] zeroinitializer
@str671 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_672"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_673"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_674"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_675"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_676"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_677"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_678"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_679"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
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
  %5 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i32 } }, ptr %5, i64 0, i32 1, i32 0
  store i32 4, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i.i, ptr %8, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_672", ptr %fun_func_0.i.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %8, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %9, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %12, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i.i.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %13, align 1
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i.i, ptr %14, align 8
  %fun_func_1.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_673", ptr %fun_func_1.i.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr %13, ptr %16, align 8
  %17 = getelementptr { i8, { ptr, ptr } }, ptr %15, i64 0, i32 1, i32 1
  store ptr %14, ptr %17, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i1.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i.i1.i.i, align 8
  store ptr %let_capture_0.i.i1.i.i, ptr %18, align 8
  %let_func_0.i.i2.i.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i2.i.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %19, align 1
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_4.i.i, ptr %20, align 8
  %fun_func_2.i.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_674", ptr %fun_func_2.i.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { ptr, ptr } }, ptr %21, i64 0, i32 1, i32 0
  store ptr %19, ptr %22, align 8
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %21, i64 0, i32 1, i32 1
  store ptr %20, ptr %23, align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i3.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %21, ptr %let_capture_0.i.i3.i.i, align 8
  store ptr %let_capture_0.i.i3.i.i, ptr %24, align 8
  %let_func_0.i.i4.i.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i4.i.i, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %25, align 1
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_6.i.i, ptr %26, align 8
  %fun_func_3.i.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_675", ptr %fun_func_3.i.i, align 8
  %27 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %27, i64 0, i32 1, i32 0
  store ptr %25, ptr %28, align 8
  %29 = getelementptr { i8, { ptr, ptr } }, ptr %27, i64 0, i32 1, i32 1
  store ptr %26, ptr %29, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i5.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %27, ptr %let_capture_0.i.i5.i.i, align 8
  store ptr %let_capture_0.i.i5.i.i, ptr %30, align 8
  %let_func_0.i.i6.i.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i6.i.i, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %31, align 1
  %32 = load ptr, ptr %30, align 8
  %33 = load ptr, ptr %let_func_0.i.i6.i.i, align 8
  %34 = tail call ptr %33(ptr %32, ptr nonnull %31)
  %35 = load ptr, ptr %24, align 8
  %36 = load ptr, ptr %let_func_0.i.i4.i.i, align 8
  %37 = tail call ptr %36(ptr %35, ptr %34)
  %38 = load ptr, ptr %18, align 8
  %39 = load ptr, ptr %let_func_0.i.i2.i.i, align 8
  %40 = tail call ptr %39(ptr %38, ptr %37)
  %41 = load ptr, ptr %12, align 8
  %42 = load ptr, ptr %let_func_0.i.i.i.i, align 8
  %43 = tail call ptr %42(ptr %41, ptr %40)
  %44 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %44, align 1
  %45 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_8.i.i, ptr %45, align 8
  %fun_func_4.i.i = getelementptr { ptr, ptr }, ptr %45, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_676", ptr %fun_func_4.i.i, align 8
  %46 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %46, align 1
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %46, i64 0, i32 1, i32 0
  store ptr %44, ptr %47, align 8
  %48 = getelementptr { i8, { ptr, ptr } }, ptr %46, i64 0, i32 1, i32 1
  store ptr %45, ptr %48, align 8
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i7.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %46, ptr %let_capture_0.i.i7.i.i, align 8
  store ptr %let_capture_0.i.i7.i.i, ptr %49, align 8
  %let_func_0.i.i8.i.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i8.i.i, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %50, align 1
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_10.i.i, ptr %51, align 8
  %fun_func_5.i.i = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_677", ptr %fun_func_5.i.i, align 8
  %52 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %52, align 1
  %53 = getelementptr { i8, { ptr, ptr } }, ptr %52, i64 0, i32 1, i32 0
  store ptr %50, ptr %53, align 8
  %54 = getelementptr { i8, { ptr, ptr } }, ptr %52, i64 0, i32 1, i32 1
  store ptr %51, ptr %54, align 8
  %55 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i9.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %52, ptr %let_capture_0.i.i9.i.i, align 8
  store ptr %let_capture_0.i.i9.i.i, ptr %55, align 8
  %let_func_0.i.i10.i.i = getelementptr { ptr, ptr }, ptr %55, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i10.i.i, align 8
  %56 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %56, align 1
  %57 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_12.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_12.i.i, ptr %57, align 8
  %fun_func_6.i.i = getelementptr { ptr, ptr }, ptr %57, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_678", ptr %fun_func_6.i.i, align 8
  %58 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %58, align 1
  %59 = getelementptr { i8, { ptr, ptr } }, ptr %58, i64 0, i32 1, i32 0
  store ptr %56, ptr %59, align 8
  %60 = getelementptr { i8, { ptr, ptr } }, ptr %58, i64 0, i32 1, i32 1
  store ptr %57, ptr %60, align 8
  %61 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i11.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %58, ptr %let_capture_0.i.i11.i.i, align 8
  store ptr %let_capture_0.i.i11.i.i, ptr %61, align 8
  %let_func_0.i.i12.i.i = getelementptr { ptr, ptr }, ptr %61, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i12.i.i, align 8
  %62 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %62, align 1
  %63 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_14.i.i, ptr %63, align 8
  %fun_func_7.i.i = getelementptr { ptr, ptr }, ptr %63, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#fun_closure_679", ptr %fun_func_7.i.i, align 8
  %64 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %64, align 1
  %65 = getelementptr { i8, { ptr, ptr } }, ptr %64, i64 0, i32 1, i32 0
  store ptr %62, ptr %65, align 8
  %66 = getelementptr { i8, { ptr, ptr } }, ptr %64, i64 0, i32 1, i32 1
  store ptr %63, ptr %66, align 8
  %67 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i13.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %64, ptr %let_capture_0.i.i13.i.i, align 8
  store ptr %let_capture_0.i.i13.i.i, ptr %67, align 8
  %let_func_0.i.i14.i.i = getelementptr { ptr, ptr }, ptr %67, i64 0, i32 1
  store ptr @"test/testcases/malgo/RightAssoc.mlg.#let_closure_613", ptr %let_func_0.i.i14.i.i, align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %68, align 1
  %69 = load ptr, ptr %67, align 8
  %70 = load ptr, ptr %let_func_0.i.i14.i.i, align 8
  %71 = tail call ptr %70(ptr %69, ptr nonnull %68)
  %72 = load ptr, ptr %61, align 8
  %73 = load ptr, ptr %let_func_0.i.i12.i.i, align 8
  %74 = tail call ptr %73(ptr %72, ptr %71)
  %75 = load ptr, ptr %55, align 8
  %76 = load ptr, ptr %let_func_0.i.i10.i.i, align 8
  %77 = tail call ptr %76(ptr %75, ptr %74)
  %78 = load ptr, ptr %49, align 8
  %79 = load ptr, ptr %let_func_0.i.i8.i.i, align 8
  %80 = tail call ptr %79(ptr %78, ptr %77)
  %81 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %81, align 1
  %82 = getelementptr { i8, { ptr } }, ptr %81, i64 0, i32 1, i32 0
  store ptr @str606, ptr %82, align 8
  %83 = tail call ptr @malgo_print_string(ptr noundef nonnull @str606)
  ret i32 0
}
