; ModuleID = '/workspaces/malgo/.malgo-work/RightAssoc.ll'
source_filename = "./test/testcases/malgo/RightAssoc.mlg"

@str3038 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"RightAssoc.#fun_closure_3030"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3031"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3032"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3033"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3034"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3035"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3036"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3037"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @RightAssoc.f(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"RightAssoc.$n_87_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0, ptr %3, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3030", ptr %fun_func_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %4, i64 0, i32 1, i32 0
  store ptr %2, ptr %5, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %4, i64 0, i32 1, i32 1
  store ptr %3, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2, ptr %8, align 8
  %fun_func_1 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3031", ptr %fun_func_1, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = getelementptr { i8, { ptr, ptr } }, ptr %9, i64 0, i32 1, i32 1
  store ptr %8, ptr %11, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %12, align 1
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_4, ptr %13, align 8
  %fun_func_2 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3032", ptr %fun_func_2, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %14, align 1
  %15 = getelementptr { i8, { ptr, ptr } }, ptr %14, i64 0, i32 1, i32 0
  store ptr %12, ptr %15, align 8
  %16 = getelementptr { i8, { ptr, ptr } }, ptr %14, i64 0, i32 1, i32 1
  store ptr %13, ptr %16, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %17, align 1
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_6, ptr %18, align 8
  %fun_func_3 = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3033", ptr %fun_func_3, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %19, align 1
  %20 = getelementptr { i8, { ptr, ptr } }, ptr %19, i64 0, i32 1, i32 0
  store ptr %17, ptr %20, align 8
  %21 = getelementptr { i8, { ptr, ptr } }, ptr %19, i64 0, i32 1, i32 1
  store ptr %18, ptr %21, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %22, align 1
  %23 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %23, align 1
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %23, i64 0, i32 1, i32 0
  store ptr %19, ptr %24, align 8
  %25 = getelementptr { i8, { ptr, ptr } }, ptr %23, i64 0, i32 1, i32 1
  store ptr %22, ptr %25, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %26, align 1
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %14, ptr %27, align 8
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 1
  store ptr %23, ptr %28, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %29, align 1
  %30 = getelementptr { i8, { ptr, ptr } }, ptr %29, i64 0, i32 1, i32 0
  store ptr %9, ptr %30, align 8
  %31 = getelementptr { i8, { ptr, ptr } }, ptr %29, i64 0, i32 1, i32 1
  store ptr %26, ptr %31, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %32, align 1
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr %4, ptr %33, align 8
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 1
  store ptr %29, ptr %34, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %35, align 1
  %36 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_8, ptr %36, align 8
  %fun_func_4 = getelementptr { ptr, ptr }, ptr %36, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3034", ptr %fun_func_4, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr, ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %35, ptr %38, align 8
  %39 = getelementptr { i8, { ptr, ptr } }, ptr %37, i64 0, i32 1, i32 1
  store ptr %36, ptr %39, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %40, align 1
  %41 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_10, ptr %41, align 8
  %fun_func_5 = getelementptr { ptr, ptr }, ptr %41, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3035", ptr %fun_func_5, align 8
  %42 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %42, align 1
  %43 = getelementptr { i8, { ptr, ptr } }, ptr %42, i64 0, i32 1, i32 0
  store ptr %40, ptr %43, align 8
  %44 = getelementptr { i8, { ptr, ptr } }, ptr %42, i64 0, i32 1, i32 1
  store ptr %41, ptr %44, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %45, align 1
  %46 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_12 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_12, ptr %46, align 8
  %fun_func_6 = getelementptr { ptr, ptr }, ptr %46, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3036", ptr %fun_func_6, align 8
  %47 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %47, align 1
  %48 = getelementptr { i8, { ptr, ptr } }, ptr %47, i64 0, i32 1, i32 0
  store ptr %45, ptr %48, align 8
  %49 = getelementptr { i8, { ptr, ptr } }, ptr %47, i64 0, i32 1, i32 1
  store ptr %46, ptr %49, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %50, align 1
  %51 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14 = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_14, ptr %51, align 8
  %fun_func_7 = getelementptr { ptr, ptr }, ptr %51, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3037", ptr %fun_func_7, align 8
  %52 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %52, align 1
  %53 = getelementptr { i8, { ptr, ptr } }, ptr %52, i64 0, i32 1, i32 0
  store ptr %50, ptr %53, align 8
  %54 = getelementptr { i8, { ptr, ptr } }, ptr %52, i64 0, i32 1, i32 1
  store ptr %51, ptr %54, align 8
  %55 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %55, align 1
  %56 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %56, align 1
  %57 = getelementptr { i8, { ptr, ptr } }, ptr %56, i64 0, i32 1, i32 0
  store ptr %52, ptr %57, align 8
  %58 = getelementptr { i8, { ptr, ptr } }, ptr %56, i64 0, i32 1, i32 1
  store ptr %55, ptr %58, align 8
  %59 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %59, align 1
  %60 = getelementptr { i8, { ptr, ptr } }, ptr %59, i64 0, i32 1, i32 0
  store ptr %47, ptr %60, align 8
  %61 = getelementptr { i8, { ptr, ptr } }, ptr %59, i64 0, i32 1, i32 1
  store ptr %56, ptr %61, align 8
  %62 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %62, align 1
  %63 = getelementptr { i8, { ptr, ptr } }, ptr %62, i64 0, i32 1, i32 0
  store ptr %42, ptr %63, align 8
  %64 = getelementptr { i8, { ptr, ptr } }, ptr %62, i64 0, i32 1, i32 1
  store ptr %59, ptr %64, align 8
  %65 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %65, align 1
  %66 = getelementptr { i8, { ptr, ptr } }, ptr %65, i64 0, i32 1, i32 0
  store ptr %37, ptr %66, align 8
  %67 = getelementptr { i8, { ptr, ptr } }, ptr %65, i64 0, i32 1, i32 1
  store ptr %62, ptr %67, align 8
  ret ptr %65
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %f_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @RightAssoc.f, ptr %f_func_0.i, align 8
  %6 = tail call ptr @RightAssoc.f(ptr poison, ptr poison)
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr @str3038, ptr %8, align 8
  %9 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3038)
  ret i32 0
}
