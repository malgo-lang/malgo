; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/RightAssoc.ll'
source_filename = "test/testcases/malgo/RightAssoc.mlg"

@str3293 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"RightAssoc.#fun_closure_3285"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3286"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3287"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3288"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3289"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3290"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 2, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3291"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 3, ptr %4, align 4
  ret ptr %3
}

define internal noundef ptr @"RightAssoc.#fun_closure_3292"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
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
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 4, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i.i, ptr %6, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3285", ptr %fun_func_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  %9 = getelementptr { i8, { ptr, ptr } }, ptr %7, i64 0, i32 1, i32 1
  store ptr %6, ptr %9, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i.i, ptr %11, align 8
  %fun_func_1.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3286", ptr %fun_func_1.i.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr, ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %10, ptr %13, align 8
  %14 = getelementptr { i8, { ptr, ptr } }, ptr %12, i64 0, i32 1, i32 1
  store ptr %11, ptr %14, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %15, align 1
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_4.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_4.i.i, ptr %16, align 8
  %fun_func_2.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3287", ptr %fun_func_2.i.i, align 8
  %17 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %17, align 1
  %18 = getelementptr { i8, { ptr, ptr } }, ptr %17, i64 0, i32 1, i32 0
  store ptr %15, ptr %18, align 8
  %19 = getelementptr { i8, { ptr, ptr } }, ptr %17, i64 0, i32 1, i32 1
  store ptr %16, ptr %19, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %20, align 1
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_6.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_6.i.i, ptr %21, align 8
  %fun_func_3.i.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3288", ptr %fun_func_3.i.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %22, align 1
  %23 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 0
  store ptr %20, ptr %23, align 8
  %24 = getelementptr { i8, { ptr, ptr } }, ptr %22, i64 0, i32 1, i32 1
  store ptr %21, ptr %24, align 8
  %25 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %25, align 1
  %26 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %26, align 1
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %22, ptr %27, align 8
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 1
  store ptr %25, ptr %28, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %29, align 1
  %30 = getelementptr { i8, { ptr, ptr } }, ptr %29, i64 0, i32 1, i32 0
  store ptr %17, ptr %30, align 8
  %31 = getelementptr { i8, { ptr, ptr } }, ptr %29, i64 0, i32 1, i32 1
  store ptr %26, ptr %31, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %32, align 1
  %33 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 0
  store ptr %12, ptr %33, align 8
  %34 = getelementptr { i8, { ptr, ptr } }, ptr %32, i64 0, i32 1, i32 1
  store ptr %29, ptr %34, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %35, align 1
  %36 = getelementptr { i8, { ptr, ptr } }, ptr %35, i64 0, i32 1, i32 0
  store ptr %7, ptr %36, align 8
  %37 = getelementptr { i8, { ptr, ptr } }, ptr %35, i64 0, i32 1, i32 1
  store ptr %32, ptr %37, align 8
  %38 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %38, align 1
  %39 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_8.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_8.i.i, ptr %39, align 8
  %fun_func_4.i.i = getelementptr { ptr, ptr }, ptr %39, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3289", ptr %fun_func_4.i.i, align 8
  %40 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %40, align 1
  %41 = getelementptr { i8, { ptr, ptr } }, ptr %40, i64 0, i32 1, i32 0
  store ptr %38, ptr %41, align 8
  %42 = getelementptr { i8, { ptr, ptr } }, ptr %40, i64 0, i32 1, i32 1
  store ptr %39, ptr %42, align 8
  %43 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %43, align 1
  %44 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_10.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_10.i.i, ptr %44, align 8
  %fun_func_5.i.i = getelementptr { ptr, ptr }, ptr %44, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3290", ptr %fun_func_5.i.i, align 8
  %45 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %45, align 1
  %46 = getelementptr { i8, { ptr, ptr } }, ptr %45, i64 0, i32 1, i32 0
  store ptr %43, ptr %46, align 8
  %47 = getelementptr { i8, { ptr, ptr } }, ptr %45, i64 0, i32 1, i32 1
  store ptr %44, ptr %47, align 8
  %48 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %48, align 1
  %49 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_12.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_12.i.i, ptr %49, align 8
  %fun_func_6.i.i = getelementptr { ptr, ptr }, ptr %49, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3291", ptr %fun_func_6.i.i, align 8
  %50 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %50, align 1
  %51 = getelementptr { i8, { ptr, ptr } }, ptr %50, i64 0, i32 1, i32 0
  store ptr %48, ptr %51, align 8
  %52 = getelementptr { i8, { ptr, ptr } }, ptr %50, i64 0, i32 1, i32 1
  store ptr %49, ptr %52, align 8
  %53 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 1, ptr %53, align 1
  %54 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_14.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_14.i.i, ptr %54, align 8
  %fun_func_7.i.i = getelementptr { ptr, ptr }, ptr %54, i64 0, i32 1
  store ptr @"RightAssoc.#fun_closure_3292", ptr %fun_func_7.i.i, align 8
  %55 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %55, align 1
  %56 = getelementptr { i8, { ptr, ptr } }, ptr %55, i64 0, i32 1, i32 0
  store ptr %53, ptr %56, align 8
  %57 = getelementptr { i8, { ptr, ptr } }, ptr %55, i64 0, i32 1, i32 1
  store ptr %54, ptr %57, align 8
  %58 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %58, align 1
  %59 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %59, align 1
  %60 = getelementptr { i8, { ptr, ptr } }, ptr %59, i64 0, i32 1, i32 0
  store ptr %55, ptr %60, align 8
  %61 = getelementptr { i8, { ptr, ptr } }, ptr %59, i64 0, i32 1, i32 1
  store ptr %58, ptr %61, align 8
  %62 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %62, align 1
  %63 = getelementptr { i8, { ptr, ptr } }, ptr %62, i64 0, i32 1, i32 0
  store ptr %50, ptr %63, align 8
  %64 = getelementptr { i8, { ptr, ptr } }, ptr %62, i64 0, i32 1, i32 1
  store ptr %59, ptr %64, align 8
  %65 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %65, align 1
  %66 = getelementptr { i8, { ptr, ptr } }, ptr %65, i64 0, i32 1, i32 0
  store ptr %45, ptr %66, align 8
  %67 = getelementptr { i8, { ptr, ptr } }, ptr %65, i64 0, i32 1, i32 1
  store ptr %62, ptr %67, align 8
  %68 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %68, align 1
  %69 = getelementptr { i8, { ptr, ptr } }, ptr %68, i64 0, i32 1, i32 0
  store ptr %40, ptr %69, align 8
  %70 = getelementptr { i8, { ptr, ptr } }, ptr %68, i64 0, i32 1, i32 1
  store ptr %65, ptr %70, align 8
  %71 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %71, align 1
  %72 = getelementptr { i8, { ptr } }, ptr %71, i64 0, i32 1, i32 0
  store ptr @str3293, ptr %72, align 8
  %73 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3293)
  ret i32 0
}
