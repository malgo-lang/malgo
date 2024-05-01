; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/ZeroArgs.ll'
source_filename = "test/testcases/malgo/ZeroArgs.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ZeroArgs.mlg.one" = local_unnamed_addr global ptr undef
@str89 = unnamed_addr constant [1 x i8] zeroinitializer
@str169 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_103"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %x_0 = load ptr, ptr %0, align 8
  ret ptr %x_0
}

define internal ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_157"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = load ptr, ptr @"test/testcases/malgo/ZeroArgs.mlg.one", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %fun_capture_0.i.i, align 8
  store ptr %fun_capture_0.i.i, ptr %4, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_103", ptr %fun_func_0.i.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %fun_func_0.i.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  %9 = getelementptr i8, ptr %8, i64 4
  %.val.i = load i32, ptr %9, align 4
  %10 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %10, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr %10)
  ret ptr %13
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %4, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_294", ptr %fun_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %fun_func_0.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  store ptr %8, ptr @"test/testcases/malgo/ZeroArgs.mlg.one", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %10, align 1
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i.i, ptr %11, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_157", ptr %fun_func_0.i.i, align 8
  %12 = load ptr, ptr @"test/testcases/malgo/ZeroArgs.mlg.one", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %12, ptr %fun_capture_0.i.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i.i, ptr %13, align 8
  %fun_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_103", ptr %fun_func_0.i.i.i.i, align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = load ptr, ptr %13, align 8
  %16 = load ptr, ptr %fun_func_0.i.i.i.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr nonnull %14)
  %18 = getelementptr i8, ptr %17, i64 4
  %.val.i.i.i = load i32, ptr %18, align 4
  %19 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i.i.i)
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %20, align 1
  %21 = getelementptr { i8, { ptr } }, ptr %20, i64 0, i32 1, i32 0
  store ptr %19, ptr %21, align 8
  %22 = tail call ptr @malgo_print_string(ptr %19)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/ZeroArgs.mlg.#fun_closure_294"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  ret ptr %3
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
