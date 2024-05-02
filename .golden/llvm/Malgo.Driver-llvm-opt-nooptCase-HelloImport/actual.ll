; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/HelloImport.ll'
source_filename = "test/testcases/malgo/HelloImport.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str79 = unnamed_addr constant [1 x i8] zeroinitializer
@str132 = unnamed_addr constant [6 x i8] c"hello\00"
@str194 = unnamed_addr constant [7 x i8] c" world\00"
@str244 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"runtime/malgo/Prelude.mlg.putStrLn"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"runtime/malgo/Prelude.mlg.$str_716_0") {
  %2 = getelementptr i8, ptr %"runtime/malgo/Prelude.mlg.$str_716_0", i64 8
  %"runtime/malgo/Prelude.mlg.$str_716_0.val" = load ptr, ptr %2, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"runtime/malgo/Prelude.mlg.$str_716_0.val")
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_newline(ptr noundef nonnull %5)
  ret ptr %6
}

define internal ptr @"test/testcases/malgo/HelloImport.mlg.#fun_closure_133"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %1) {
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr @str194, ptr %4, align 8
  %5 = load ptr, ptr %1, align 8
  %6 = getelementptr { ptr, ptr }, ptr %1, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr nonnull %3)
  ret ptr %8
}

define internal ptr @"test/testcases/malgo/HelloImport.mlg.#let_closure_152"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
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
  store ptr @str132, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr noundef nonnull @str132)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %putStrLn_func_0.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.putStrLn", ptr %putStrLn_func_0.i, align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %9, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/HelloImport.mlg.#let_closure_152", ptr %let_func_0.i.i, align 8
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %10, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/HelloImport.mlg.#fun_closure_133", ptr %fun_func_0.i, align 8
  %11 = load ptr, ptr %9, align 8
  %12 = load ptr, ptr %let_func_0.i.i, align 8
  %13 = tail call ptr %12(ptr %11, ptr nonnull %10)
  ret i32 0
}
