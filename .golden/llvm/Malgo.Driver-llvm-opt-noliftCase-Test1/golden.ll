; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/Test1.ll'
source_filename = "test/testcases/malgo/Test1.mlg"

@str99 = unnamed_addr constant [5 x i8] c"True\00"
@str101 = unnamed_addr constant [6 x i8] c"False\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal ptr @"test/testcases/malgo/Test1.mlg.#let_closure_97"(ptr nocapture nofree readonly align 8 %0, ptr nocapture nofree readonly %1) {
  %d_addr_0 = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %d_0 = load ptr, ptr %d_addr_0, align 8
  %3 = load i8, ptr %d_0, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %"switch_branch_test/testcases/malgo/Test1.mlg.True_0", label %common.ret

common.ret:                                       ; preds = %2, %"switch_branch_test/testcases/malgo/Test1.mlg.True_0"
  %.sink5 = phi ptr [ %t_0, %"switch_branch_test/testcases/malgo/Test1.mlg.True_0" ], [ %1, %2 ]
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = load ptr, ptr %.sink5, align 8
  %6 = getelementptr { ptr, ptr }, ptr %.sink5, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr nonnull %4)
  ret ptr %8

"switch_branch_test/testcases/malgo/Test1.mlg.True_0": ; preds = %2
  %t_0 = load ptr, ptr %0, align 8
  br label %common.ret
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#let_closure_96"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %d_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr %1, ptr %let_capture_0, align 8
  %d_1 = getelementptr { ptr, ptr }, ptr %let_capture_0, i64 0, i32 1
  store ptr %d_0, ptr %d_1, align 8
  store ptr %let_capture_0, ptr %3, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#let_closure_97", ptr %let_func_0, align 8
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_98"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_print_string(ptr noundef nonnull @str99)
  ret ptr %3
}

define internal ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_100"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
  %3 = tail call ptr @malgo_print_string(ptr noundef nonnull @str101)
  ret ptr %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %3, align 1
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#let_closure_96", ptr %let_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_98", ptr %fun_func_0.i, align 8
  %6 = load ptr, ptr %4, align 8
  %7 = load ptr, ptr %let_func_0.i, align 8
  %8 = tail call ptr %7(ptr %6, ptr nonnull %5)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_2.i, ptr %9, align 8
  %fun_func_1.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @"test/testcases/malgo/Test1.mlg.#fun_closure_100", ptr %fun_func_1.i, align 8
  %10 = load ptr, ptr %8, align 8
  %11 = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr nonnull %9)
  ret i32 0
}
