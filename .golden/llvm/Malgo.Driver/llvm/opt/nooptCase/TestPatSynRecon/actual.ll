; ModuleID = '.malgo-work/test/testcases/malgo/TestPatSynRecon.ll'
source_filename = "test/testcases/malgo/TestPatSynRecon.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str298 = unnamed_addr constant [1 x i8] zeroinitializer
@str362 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i64 @malgo_add_int64_t(i64, i64) local_unnamed_addr

declare ptr @malgo_int64_t_to_string(i64) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_214"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_215", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %4, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_217", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %5 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i.i, i64 %1)
  ret i64 %5
}

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_215"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %x_0 = load i64, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_217", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %4 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i, i64 %1)
  ret i64 %4
}

define internal i64 @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_217"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, i64 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = tail call i64 @malgo_add_int64_t(i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_297"(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %1) {
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %3, align 4
  %4 = tail call ptr @malgo_int64_t_to_string(i64 %.val)
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr %4)
  ret ptr %7
}

define internal ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_301"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(16) %1) {
  %x_0 = load ptr, ptr %0, align 8
  %.val = load ptr, ptr %1, align 8
  %3 = getelementptr i8, ptr %1, i64 8
  %.val1 = load ptr, ptr %3, align 8
  %4 = tail call ptr %.val1(ptr %.val, ptr %x_0)
  ret ptr %4
}

define internal noundef ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_304"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call noundef ptr @malgo_malloc(i64 noundef 24)
  store i8 1, ptr %3, align 1
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 0
  store ptr %p_0, ptr %4, align 8
  %5 = getelementptr { i8, { ptr, ptr } }, ptr %3, i64 0, i32 1, i32 1
  store ptr %1, ptr %5, align 8
  ret ptr %3
}

define internal noundef ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_365"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %p_0 = load i64, ptr %0, align 4
  %3 = getelementptr i8, ptr %1, i64 4
  %.val = load i64, ptr %3, align 4
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %p_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_214", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i64, ptr %let_capture_0.i.i, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %5, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_215", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_217", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i64, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %7 = tail call i64 @malgo_add_int64_t(i64 %p_0.i.i.i.i.i.i, i64 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 %7, ptr %9, align 4
  ret ptr %8
}

define internal fastcc ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0") unnamed_addr {
  %1 = load i8, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", align 1
  %switch = icmp eq i8 %1, 0
  br i1 %switch, label %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0", label %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0"

common.ret:                                       ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0", %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0"
  %common.ret.op = phi ptr [ %2, %"switch_branch_runtime/malgo/Prelude.mlg.Nil_0" ], [ %15, %"switch_branch_runtime/malgo/Prelude.mlg.Cons_0" ]
  ret ptr %common.ret.op

"switch_branch_runtime/malgo/Prelude.mlg.Nil_0":  ; preds = %0
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i64 } }, ptr %2, i64 0, i32 1, i32 0
  store i64 0, ptr %3, align 4
  br label %common.ret

"switch_branch_runtime/malgo/Prelude.mlg.Cons_0": ; preds = %0
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { i8, { ptr, ptr } }, ptr %"test/testcases/malgo/TestPatSynRecon.mlg.$cons_95_0", i64 0, i32 1, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1
  %9 = load i64, ptr %8, align 4
  %10 = tail call fastcc ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr nocapture nofree readonly %7)
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %11, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_301", ptr %let_func_0.i, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i64 %9, ptr %fun_capture_0, align 4
  store ptr %fun_capture_0, ptr %12, align 8
  %fun_func_0 = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_365", ptr %fun_func_0, align 8
  %13 = load ptr, ptr %11, align 8
  %14 = load ptr, ptr %let_func_0.i, align 8
  %15 = tail call ptr %14(ptr %13, ptr nonnull %12)
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { i64 } }, ptr %5, i64 0, i32 1, i32 0
  store i64 1, ptr %6, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %7, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_304", ptr %let_func_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 12)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i64 } }, ptr %8, i64 0, i32 1, i32 0
  store i64 2, ptr %9, align 4
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %8, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %10, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_304", ptr %let_func_0.i2.i, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = load ptr, ptr %10, align 8
  %13 = load ptr, ptr %let_func_0.i2.i, align 8
  %14 = tail call ptr %13(ptr %12, ptr nonnull %11)
  %15 = load ptr, ptr %7, align 8
  %16 = load ptr, ptr %let_func_0.i.i, align 8
  %17 = tail call ptr %16(ptr %15, ptr %14)
  %18 = tail call fastcc ptr @"test/testcases/malgo/TestPatSynRecon.mlg.sum"(ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %17)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i3.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i3.i, align 8
  store ptr %let_capture_0.i3.i, ptr %19, align 8
  %let_func_0.i4.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#let_closure_301", ptr %let_func_0.i4.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %20, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"test/testcases/malgo/TestPatSynRecon.mlg.#fun_closure_297", ptr %fun_func_0.i, align 8
  %21 = load ptr, ptr %19, align 8
  %22 = load ptr, ptr %let_func_0.i4.i, align 8
  %23 = tail call ptr %22(ptr %21, ptr nonnull %20)
  ret i32 0
}
