; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/ToplevelVariable.ll'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.one" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.comp" = local_unnamed_addr global ptr undef
@str169 = unnamed_addr constant [1 x i8] zeroinitializer
@str308 = unnamed_addr constant [3 x i8] c"OK\00"
@str331 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @"runtime/malgo/Prelude.mlg.identity"(ptr nocapture nofree readnone %0, ptr nofree readnone returned %"runtime/malgo/Prelude.mlg.$x_890_0") #0 {
  ret ptr %"runtime/malgo/Prelude.mlg.$x_890_0"
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_262"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_282", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_282"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_332"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %a_0 = load ptr, ptr %0, align 8
  ret ptr %a_0
}

define internal ptr @"runtime/malgo/Prelude.mlg.const"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$a_956_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$a_956_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_332", ptr %let_func_0, align 8
  ret ptr %2
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @"runtime/malgo/Builtin.mlg.undefined", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  store ptr %4, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %6, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_357", ptr %fun_func_0.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = load ptr, ptr %6, align 8
  %11 = load ptr, ptr %fun_func_0.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %8)
  store ptr %12, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  %14 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %.val.i = load i8, ptr %14, align 1
  %switch.i.i = icmp eq i8 %.val.i, 0
  br i1 %switch.i.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i": ; preds = %1
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %15, align 1
  %16 = getelementptr { i8, { ptr } }, ptr %15, i64 0, i32 1, i32 0
  store ptr @str308, ptr %16, align 8
  %17 = tail call ptr @malgo_print_string(ptr noundef nonnull @str308)
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i": ; preds = %1
  %18 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %18, align 1
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"test/testcases/malgo/ToplevelVariable.mlg.main.exit": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i.i", %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i.i"
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %19, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %identity_func_0.i.i = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0.i.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %21, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_332", ptr %let_func_0.i.i.i, align 8
  %a_0.i.i.i = load ptr, ptr %let_capture_0.i.i.i, align 8
  %22 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %22, ptr %let_capture_0.i.i2.i, align 8
  store ptr %let_capture_0.i.i2.i, ptr %23, align 8
  %let_func_0.i.i3.i = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_262", ptr %let_func_0.i.i3.i, align 8
  %"int32#_0.i.i.i" = load ptr, ptr %let_capture_0.i.i2.i, align 8
  %24 = getelementptr i8, ptr %"int32#_0.i.i.i", i64 4
  %"int32#_0.val.i.i.i" = load i32, ptr %24, align 4
  %25 = getelementptr i8, ptr %22, i64 4
  %.val.i.i.i = load i32, ptr %25, align 4
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i.i", ptr %let_capture_0.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i, ptr %26, align 8
  %let_func_0.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %26, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_282", ptr %let_func_0.i.i.i.i.i, align 8
  %x_0.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i, align 4
  %27 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i.i, ptr %27, align 8
  %let_func_0.i.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %27, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_270", ptr %let_func_0.i.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i.i, align 4
  %28 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i.i.i.i, i32 %.val.i.i.i)
  %29 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %29, align 1
  %30 = getelementptr { i8, { i32 } }, ptr %29, i64 0, i32 1, i32 0
  store i32 %28, ptr %30, align 4
  %31 = load ptr, ptr %a_0.i.i.i, align 8
  %32 = getelementptr { ptr, ptr }, ptr %a_0.i.i.i, i64 0, i32 1
  %33 = load ptr, ptr %32, align 8
  %34 = tail call ptr %33(ptr %31, ptr nonnull %29)
  %35 = getelementptr i8, ptr %34, i64 4
  %.val1.i = load i32, ptr %35, align 4
  %36 = tail call ptr @malgo_int32_t_to_string(i32 %.val1.i)
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %36, ptr %38, align 8
  %39 = tail call ptr @malgo_print_string(ptr %36)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariable.mlg.#fun_closure_357"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
common.ret:
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  ret ptr %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
