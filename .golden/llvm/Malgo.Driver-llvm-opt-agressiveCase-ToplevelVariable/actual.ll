; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/ToplevelVariable.ll'
source_filename = "test/testcases/malgo/ToplevelVariable.mlg"

@"test/testcases/malgo/ToplevelVariable.mlg.one" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariable.mlg.comp" = local_unnamed_addr global ptr undef
@str3732 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @"runtime/malgo/Prelude.mlg.identity"(ptr nocapture nofree readnone %0, ptr nofree readnone returned %"runtime/malgo/Prelude.mlg.$x_890_0") #0 {
  ret ptr %"runtime/malgo/Prelude.mlg.$x_890_0"
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3733"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %cast_0 = load ptr, ptr %0, align 8
  ret ptr %cast_0
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3734"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3735"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %cast_0 = load ptr, ptr %0, align 8
  ret ptr %cast_0
}

define internal i32 @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3736"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3737"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %a_0 = load ptr, ptr %0, align 8
  ret ptr %a_0
}

define internal ptr @"runtime/malgo/Prelude.mlg.const"(ptr nocapture nofree readnone %0, ptr nofree %"runtime/malgo/Prelude.mlg.$a_956_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"runtime/malgo/Prelude.mlg.$a_956_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3737", ptr %let_func_0, align 8
  ret ptr %2
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %2, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr %4, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %7, align 1
  store ptr %7, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.comp", align 8
  %10 = load i8, ptr %9, align 1
  %switch.i = icmp eq i8 %10, 0
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  br i1 %switch.i, label %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i", label %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i"

"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i": ; preds = %1
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr @str3732, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr noundef nonnull @str3732)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_0.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %identity_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_0.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %15, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %16, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3733", ptr %let_func_0.i, align 8
  %cast_0.i.i = load ptr, ptr %let_capture_0.i, align 8
  %17 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %18 = getelementptr { i8, { i32 } }, ptr %17, i64 0, i32 1
  %19 = load i32, ptr %18, align 4
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_2.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %19, ptr %let_capture_2.i, align 4
  store ptr %let_capture_2.i, ptr %20, align 8
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i": ; preds = %1
  store ptr null, ptr %11, align 8
  %const_func_1.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.const", ptr %const_func_1.i, align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %21, align 8
  %identity_func_1.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"runtime/malgo/Prelude.mlg.identity", ptr %identity_func_1.i, align 8
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_4.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %21, ptr %let_capture_4.i, align 8
  store ptr %let_capture_4.i, ptr %22, align 8
  %let_func_2.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3735", ptr %let_func_2.i, align 8
  %cast_0.i1.i = load ptr, ptr %let_capture_4.i, align 8
  %23 = load ptr, ptr @"test/testcases/malgo/ToplevelVariable.mlg.one", align 8
  %24 = getelementptr { i8, { i32 } }, ptr %23, i64 0, i32 1
  %25 = load i32, ptr %24, align 4
  %26 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_6.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %25, ptr %let_capture_6.i, align 4
  store ptr %let_capture_6.i, ptr %26, align 8
  br label %"test/testcases/malgo/ToplevelVariable.mlg.main.exit"

"test/testcases/malgo/ToplevelVariable.mlg.main.exit": ; preds = %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i", %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i"
  %.sink.i = phi ptr [ %26, %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i" ], [ %20, %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i" ]
  %"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3736.sink.i" = phi ptr [ @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3736", %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i" ], [ @"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3734", %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i" ]
  %let_capture_6.sink.i = phi ptr [ %let_capture_6.i, %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i" ], [ %let_capture_2.i, %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i" ]
  %.sink15.i = phi i32 [ %25, %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i" ], [ %19, %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i" ]
  %cast_0.i1.sink11.i = phi ptr [ %cast_0.i1.i, %"switch_branch_runtime/malgo/Prelude.mlg.Just_0.i" ], [ %cast_0.i.i, %"switch_branch_runtime/malgo/Prelude.mlg.Nothing_0.i" ]
  %let_func_3.i = getelementptr { ptr, ptr }, ptr %.sink.i, i64 0, i32 1
  store ptr %"test/testcases/malgo/ToplevelVariable.mlg.#let_closure_3736.sink.i", ptr %let_func_3.i, align 8
  %p_0.i2.i = load i32, ptr %let_capture_6.sink.i, align 4
  %27 = tail call i32 @malgo_add_int32_t(i32 %p_0.i2.i, i32 %.sink15.i)
  %28 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %28, align 1
  %29 = getelementptr { i8, { i32 } }, ptr %28, i64 0, i32 1, i32 0
  store i32 %27, ptr %29, align 4
  %30 = load ptr, ptr %cast_0.i1.sink11.i, align 8
  %31 = getelementptr { ptr, ptr }, ptr %cast_0.i1.sink11.i, i64 0, i32 1
  %32 = load ptr, ptr %31, align 8
  %33 = tail call ptr %32(ptr %30, ptr nonnull %28)
  %34 = getelementptr { i8, { i32 } }, ptr %33, i64 0, i32 1
  %35 = load i32, ptr %34, align 4
  %36 = tail call ptr @malgo_int32_t_to_string(i32 %35)
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %37, align 1
  %38 = getelementptr { i8, { ptr } }, ptr %37, i64 0, i32 1, i32 0
  store ptr %36, ptr %38, align 8
  %39 = tail call ptr @malgo_print_string(ptr %36)
  ret i32 0
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
