; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/ToplevelVariableNoImport.ll'
source_filename = "test/testcases/malgo/ToplevelVariableNoImport.mlg"

@"test/testcases/malgo/ToplevelVariableNoImport.mlg.one" = local_unnamed_addr global ptr undef
@"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp" = local_unnamed_addr global ptr undef
@str284 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.identity"(ptr nocapture nofree readnone %0, ptr nofree readnone returned %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_200_0") #0 {
  ret ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$x_200_0"
}

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_285"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_288", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_286"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %a_0 = load ptr, ptr %0, align 8
  ret ptr %a_0
}

define internal ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.const"(ptr nocapture nofree readnone %0, ptr nofree %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_201_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"test/testcases/malgo/ToplevelVariableNoImport.mlg.$a_201_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_286", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal i32 @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_288"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 1, ptr %3, align 4
  store ptr %2, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.one", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %4, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_293", ptr %fun_func_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %5, align 1
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = load ptr, ptr %fun_func_0.i, align 8
  %10 = tail call ptr %9(ptr %8, ptr nonnull %6)
  store ptr %10, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp", align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %11, align 1
  %12 = load ptr, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.comp", align 8
  %.val.i = load i8, ptr %12, align 1
  %switch.i.i = icmp eq i8 %.val.i, 0
  br i1 %switch.i.i, label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0.i.i", label %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0.i.i"

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0.i.i": ; preds = %1
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { ptr } }, ptr %13, i64 0, i32 1, i32 0
  store ptr @str284, ptr %14, align 8
  %15 = tail call ptr @malgo_print_string(ptr noundef nonnull @str284)
  br label %"test/testcases/malgo/ToplevelVariableNoImport.mlg.main.exit"

"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0.i.i": ; preds = %1
  %16 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %16, align 1
  br label %"test/testcases/malgo/ToplevelVariableNoImport.mlg.main.exit"

"test/testcases/malgo/ToplevelVariableNoImport.mlg.main.exit": ; preds = %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Nothing_0.i.i", %"switch_branch_test/testcases/malgo/ToplevelVariableNoImport.mlg.Just_0.i.i"
  %17 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %17, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.const", ptr %const_func_0.i, align 8
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %identity_func_0.i.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.identity", ptr %identity_func_0.i.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %18, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %19, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_286", ptr %let_func_0.i.i.i, align 8
  %a_0.i.i.i = load ptr, ptr %let_capture_0.i.i.i, align 8
  %20 = load ptr, ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.one", align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %20, ptr %let_capture_0.i.i2.i, align 8
  store ptr %let_capture_0.i.i2.i, ptr %21, align 8
  %let_func_0.i.i3.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_285", ptr %let_func_0.i.i3.i, align 8
  %"int32#_0.i.i.i" = load ptr, ptr %let_capture_0.i.i2.i, align 8
  %22 = getelementptr i8, ptr %"int32#_0.i.i.i", i64 4
  %"int32#_0.val.i.i.i" = load i32, ptr %22, align 4
  %23 = getelementptr i8, ptr %20, i64 4
  %.val.i.i.i = load i32, ptr %23, align 4
  %24 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i.i", ptr %let_capture_0.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i, ptr %24, align 8
  %let_func_0.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_288", ptr %let_func_0.i.i.i.i.i, align 8
  %x_0.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i, align 4
  %25 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i.i, ptr %25, align 8
  %let_func_0.i.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %25, i64 0, i32 1
  store ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#let_closure_287", ptr %let_func_0.i.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i.i, align 4
  %26 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i.i.i.i, i32 %.val.i.i.i)
  %27 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %27, align 1
  %28 = getelementptr { i8, { i32 } }, ptr %27, i64 0, i32 1, i32 0
  store i32 %26, ptr %28, align 4
  %29 = load ptr, ptr %a_0.i.i.i, align 8
  %30 = getelementptr { ptr, ptr }, ptr %a_0.i.i.i, i64 0, i32 1
  %31 = load ptr, ptr %30, align 8
  %32 = tail call ptr %31(ptr %29, ptr nonnull %27)
  %33 = getelementptr i8, ptr %32, i64 4
  %.val1.i = load i32, ptr %33, align 4
  %34 = tail call ptr @malgo_int32_t_to_string(i32 %.val1.i)
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %35, align 1
  %36 = getelementptr { i8, { ptr } }, ptr %35, i64 0, i32 1, i32 0
  store ptr %34, ptr %36, align 8
  %37 = tail call ptr @malgo_print_string(ptr %34)
  ret i32 0
}

define internal noundef ptr @"test/testcases/malgo/ToplevelVariableNoImport.mlg.#fun_closure_293"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
common.ret:
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  ret ptr %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
