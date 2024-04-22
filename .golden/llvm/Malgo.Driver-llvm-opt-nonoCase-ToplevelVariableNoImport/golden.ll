; ModuleID = './test/tmp/malgo_test/nono/ToplevelVariableNoImport.ll'
source_filename = "./test/testcases/malgo/ToplevelVariableNoImport.mlg"

@ToplevelVariableNoImport.one = local_unnamed_addr global ptr undef
@ToplevelVariableNoImport.comp = local_unnamed_addr global ptr undef
@str289 = unnamed_addr constant [3 x i8] c"OK\00"

declare void @GC_init() local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"ToplevelVariableNoImport.Int32#"(ptr nocapture nofree readnone %0, i32 %"ToplevelVariableNoImport.$p_167_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"ToplevelVariableNoImport.$p_167_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"ToplevelVariableNoImport.String#"(ptr nocapture nofree readnone %0, ptr nofree %"ToplevelVariableNoImport.$p_169_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"ToplevelVariableNoImport.$p_169_0", ptr %3, align 8
  ret ptr %2
}

define internal noundef ptr @ToplevelVariableNoImport.Just(ptr nocapture nofree readnone %0, ptr nofree %"ToplevelVariableNoImport.$p_172_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"ToplevelVariableNoImport.$p_172_0", ptr %3, align 8
  ret ptr %2
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_284"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @ToplevelVariableNoImport.malgo_add_int32_t(ptr nocapture nofree readnone %0, i32 %"ToplevelVariableNoImport.$p_174_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"ToplevelVariableNoImport.$p_174_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_284", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"ToplevelVariableNoImport.$p_180_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"ToplevelVariableNoImport.$p_180_0")
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.malgo_int32_t_to_string(ptr nocapture nofree readnone %0, i32 %"ToplevelVariableNoImport.$p_181_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"ToplevelVariableNoImport.$p_181_0")
  ret ptr %2
}

define internal ptr @"ToplevelVariableNoImport.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"ToplevelVariableNoImport.$x_182_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"ToplevelVariableNoImport.$x_182_0")
  ret ptr %3
}

define internal noundef ptr @ToplevelVariableNoImport.toStringInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"ToplevelVariableNoImport.$int32#_184_0") {
"switch_branch_ToplevelVariableNoImport.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$int32#_184_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal ptr @"ToplevelVariableNoImport.printString#"(ptr nocapture nofree readnone %0, ptr %"ToplevelVariableNoImport.$x_189_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"ToplevelVariableNoImport.$x_189_0")
  ret ptr %3
}

define internal ptr @ToplevelVariableNoImport.printString(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"ToplevelVariableNoImport.$string#_191_0") {
"switch_branch_ToplevelVariableNoImport.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"ToplevelVariableNoImport.$string#_191_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @ToplevelVariableNoImport.printInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"ToplevelVariableNoImport.$i_194_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %toStringInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.toStringInt32, ptr %toStringInt32_func_0, align 8
  %3 = getelementptr { i8, { i32 } }, ptr %"ToplevelVariableNoImport.$i_194_0", i64 0, i32 1
  %4 = load i32, ptr %3, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"toStringInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.toStringInt32#", ptr %"toStringInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %malgo_int32_t_to_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i.i, align 8
  %7 = tail call ptr @malgo_int32_t_to_string(i32 %4)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %8, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.String#", ptr %"String#_func_0.i", align 8
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %9, align 1
  %10 = getelementptr { i8, { ptr } }, ptr %9, i64 0, i32 1, i32 0
  store ptr %7, ptr %10, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %11, align 8
  %printString_func_0 = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.printString, ptr %printString_func_0, align 8
  %12 = load ptr, ptr %10, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %"printString#_func_0.i" = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.printString#", ptr %"printString#_func_0.i", align 8
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %malgo_print_string_func_0.i.i = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_print_string, ptr %malgo_print_string_func_0.i.i, align 8
  %15 = tail call ptr @malgo_print_string(ptr %12)
  ret ptr %15
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn
define internal ptr @ToplevelVariableNoImport.identity(ptr nocapture nofree readnone %0, ptr nofree readnone returned %"ToplevelVariableNoImport.$x_200_0") #0 {
  ret ptr %"ToplevelVariableNoImport.$x_200_0"
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"ToplevelVariableNoImport.#let_closure_285"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #1 {
  %a_0 = load ptr, ptr %0, align 8
  ret ptr %a_0
}

define internal ptr @ToplevelVariableNoImport.const(ptr nocapture nofree readnone %0, ptr nofree %"ToplevelVariableNoImport.$a_201_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"ToplevelVariableNoImport.$a_201_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_285", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @ToplevelVariableNoImport.constId(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %"ToplevelVariableNoImport.$eta_210_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %identity_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %const_func_0 = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_0, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %2, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_285", ptr %let_func_0.i, align 8
  %a_0.i = load ptr, ptr %let_capture_0.i, align 8
  ret ptr %a_0.i
}

define internal i32 @"ToplevelVariableNoImport.#let_closure_286"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_add_int32_t_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_284", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %5 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %5
}

define internal ptr @"ToplevelVariableNoImport.addInt32#"(ptr nocapture nofree readnone %0, i32 %"ToplevelVariableNoImport.$x_223_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"ToplevelVariableNoImport.$x_223_0", ptr %let_capture_0, align 4
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_286", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"ToplevelVariableNoImport.#let_closure_287"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
  %"int32#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"int32#_0", i64 4
  %"int32#_0.val" = load i32, ptr %3, align 4
  %4 = getelementptr i8, ptr %1, i64 4
  %.val = load i32, ptr %4, align 4
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"addInt32#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.addInt32#", ptr %"addInt32#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val", ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_286", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_add_int32_t_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.malgo_add_int32_t, ptr %malgo_add_int32_t_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_284", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %9 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.Int32#", ptr %"Int32#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { i32 } }, ptr %11, i64 0, i32 1, i32 0
  store i32 %9, ptr %12, align 4
  ret ptr %11
}

define internal ptr @ToplevelVariableNoImport.addInt32(ptr nocapture nofree readnone %0, ptr nofree %"ToplevelVariableNoImport.$int32#_235_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"ToplevelVariableNoImport.$int32#_235_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_287", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @ToplevelVariableNoImport.addOne(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly %"ToplevelVariableNoImport.$eta_257_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %addInt32_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.addInt32, ptr %addInt32_func_0, align 8
  %3 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %3, ptr %let_capture_0.i, align 8
  store ptr %let_capture_0.i, ptr %4, align 8
  %let_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_287", ptr %let_func_0.i, align 8
  %5 = tail call noundef ptr @"ToplevelVariableNoImport.#let_closure_287"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i, ptr nocapture nofree readonly align 4 %"ToplevelVariableNoImport.$eta_257_0")
  ret ptr %5
}

define internal ptr @"ToplevelVariableNoImport.#fun_closure_288"(ptr nocapture nofree readonly %0, ptr nocapture nofree noundef nonnull readonly dereferenceable(1) %1) {
  %3 = load i8, ptr %1, align 1
  %switch = icmp eq i8 %3, 0
  br i1 %switch, label %switch_branch_ToplevelVariableNoImport.Nothing_0, label %switch_branch_ToplevelVariableNoImport.Just_0

common.ret:                                       ; preds = %switch_branch_ToplevelVariableNoImport.Just_0, %switch_branch_ToplevelVariableNoImport.Nothing_0
  %common.ret.op = phi ptr [ %11, %switch_branch_ToplevelVariableNoImport.Nothing_0 ], [ %12, %switch_branch_ToplevelVariableNoImport.Just_0 ]
  ret ptr %common.ret.op

switch_branch_ToplevelVariableNoImport.Nothing_0: ; preds = %2
  %"String#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i64 0, i32 1
  %"String#_0" = load ptr, ptr %"String#_addr_0", align 8
  %printString_0 = load ptr, ptr %0, align 8
  %4 = load ptr, ptr %"String#_0", align 8
  %5 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = tail call ptr %6(ptr %4, ptr nonnull @str289)
  %8 = load ptr, ptr %printString_0, align 8
  %9 = getelementptr { ptr, ptr }, ptr %printString_0, i64 0, i32 1
  %10 = load ptr, ptr %9, align 8
  %11 = tail call ptr %10(ptr %8, ptr %7)
  br label %common.ret

switch_branch_ToplevelVariableNoImport.Just_0:    ; preds = %2
  %12 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %12, align 1
  br label %common.ret
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.Int32#", ptr %"Int32#_func_0.i", align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 1, ptr %4, align 4
  store ptr %3, ptr @ToplevelVariableNoImport.one, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 0)
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#fun_closure_294", ptr %fun_func_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %6, align 1
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %Just_func_0.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.Just, ptr %Just_func_0.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 1, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %6, ptr %9, align 8
  %10 = load ptr, ptr %5, align 8
  %11 = load ptr, ptr %fun_func_0.i, align 8
  %12 = tail call ptr %11(ptr %10, ptr nonnull %8)
  store ptr %12, ptr @ToplevelVariableNoImport.comp, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %13, align 1
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i1 = tail call ptr @malgo_malloc(i64 noundef 16)
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.printString, ptr %printString_func_0.i, align 8
  store ptr %15, ptr %fun_capture_0.i1, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.String#", ptr %"String#_func_0.i", align 8
  %"String#_0.i" = getelementptr { ptr, ptr }, ptr %fun_capture_0.i1, i64 0, i32 1
  store ptr %16, ptr %"String#_0.i", align 8
  store ptr %fun_capture_0.i1, ptr %14, align 8
  %fun_func_0.i2 = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#fun_closure_288", ptr %fun_func_0.i2, align 8
  %17 = load ptr, ptr @ToplevelVariableNoImport.comp, align 8
  %18 = load i8, ptr %17, align 1
  %switch.i.i = icmp eq i8 %18, 0
  br i1 %switch.i.i, label %switch_branch_ToplevelVariableNoImport.Nothing_0.i.i, label %switch_branch_ToplevelVariableNoImport.Just_0.i.i

switch_branch_ToplevelVariableNoImport.Nothing_0.i.i: ; preds = %1
  %"String#_0.i.i" = load ptr, ptr %"String#_0.i", align 8
  %printString_0.i.i = load ptr, ptr %fun_capture_0.i1, align 8
  %19 = load ptr, ptr %"String#_0.i.i", align 8
  %20 = getelementptr { ptr, ptr }, ptr %"String#_0.i.i", i64 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = tail call ptr %21(ptr %19, ptr nonnull @str289)
  %23 = load ptr, ptr %printString_0.i.i, align 8
  %24 = getelementptr { ptr, ptr }, ptr %printString_0.i.i, i64 0, i32 1
  %25 = load ptr, ptr %24, align 8
  %26 = tail call ptr %25(ptr %23, ptr %22)
  br label %ToplevelVariableNoImport.main.exit

switch_branch_ToplevelVariableNoImport.Just_0.i.i: ; preds = %1
  %27 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %27, align 1
  br label %ToplevelVariableNoImport.main.exit

ToplevelVariableNoImport.main.exit:               ; preds = %switch_branch_ToplevelVariableNoImport.Nothing_0.i.i, %switch_branch_ToplevelVariableNoImport.Just_0.i.i
  %28 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %28, align 8
  %const_func_0.i = getelementptr { ptr, ptr }, ptr %28, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_0.i, align 8
  %29 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %29, align 8
  %constId_func_0.i = getelementptr { ptr, ptr }, ptr %29, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.constId, ptr %constId_func_0.i, align 8
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %30, align 8
  %identity_func_0.i.i = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.identity, ptr %identity_func_0.i.i, align 8
  %31 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %31, align 8
  %const_func_0.i.i = getelementptr { ptr, ptr }, ptr %31, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.const, ptr %const_func_0.i.i, align 8
  %32 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %30, ptr %let_capture_0.i.i.i, align 8
  store ptr %let_capture_0.i.i.i, ptr %32, align 8
  %let_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %32, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_285", ptr %let_func_0.i.i.i, align 8
  %a_0.i.i.i = load ptr, ptr %let_capture_0.i.i.i, align 8
  %33 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %33, align 8
  %addOne_func_0.i = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.addOne, ptr %addOne_func_0.i, align 8
  %34 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %35 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %35, align 8
  %addInt32_func_0.i.i = getelementptr { ptr, ptr }, ptr %35, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.addInt32, ptr %addInt32_func_0.i.i, align 8
  %36 = load ptr, ptr @ToplevelVariableNoImport.one, align 8
  %37 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %36, ptr %let_capture_0.i.i1.i, align 8
  store ptr %let_capture_0.i.i1.i, ptr %37, align 8
  %let_func_0.i.i2.i = getelementptr { ptr, ptr }, ptr %37, i64 0, i32 1
  store ptr @"ToplevelVariableNoImport.#let_closure_287", ptr %let_func_0.i.i2.i, align 8
  %38 = tail call ptr @"ToplevelVariableNoImport.#let_closure_287"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i1.i, ptr nocapture nofree readonly align 4 %34)
  %39 = load ptr, ptr %a_0.i.i.i, align 8
  %40 = getelementptr { ptr, ptr }, ptr %a_0.i.i.i, i64 0, i32 1
  %41 = load ptr, ptr %40, align 8
  %42 = tail call ptr %41(ptr %39, ptr %38)
  %43 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %43, align 8
  %printInt32_func_0.i = getelementptr { ptr, ptr }, ptr %43, i64 0, i32 1
  store ptr @ToplevelVariableNoImport.printInt32, ptr %printInt32_func_0.i, align 8
  %44 = tail call ptr @ToplevelVariableNoImport.printInt32(ptr poison, ptr nocapture nofree readonly %42)
  ret i32 0
}

define internal noundef ptr @"ToplevelVariableNoImport.#fun_closure_294"(ptr nocapture nofree readnone %0, ptr nocapture nofree readnone %1) {
common.ret:
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  ret ptr %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone willreturn }
attributes #1 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
