; ModuleID = './test/tmp/malgo_test/nono/Show.ll'
source_filename = "./test/testcases/malgo/Show.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@Show.showInt32 = local_unnamed_addr global ptr undef
@str246 = unnamed_addr constant [1 x i8] zeroinitializer
@str266 = unnamed_addr constant [10 x i8] c"no branch\00"
@str444 = unnamed_addr constant [2 x i8] c"(\00"
@str445 = unnamed_addr constant [3 x i8] c", \00"
@str446 = unnamed_addr constant [2 x i8] c")\00"
@str447 = unnamed_addr constant [5 x i8] c"show\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_string_append(ptr, ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Builtin.Int32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_1792_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { i32 } }, ptr %2, i64 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %3, align 4
  ret ptr %2
}

define internal noundef ptr @"Builtin.String#"(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$p_1802_0") {
  %2 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %2, align 1
  %3 = getelementptr { i8, { ptr } }, ptr %2, i64 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %3, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_1807_0") {
  %2 = tail call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal ptr @"Show.#let_closure_326"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %p_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_string_append(ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.malgo_string_append(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$p_2140_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$p_2140_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_326", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_int32_t_to_string(ptr nocapture nofree readnone %0, i32 %"Builtin.$p_2155_0") {
  %2 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$p_2155_0")
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr nocapture nofree readnone %0, ptr %"Builtin.$p_2163_0") {
  %2 = tail call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt32#"(ptr nocapture nofree readnone %0, i32 %"Builtin.$x_2179_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_int32_t_to_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0, align 8
  %3 = tail call ptr @malgo_int32_t_to_string(i32 %"Builtin.$x_2179_0")
  ret ptr %3
}

define internal noundef ptr @Builtin.toStringInt32(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 4 %"Builtin.$int32#_2181_0") {
"switch_branch_Builtin.Int32#_0":
  %1 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2181_0", i64 0, i32 1
  %2 = load i32, ptr %1, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"toStringInt32#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.toStringInt32#", ptr %"toStringInt32#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_int32_t_to_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_int32_t_to_string, ptr %malgo_int32_t_to_string_func_0.i, align 8
  %5 = tail call ptr @malgo_int32_t_to_string(i32 %2)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %"String#_func_0" = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0", align 8
  %7 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %7, align 1
  %8 = getelementptr { i8, { ptr } }, ptr %7, i64 0, i32 1, i32 0
  store ptr %5, ptr %8, align 8
  ret ptr %7
}

define internal ptr @"Builtin.printString#"(ptr nocapture nofree readnone %0, ptr %"Builtin.$x_2399_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %2, align 8
  %malgo_print_string_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0, align 8
  %3 = tail call ptr @malgo_print_string(ptr %"Builtin.$x_2399_0")
  ret ptr %3
}

define internal ptr @Builtin.printString(ptr nocapture nofree readnone %0, ptr nocapture nofree readonly align 8 %"Builtin.$string#_2401_0") {
"switch_branch_Builtin.String#_0":
  %1 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2401_0", i64 0, i32 1
  %2 = load ptr, ptr %1, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %"printString#_func_0" = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0", align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %malgo_print_string_func_0.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i, align 8
  %5 = tail call ptr @malgo_print_string(ptr %2)
  ret ptr %5
}

define internal ptr @"Show.#let_closure_433"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_string_append_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_string_append, ptr %malgo_string_append_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Show.#let_closure_326", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %5 = tail call ptr @malgo_string_append(ptr %p_0.i.i, ptr %1)
  ret ptr %5
}

define internal ptr @"Builtin.appendString#"(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$x_3963_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$x_3963_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_433", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Show.#let_closure_434"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 8 %1) {
  %"string#_0" = load ptr, ptr %0, align 8
  %3 = getelementptr i8, ptr %"string#_0", i64 8
  %"string#_0.val" = load ptr, ptr %3, align 8
  %4 = getelementptr i8, ptr %1, i64 8
  %.val = load ptr, ptr %4, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"appendString#_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Builtin.appendString#", ptr %"appendString#_func_0.i", align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"string#_0.val", ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %6, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Show.#let_closure_433", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %7, align 8
  %malgo_string_append_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @Builtin.malgo_string_append, ptr %malgo_string_append_func_0.i.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 8
  store ptr %let_capture_0.i.i.i.i, ptr %8, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Show.#let_closure_326", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load ptr, ptr %let_capture_0.i.i.i.i, align 8
  %9 = tail call ptr @malgo_string_append(ptr %p_0.i.i.i.i, ptr %.val)
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %10, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  %11 = tail call noundef ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %11, align 1
  %12 = getelementptr { i8, { ptr } }, ptr %11, i64 0, i32 1, i32 0
  store ptr %9, ptr %12, align 8
  ret ptr %11
}

define internal ptr @Builtin.appendString(ptr nocapture nofree readnone %0, ptr nofree %"Builtin.$string#_3975_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Builtin.$string#_3975_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_434", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Show.#fun_closure_443"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(40) %0, ptr nocapture nofree readonly align 8 %1) {
"switch_branch_Tuple#_0":
  %"String#_0" = load ptr, ptr %0, align 8
  %"<>_addr_0" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 1
  %"<>_0" = load ptr, ptr %"<>_addr_0", align 8
  %showDictB_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 2
  %showDictB_0 = load ptr, ptr %showDictB_addr_0, align 8
  %showDictA_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 3
  %showDictA_0 = load ptr, ptr %showDictA_addr_0, align 8
  %show_addr_0 = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %0, i64 0, i32 4
  %show_0 = load ptr, ptr %show_addr_0, align 8
  %2 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr { i8, { ptr, ptr } }, ptr %1, i64 0, i32 1, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = load ptr, ptr %"String#_0", align 8
  %7 = getelementptr { ptr, ptr }, ptr %"String#_0", i64 0, i32 1
  %8 = load ptr, ptr %7, align 8
  %9 = tail call ptr %8(ptr %6, ptr nonnull @str444)
  %10 = load ptr, ptr %"<>_0", align 8
  %11 = getelementptr { ptr, ptr }, ptr %"<>_0", i64 0, i32 1
  %12 = load ptr, ptr %11, align 8
  %13 = tail call ptr %12(ptr %10, ptr %9)
  %14 = load ptr, ptr %show_0, align 8
  %15 = getelementptr { ptr, ptr }, ptr %show_0, i64 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = tail call ptr %16(ptr %14, ptr %showDictA_0)
  %18 = load ptr, ptr %17, align 8
  %19 = getelementptr { ptr, ptr }, ptr %17, i64 0, i32 1
  %20 = load ptr, ptr %19, align 8
  %21 = tail call ptr %20(ptr %18, ptr %3)
  %22 = load ptr, ptr %"<>_0", align 8
  %23 = load ptr, ptr %11, align 8
  %24 = tail call ptr %23(ptr %22, ptr %21)
  %25 = load ptr, ptr %"String#_0", align 8
  %26 = load ptr, ptr %7, align 8
  %27 = tail call ptr %26(ptr %25, ptr nonnull @str445)
  %28 = load ptr, ptr %"<>_0", align 8
  %29 = load ptr, ptr %11, align 8
  %30 = tail call ptr %29(ptr %28, ptr %27)
  %31 = load ptr, ptr %show_0, align 8
  %32 = load ptr, ptr %15, align 8
  %33 = tail call ptr %32(ptr %31, ptr %showDictB_0)
  %34 = load ptr, ptr %33, align 8
  %35 = getelementptr { ptr, ptr }, ptr %33, i64 0, i32 1
  %36 = load ptr, ptr %35, align 8
  %37 = tail call ptr %36(ptr %34, ptr %5)
  %38 = load ptr, ptr %"<>_0", align 8
  %39 = load ptr, ptr %11, align 8
  %40 = tail call ptr %39(ptr %38, ptr %37)
  %41 = load ptr, ptr %"String#_0", align 8
  %42 = load ptr, ptr %7, align 8
  %43 = tail call ptr %42(ptr %41, ptr nonnull @str446)
  %44 = load ptr, ptr %40, align 8
  %45 = getelementptr { ptr, ptr }, ptr %40, i64 0, i32 1
  %46 = load ptr, ptr %45, align 8
  %47 = tail call ptr %46(ptr %44, ptr %43)
  %48 = load ptr, ptr %30, align 8
  %49 = getelementptr { ptr, ptr }, ptr %30, i64 0, i32 1
  %50 = load ptr, ptr %49, align 8
  %51 = tail call ptr %50(ptr %48, ptr %47)
  %52 = load ptr, ptr %24, align 8
  %53 = getelementptr { ptr, ptr }, ptr %24, i64 0, i32 1
  %54 = load ptr, ptr %53, align 8
  %55 = tail call ptr %54(ptr %52, ptr %51)
  %56 = load ptr, ptr %13, align 8
  %57 = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  %58 = load ptr, ptr %57, align 8
  %59 = tail call ptr %58(ptr %56, ptr %55)
  ret ptr %59
}

declare ptr @malgo_hash_table_new() local_unnamed_addr

declare void @malgo_hash_table_insert(ptr, ptr, ptr) local_unnamed_addr

declare ptr @malgo_hash_table_get(ptr, ptr) local_unnamed_addr

define internal ptr @Show.show(ptr nocapture nofree readnone %0, ptr %"Show.$record_119_0") {
  %2 = tail call ptr @malgo_hash_table_get(ptr %"Show.$record_119_0", ptr noundef nonnull @str447)
  ret ptr %2
}

define internal ptr @"Show.#let_closure_448"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %showDict_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %show_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Show.show, ptr %show_func_0.i, align 8
  %4 = tail call ptr @malgo_hash_table_get(ptr %showDict_0, ptr noundef nonnull @str447)
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  %7 = load ptr, ptr %6, align 8
  %8 = tail call ptr %7(ptr %5, ptr %1)
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %printString_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Builtin.printString, ptr %printString_func_0.i, align 8
  %10 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %12, align 8
  %"printString#_func_0.i.i" = getelementptr { ptr, ptr }, ptr %12, i64 0, i32 1
  store ptr @"Builtin.printString#", ptr %"printString#_func_0.i.i", align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %13, align 8
  %malgo_print_string_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @Builtin.malgo_print_string, ptr %malgo_print_string_func_0.i.i.i, align 8
  %14 = tail call ptr @malgo_print_string(ptr %11)
  ret ptr %14
}

define internal ptr @Show.print(ptr nocapture nofree readnone %0, ptr nofree %"Show.$showDict_121_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Show.$showDict_121_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_448", ptr %let_func_0, align 8
  ret ptr %2
}

define internal noundef ptr @"Show.#let_closure_449"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly %1) {
  %x_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %appendString_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.appendString, ptr %appendString_func_0.i, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %x_0, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %4, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Show.#let_closure_434", ptr %let_func_0.i.i, align 8
  %5 = tail call noundef ptr @"Show.#let_closure_434"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %let_capture_0.i.i, ptr nocapture nofree readonly align 8 %1)
  ret ptr %5
}

define internal ptr @"Show.<>"(ptr nocapture nofree readnone %0, ptr nofree %"Show.$x_137_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Show.$x_137_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_449", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Show.#let_closure_450"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nofree %1) {
  %showDictA_0 = load ptr, ptr %0, align 8
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 40)
  %4 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %4, align 8
  %"String#_func_0.i" = getelementptr { ptr, ptr }, ptr %4, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i", align 8
  store ptr %4, ptr %fun_capture_0.i, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %5, align 8
  %"<>_func_0.i" = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.<>", ptr %"<>_func_0.i", align 8
  %"<>_0.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 1
  store ptr %5, ptr %"<>_0.i", align 8
  %showDictB_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 2
  store ptr %1, ptr %showDictB_0.i, align 8
  %showDictA_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 3
  store ptr %showDictA_0, ptr %showDictA_0.i, align 8
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %show_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Show.show, ptr %show_func_0.i, align 8
  %show_0.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i, i64 0, i32 4
  store ptr %6, ptr %show_0.i, align 8
  store ptr %fun_capture_0.i, ptr %3, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Show.#fun_closure_443", ptr %fun_func_0.i, align 8
  %7 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str447, ptr noundef nonnull %3)
  ret ptr %7
}

define internal ptr @Show.showTuple2(ptr nocapture nofree readnone %0, ptr nofree %"Show.$showDictA_149_0") {
  %2 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0 = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %"Show.$showDictA_149_0", ptr %let_capture_0, align 8
  store ptr %let_capture_0, ptr %2, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i64 0, i32 1
  store ptr @"Show.#let_closure_450", ptr %let_func_0, align 8
  ret ptr %2
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %3, align 8
  %malgo_unsafe_cast_func_0.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @Builtin.malgo_unsafe_cast, ptr %malgo_unsafe_cast_func_0.i, align 8
  %4 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %4, ptr @Builtin.undefined, align 8
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i = tail call ptr @malgo_malloc(i64 noundef 8)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %6, align 8
  %toStringInt32_func_0.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @Builtin.toStringInt32, ptr %toStringInt32_func_0.i, align 8
  store ptr %6, ptr %fun_capture_0.i, align 8
  store ptr %fun_capture_0.i, ptr %5, align 8
  %fun_func_0.i = getelementptr { ptr, ptr }, ptr %5, i64 0, i32 1
  store ptr @"Show.#fun_closure_455", ptr %fun_func_0.i, align 8
  %7 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %7, ptr noundef nonnull @str447, ptr noundef nonnull %5)
  store ptr %7, ptr @Show.showInt32, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %9, align 8
  %showTuple2_func_0.i = getelementptr { ptr, ptr }, ptr %9, i64 0, i32 1
  store ptr @Show.showTuple2, ptr %showTuple2_func_0.i, align 8
  %10 = load ptr, ptr @Show.showInt32, align 8
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %10, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %11, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Show.#let_closure_450", ptr %let_func_0.i.i, align 8
  %12 = load ptr, ptr @Show.showInt32, align 8
  %showDictA_0.i.i = load ptr, ptr %let_capture_0.i.i, align 8
  %13 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i.i = tail call ptr @malgo_malloc(i64 noundef 40)
  %14 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %14, align 8
  %"String#_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %14, i64 0, i32 1
  store ptr @"Builtin.String#", ptr %"String#_func_0.i.i.i", align 8
  store ptr %14, ptr %fun_capture_0.i.i.i, align 8
  %15 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %15, align 8
  %"<>_func_0.i.i.i" = getelementptr { ptr, ptr }, ptr %15, i64 0, i32 1
  store ptr @"Show.<>", ptr %"<>_func_0.i.i.i", align 8
  %"<>_0.i.i.i" = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 1
  store ptr %15, ptr %"<>_0.i.i.i", align 8
  %showDictB_0.i.i.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 2
  store ptr %12, ptr %showDictB_0.i.i.i, align 8
  %showDictA_0.i.i.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 3
  store ptr %showDictA_0.i.i, ptr %showDictA_0.i.i.i, align 8
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %16, align 8
  %show_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %16, i64 0, i32 1
  store ptr @Show.show, ptr %show_func_0.i.i.i, align 8
  %show_0.i.i.i = getelementptr { ptr, ptr, ptr, ptr, ptr }, ptr %fun_capture_0.i.i.i, i64 0, i32 4
  store ptr %16, ptr %show_0.i.i.i, align 8
  store ptr %fun_capture_0.i.i.i, ptr %13, align 8
  %fun_func_0.i.i.i = getelementptr { ptr, ptr }, ptr %13, i64 0, i32 1
  store ptr @"Show.#fun_closure_443", ptr %fun_func_0.i.i.i, align 8
  %17 = tail call ptr @malgo_hash_table_new()
  tail call void @malgo_hash_table_insert(ptr %17, ptr noundef nonnull @str447, ptr noundef nonnull %13)
  %18 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %18, align 8
  %print_func_0.i = getelementptr { ptr, ptr }, ptr %18, i64 0, i32 1
  store ptr @Show.print, ptr %print_func_0.i, align 8
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i1.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %17, ptr %let_capture_0.i1.i, align 8
  store ptr %let_capture_0.i1.i, ptr %19, align 8
  %let_func_0.i2.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"Show.#let_closure_448", ptr %let_func_0.i2.i, align 8
  %20 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %20, align 8
  %"Int32#_func_0.i" = getelementptr { ptr, ptr }, ptr %20, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_0.i", align 8
  %21 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %21, align 1
  %22 = getelementptr { i8, { i32 } }, ptr %21, i64 0, i32 1, i32 0
  store i32 1, ptr %22, align 4
  %23 = tail call ptr @malgo_malloc(i64 noundef 16)
  store ptr null, ptr %23, align 8
  %"Int32#_func_1.i" = getelementptr { ptr, ptr }, ptr %23, i64 0, i32 1
  store ptr @"Builtin.Int32#", ptr %"Int32#_func_1.i", align 8
  %24 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { i32 } }, ptr %24, i64 0, i32 1, i32 0
  store i32 2, ptr %25, align 4
  %26 = tail call ptr @malgo_malloc(i64 noundef 24)
  store i8 0, ptr %26, align 1
  %27 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 0
  store ptr %21, ptr %27, align 8
  %28 = getelementptr { i8, { ptr, ptr } }, ptr %26, i64 0, i32 1, i32 1
  store ptr %24, ptr %28, align 8
  %29 = load ptr, ptr %19, align 8
  %30 = load ptr, ptr %let_func_0.i2.i, align 8
  %31 = tail call ptr %30(ptr %29, ptr nonnull %26)
  ret i32 0
}

define internal ptr @"Show.#fun_closure_455"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr %1) {
  %toStringInt32_0 = load ptr, ptr %0, align 8
  %3 = load ptr, ptr %toStringInt32_0, align 8
  %4 = getelementptr { ptr, ptr }, ptr %toStringInt32_0, i64 0, i32 1
  %5 = load ptr, ptr %4, align 8
  %6 = tail call ptr %5(ptr %3, ptr %1)
  ret ptr %6
}
