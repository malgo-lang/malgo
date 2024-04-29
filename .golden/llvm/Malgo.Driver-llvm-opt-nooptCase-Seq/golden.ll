; ModuleID = '/workspaces/malgo/.malgo-work/Seq.ll'
source_filename = "./test/testcases/malgo/Seq.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@Seq.executeWhenLoaded = local_unnamed_addr global ptr undef
@str99 = unnamed_addr constant [1 x i8] zeroinitializer
@str245 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare i32 @malgo_add_int32_t(i32, i32) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define internal noundef ptr @"Seq.#let_closure_173"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readonly align 4 %1) {
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
  store ptr @"Seq.#let_closure_194", ptr %let_func_0.i.i, align 8
  %x_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i, ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %6, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %6, i64 0, i32 1
  store ptr @"Seq.#let_closure_265", ptr %let_func_0.i.i.i.i, align 8
  %p_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %7 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i, i32 %.val)
  %8 = tail call noundef ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1, i32 0
  store i32 %7, ptr %9, align 4
  ret ptr %8
}

define internal i32 @"Seq.#let_closure_194"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %x_0 = load i32, ptr %0, align 4
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0, ptr %let_capture_0.i.i, align 4
  store ptr %let_capture_0.i.i, ptr %3, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %3, i64 0, i32 1
  store ptr @"Seq.#let_closure_265", ptr %let_func_0.i.i, align 8
  %p_0.i.i = load i32, ptr %let_capture_0.i.i, align 4
  %4 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i, i32 %1)
  ret i32 %4
}

define internal i32 @"Seq.#let_closure_265"(ptr nocapture nofree noundef nonnull readonly align 4 dereferenceable(4) %0, i32 %1) {
  %p_0 = load i32, ptr %0, align 4
  %3 = tail call i32 @malgo_add_int32_t(i32 %p_0, i32 %1)
  ret i32 %3
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %4, align 1
  %5 = getelementptr { i8, { i32 } }, ptr %4, i64 0, i32 1, i32 0
  store i32 1, ptr %5, align 4
  %6 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { i32 } }, ptr %6, i64 0, i32 1, i32 0
  store i32 2, ptr %7, align 4
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %4, ptr %let_capture_0.i.i, align 8
  store ptr %let_capture_0.i.i, ptr %8, align 8
  %let_func_0.i.i = getelementptr { ptr, ptr }, ptr %8, i64 0, i32 1
  store ptr @"Seq.#let_closure_173", ptr %let_func_0.i.i, align 8
  %"int32#_0.i.i" = load ptr, ptr %let_capture_0.i.i, align 8
  %9 = getelementptr i8, ptr %"int32#_0.i.i", i64 4
  %"int32#_0.val.i.i" = load i32, ptr %9, align 4
  %.val.i.i = load i32, ptr %7, align 4
  %10 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i.i", ptr %let_capture_0.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i, ptr %10, align 8
  %let_func_0.i.i.i.i = getelementptr { ptr, ptr }, ptr %10, i64 0, i32 1
  store ptr @"Seq.#let_closure_194", ptr %let_func_0.i.i.i.i, align 8
  %x_0.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i, align 4
  %11 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i.i, ptr %let_capture_0.i.i.i.i.i.i, align 4
  store ptr %let_capture_0.i.i.i.i.i.i, ptr %11, align 8
  %let_func_0.i.i.i.i.i.i = getelementptr { ptr, ptr }, ptr %11, i64 0, i32 1
  store ptr @"Seq.#let_closure_265", ptr %let_func_0.i.i.i.i.i.i, align 8
  %p_0.i.i.i.i.i.i = load i32, ptr %let_capture_0.i.i.i.i.i.i, align 4
  %12 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i.i.i, i32 %.val.i.i)
  %13 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %13, align 1
  %14 = getelementptr { i8, { i32 } }, ptr %13, i64 0, i32 1, i32 0
  store i32 %12, ptr %14, align 4
  %15 = tail call ptr @malgo_int32_t_to_string(i32 %12)
  %16 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %16, align 1
  %17 = getelementptr { i8, { ptr } }, ptr %16, i64 0, i32 1, i32 0
  store ptr %15, ptr %17, align 8
  %18 = tail call ptr @malgo_print_string(ptr %15)
  %19 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i2.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %4, ptr %let_capture_0.i2.i, align 8
  store ptr %let_capture_0.i2.i, ptr %19, align 8
  %let_func_0.i3.i = getelementptr { ptr, ptr }, ptr %19, i64 0, i32 1
  store ptr @"Seq.#let_closure_173", ptr %let_func_0.i3.i, align 8
  %"int32#_0.i4.i" = load ptr, ptr %let_capture_0.i2.i, align 8
  %20 = getelementptr i8, ptr %"int32#_0.i4.i", i64 4
  %"int32#_0.val.i5.i" = load i32, ptr %20, align 4
  %.val.i6.i = load i32, ptr %7, align 4
  %21 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i7.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %"int32#_0.val.i5.i", ptr %let_capture_0.i.i.i7.i, align 4
  store ptr %let_capture_0.i.i.i7.i, ptr %21, align 8
  %let_func_0.i.i.i8.i = getelementptr { ptr, ptr }, ptr %21, i64 0, i32 1
  store ptr @"Seq.#let_closure_194", ptr %let_func_0.i.i.i8.i, align 8
  %x_0.i.i.i9.i = load i32, ptr %let_capture_0.i.i.i7.i, align 4
  %22 = tail call ptr @malgo_malloc(i64 noundef 16)
  %let_capture_0.i.i.i.i.i10.i = tail call ptr @malgo_malloc(i64 noundef 4)
  store i32 %x_0.i.i.i9.i, ptr %let_capture_0.i.i.i.i.i10.i, align 4
  store ptr %let_capture_0.i.i.i.i.i10.i, ptr %22, align 8
  %let_func_0.i.i.i.i.i11.i = getelementptr { ptr, ptr }, ptr %22, i64 0, i32 1
  store ptr @"Seq.#let_closure_265", ptr %let_func_0.i.i.i.i.i11.i, align 8
  %p_0.i.i.i.i.i12.i = load i32, ptr %let_capture_0.i.i.i.i.i10.i, align 4
  %23 = tail call i32 @malgo_add_int32_t(i32 %p_0.i.i.i.i.i12.i, i32 %.val.i6.i)
  %24 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %24, align 1
  %25 = getelementptr { i8, { i32 } }, ptr %24, i64 0, i32 1, i32 0
  store i32 %23, ptr %25, align 4
  store ptr %24, ptr @Seq.executeWhenLoaded, align 8
  %26 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %26, align 1
  %27 = load ptr, ptr @Seq.executeWhenLoaded, align 8
  %28 = getelementptr i8, ptr %27, i64 4
  %.val.i = load i32, ptr %28, align 4
  %29 = tail call ptr @malgo_int32_t_to_string(i32 %.val.i)
  %30 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %30, align 1
  %31 = getelementptr { i8, { ptr } }, ptr %30, i64 0, i32 1, i32 0
  store ptr %29, ptr %31, align 8
  %32 = tail call ptr @malgo_print_string(ptr %29)
  ret i32 0
}
