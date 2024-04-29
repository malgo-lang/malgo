; ModuleID = '/workspaces/malgo/.malgo-work/DeferLaw.ll'
source_filename = "./test/testcases/malgo/DeferLaw.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str84 = unnamed_addr constant [1 x i8] zeroinitializer
@str235 = unnamed_addr constant [10 x i8] c"no branch\00"
@str243 = unnamed_addr constant [6 x i8] c"Hello\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn
define internal ptr @"DeferLaw.#fun_closure_121"(ptr nocapture nofree noundef nonnull readonly align 8 dereferenceable(8) %0, ptr nocapture nofree readnone %1) #0 {
  %x_0 = load ptr, ptr %0, align 8
  ret ptr %x_0
}

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %2)
  store ptr %3, ptr @Builtin.undefined, align 8
  %4 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %4, align 1
  %5 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %5, align 1
  %6 = getelementptr { i8, { ptr } }, ptr %5, i64 0, i32 1, i32 0
  store ptr @str243, ptr %6, align 8
  %7 = tail call ptr @malgo_malloc(i64 noundef 16)
  %fun_capture_0.i.i = tail call ptr @malgo_malloc(i64 noundef 8)
  store ptr %5, ptr %fun_capture_0.i.i, align 8
  store ptr %fun_capture_0.i.i, ptr %7, align 8
  %fun_func_0.i.i = getelementptr { ptr, ptr }, ptr %7, i64 0, i32 1
  store ptr @"DeferLaw.#fun_closure_121", ptr %fun_func_0.i.i, align 8
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = load ptr, ptr %7, align 8
  %10 = load ptr, ptr %fun_func_0.i.i, align 8
  %11 = tail call ptr %10(ptr %9, ptr nonnull %8)
  %12 = getelementptr i8, ptr %11, i64 8
  %.val.i = load ptr, ptr %12, align 8
  %13 = tail call ptr @malgo_print_string(ptr %.val.i)
  %14 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %14, align 1
  %15 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %15, align 1
  %16 = tail call ptr @malgo_newline(ptr noundef nonnull %15)
  ret i32 0
}

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind readonly willreturn }
