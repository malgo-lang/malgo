; ModuleID = '/workspaces/malgo/.malgo-work/test/testcases/malgo/TestExplicitModule.ll'
source_filename = "test/testcases/malgo/TestExplicitModule.mlg"

@"runtime/malgo/Builtin.mlg.undefined" = local_unnamed_addr global ptr undef
@str55 = unnamed_addr constant [1 x i8] zeroinitializer
@str190 = unnamed_addr constant [13 x i8] c"Hello, world\00"
@str219 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_newline(ptr) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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
  store ptr @str190, ptr %6, align 8
  %7 = tail call ptr @malgo_print_string(ptr noundef nonnull @str190)
  %8 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %8, align 1
  %9 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %9, align 1
  %10 = tail call ptr @malgo_newline(ptr noundef nonnull %9)
  ret i32 0
}
