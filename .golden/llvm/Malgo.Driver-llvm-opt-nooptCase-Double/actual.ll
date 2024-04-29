; ModuleID = '/workspaces/malgo/.malgo-work/Double.ll'
source_filename = "./test/testcases/malgo/Double.mlg"

@Builtin.undefined = local_unnamed_addr global ptr undef
@str64 = unnamed_addr constant [1 x i8] zeroinitializer
@str210 = unnamed_addr constant [10 x i8] c"no branch\00"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

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
  %6 = getelementptr { i8, { double } }, ptr %5, i64 0, i32 1, i32 0
  store double 3.140000e+00, ptr %6, align 8
  %7 = tail call ptr @malgo_double_to_string(double noundef 3.140000e+00)
  %8 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %8, align 1
  %9 = getelementptr { i8, { ptr } }, ptr %8, i64 0, i32 1, i32 0
  store ptr %7, ptr %9, align 8
  %10 = tail call ptr @malgo_print_string(ptr %7)
  ret i32 0
}
