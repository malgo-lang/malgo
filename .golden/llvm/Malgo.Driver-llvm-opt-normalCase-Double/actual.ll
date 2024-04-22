; ModuleID = './test/tmp/malgo_test/normal/Double.ll'
source_filename = "./test/testcases/malgo/Double.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_double_to_string(double) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { double } }, ptr %3, i64 0, i32 1, i32 0
  store double 3.140000e+00, ptr %4, align 8
  %5 = tail call ptr @malgo_double_to_string(double noundef 3.140000e+00)
  %6 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %6, align 1
  %7 = getelementptr { i8, { ptr } }, ptr %6, i64 0, i32 1, i32 0
  store ptr %5, ptr %7, align 8
  %8 = tail call ptr @malgo_print_string(ptr %5)
  ret i32 0
}
