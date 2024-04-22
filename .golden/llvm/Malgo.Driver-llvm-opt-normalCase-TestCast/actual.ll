; ModuleID = './test/tmp/malgo_test/normal/TestCast.ll'
source_filename = "./test/testcases/malgo/TestCast.mlg"

declare void @GC_init() local_unnamed_addr

declare ptr @malgo_unsafe_cast(ptr) local_unnamed_addr

declare ptr @malgo_int32_t_to_string(i32) local_unnamed_addr

declare ptr @malgo_print_string(ptr) local_unnamed_addr

declare ptr @malgo_new_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_read_vector(i64, ptr) local_unnamed_addr

declare ptr @malgo_malloc(i64) local_unnamed_addr

define noundef i32 @main(ptr nocapture nofree readnone %0) local_unnamed_addr {
  tail call void @GC_init()
  %2 = tail call ptr @malgo_malloc(i64 noundef 1)
  store i8 0, ptr %2, align 1
  %3 = tail call ptr @malgo_malloc(i64 noundef 8)
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %3, i64 0, i32 1, i32 0
  store i32 5, ptr %4, align 4
  %5 = tail call ptr @malgo_unsafe_cast(ptr noundef nonnull %3)
  %6 = tail call ptr @malgo_new_vector(i64 noundef 10, ptr %5)
  %7 = tail call ptr @malgo_read_vector(i64 noundef 2, ptr %6)
  %8 = tail call ptr @malgo_unsafe_cast(ptr %7)
  %9 = getelementptr { i8, { i32 } }, ptr %8, i64 0, i32 1
  %10 = load i32, ptr %9, align 4
  %11 = tail call ptr @malgo_int32_t_to_string(i32 %10)
  %12 = tail call ptr @malgo_malloc(i64 noundef 16)
  store i8 0, ptr %12, align 1
  %13 = getelementptr { i8, { ptr } }, ptr %12, i64 0, i32 1, i32 0
  store ptr %11, ptr %13, align 8
  %14 = tail call ptr @malgo_print_string(ptr %11)
  ret i32 0
}
