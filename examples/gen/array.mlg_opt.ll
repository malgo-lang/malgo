; ModuleID = './examples/gen/array.mlg.ll'
source_filename = "./examples/array.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @newline() local_unnamed_addr

define {}* @newline.6() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.5(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 80)
  %2 = getelementptr i8, i8* %1, i64 8
  %3 = bitcast i8* %2 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %1, i64 16
  %6 = bitcast i8* %5 to i64*
  store i64 42, i64* %6, align 8
  %7 = tail call {}* @print_int(i64 %4)
  %8 = tail call {}* @newline()
  %9 = tail call {}* @print_int(i64 42)
  %10 = tail call {}* @newline()
  ret i32 0
}
