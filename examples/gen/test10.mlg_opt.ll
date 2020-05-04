; ModuleID = './examples/gen/test10.mlg.ll'
source_filename = "./examples/test10.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @"$lambda9"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda9", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i64 @add_i64(i64 10, i64 %1)
  ret i64 %7
}

declare i64 @add_i64(i64, i64) local_unnamed_addr

define i64 @f0(i64, i64) local_unnamed_addr {
  %3 = tail call i64 @add_i64(i64 %0, i64 %1)
  ret i64 %3
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda9", i64 (i8*, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda9", i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %1, i8** %9, align 8
  %10 = tail call i64 @add_i64(i64 10, i64 3)
  ret i32 0
}
