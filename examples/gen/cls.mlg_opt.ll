; ModuleID = './examples/gen/cls.mlg.ll'
source_filename = "./examples/cls.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @add_inner2(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

define { i64 (i8*, i64)*, i8* }* @add0(i64) local_unnamed_addr {
  %2 = tail call i8* @GC_malloc(i64 8)
  %3 = bitcast i8* %2 to i64*
  store i64 %0, i64* %3, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %2, i8** %8, align 8
  ret { i64 (i8*, i64)*, i8* }* %5
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 3, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %1, i8** %10, align 8
  ret i32 0
}
