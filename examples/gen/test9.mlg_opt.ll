; ModuleID = './examples/gen/test9.mlg.ll'
source_filename = "./examples/test9.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @f.11(i8*, {}* nocapture readnone) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, {}*)**
  store i64 (i8*, {}*)* @f.11, i64 (i8*, {}*)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  ret i64 %4
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, {}*)**
  store i64 (i8*, {}*)* @f.11, i64 (i8*, {}*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = load i64 (i8*, {}*)*, i64 (i8*, {}*)** %4, align 8
  %10 = load i8*, i8** %6, align 8
  %11 = tail call i64 %9(i8* %10, {}* %8)
  ret i32 0
}
