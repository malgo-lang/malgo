; ModuleID = './examples/gen/test3.mlg.ll'
source_filename = "./examples/test3.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f12(i8*, {}* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, {}*)**
  store {}* (i8*, {}*)* @f12, {}* (i8*, {}*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  ret {}* %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*, {}*)**
  store {}* (i8*, {}*)* @f12, {}* (i8*, {}*)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %3, align 8
  %11 = load i8*, i8** %6, align 8
  %12 = tail call {}* %10(i8* %11, {}* %9)
  %13 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %3, align 8
  %14 = load i8*, i8** %6, align 8
  %15 = tail call {}* %13(i8* %14, {}* %12)
  ret i32 0
}
