; ModuleID = './examples/gen/test3.mlg.ll'
source_filename = "./examples/test3.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f0(i8*, {}* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to {}* (i8*, {}*)**
  store {}* (i8*, {}*)* @f0, {}* (i8*, {}*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  ret {}* %1
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*, {}*)**
  store {}* (i8*, {}*)* @f0, {}* (i8*, {}*)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = bitcast i8* %6 to {}*
  %8 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %3, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = tail call {}* %8(i8* %9, {}* %7)
  %11 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %3, align 8
  %12 = load i8*, i8** %5, align 8
  %13 = tail call {}* %11(i8* %12, {}* %10)
  ret i32 0
}
