; ModuleID = './examples/gen/test2.mlg.ll'
source_filename = "./examples/test2.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f7(i8*) {
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*)**
  store {}* (i8*)* @f7, {}* (i8*)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  ret {}* %9
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to {}* (i8*)**
  store {}* (i8*)* @f7, {}* (i8*)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load {}* (i8*)*, {}* (i8*)** %3, align 8
  %9 = load i8*, i8** %6, align 8
  %10 = tail call {}* %8(i8* %9)
  ret i32 0
}
