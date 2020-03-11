; ModuleID = './examples/gen/test8.mlg.ll'
source_filename = "./examples/test8.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @g3(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }**
  %7 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %6, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g3, i64 (i8*, i64)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i64 0, i32 0
  %13 = load i64 (i8*, i64)*, i64 (i8*, i64)** %12, align 8
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i64 0, i32 1
  %15 = load i8*, i8** %14, align 8
  %16 = tail call i64 %13(i8* %15, i64 %4)
  %17 = add i64 %16, %1
  ret i64 %17
}

define i64 @f1(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f1, i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f1, i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64*
  store i64 42, i64* %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %3, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g3, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %7, i8** %14, align 8
  %15 = load i64, i64* %8, align 8
  %16 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }**
  %17 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %16, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g3, i64 (i8*, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %7, i8** %21, align 8
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i64 0, i32 0
  %23 = load i64 (i8*, i64)*, i64 (i8*, i64)** %22, align 8
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i64 0, i32 1
  %25 = load i8*, i8** %24, align 8
  %26 = tail call i64 %23(i8* %25, i64 %15)
  %27 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %28 = load i8*, i8** %6, align 8
  %29 = tail call i64 %27(i8* %28, i64 5)
  ret i32 0
}
