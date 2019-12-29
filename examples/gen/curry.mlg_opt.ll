; ModuleID = './examples/gen/curry.mlg.ll'
source_filename = "./examples/curry.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main14() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda15", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %3, align 8
  %9 = load i8*, i8** %6, align 8
  %10 = tail call { i64 (i8*, i64)*, i8* }* %8(i8* %9, i64 1)
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i64 0, i32 0
  %12 = load i64 (i8*, i64)*, i64 (i8*, i64)** %11, align 8
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i64 0, i32 1
  %14 = load i8*, i8** %13, align 8
  %15 = tail call i64 %12(i8* %14, i64 2)
  ret i32 0
}

define { i64 (i8*, i64)*, i8* }* @"$lambda15"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda15", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 8)
  %10 = bitcast i8* %9 to i64*
  store i64 %1, i64* %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = bitcast i8* %12 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda16", i64 (i8*, i64)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %12, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %9, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  ret { i64 (i8*, i64)*, i8* }* %13
}

define i64 @"$lambda16"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda16", i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = add i64 %4, %1
  ret i64 %11
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda15", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %3, align 8
  %9 = load i8*, i8** %6, align 8
  %10 = tail call { i64 (i8*, i64)*, i8* }* %8(i8* %9, i64 1)
  %11 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i64 0, i32 0
  %12 = load i64 (i8*, i64)*, i64 (i8*, i64)** %11, align 8
  %13 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %10, i64 0, i32 1
  %14 = load i8*, i8** %13, align 8
  %15 = tail call i64 %12(i8* %14, i64 2)
  ret i32 0
}
