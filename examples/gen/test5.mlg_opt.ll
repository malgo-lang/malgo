; ModuleID = './examples/gen/test5.mlg.ll'
source_filename = "./examples/test5.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64 (i8*, i64)*, i8* }* @k1(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 8)
  %10 = bitcast i8* %9 to i64*
  store i64 %4, i64* %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }*
  %13 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %13, align 8
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %9, i8** %15, align 8
  ret { i64 (i8*, i64)*, i8* }* %12
}

define i64 @f4(i8*, i64, i64) {
  %4 = bitcast i8* %0 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }**
  %5 = load { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }** %4, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f4, i64 (i8*, i64, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i64 0, i32 0
  %11 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %10, align 8
  %12 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i64 0, i32 1
  %13 = load i8*, i8** %12, align 8
  %14 = tail call { i64 (i8*, i64)*, i8* }* %11(i8* %13, i64 %1)
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i64 0, i32 0
  %16 = load i64 (i8*, i64)*, i64 (i8*, i64)** %15, align 8
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %14, i64 0, i32 1
  %18 = load i8*, i8** %17, align 8
  %19 = tail call i64 %16(i8* %18, i64 %2)
  ret i64 %19
}

define i64 @"$lambda13"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %6, align 8
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
  %4 = bitcast i8* %3 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i8**
  store i8* %3, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f4, i64 (i8*, i64, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = bitcast i8* %7 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }**
  %14 = load { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }** %13, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f4, i64 (i8*, i64, i64)** %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %7, i8** %18, align 8
  %19 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %14, i64 0, i32 0
  %20 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %19, align 8
  %21 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %14, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call { i64 (i8*, i64)*, i8* }* %20(i8* %22, i64 3)
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %23, i64 0, i32 0
  %25 = load i64 (i8*, i64)*, i64 (i8*, i64)** %24, align 8
  %26 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %23, i64 0, i32 1
  %27 = load i8*, i8** %26, align 8
  %28 = tail call i64 %25(i8* %27, i64 4)
  ret i32 0
}
