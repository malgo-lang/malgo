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
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %0, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 8)
  %14 = bitcast i8* %13 to i64*
  store i64 %4, i64* %14, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { i64 (i8*, i64)*, i8* }*
  %17 = bitcast i8* %15 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i8**
  store i8* %13, i8** %19, align 8
  ret { i64 (i8*, i64)*, i8* }* %16
}

define i64 @f2(i8*, i64, i64) {
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = bitcast i8* %0 to i64*
  %13 = load i64, i64* %12, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %0, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %0, i8** %21, align 8
  %22 = tail call i8* @GC_malloc(i64 8)
  %23 = bitcast i8* %22 to i64*
  store i64 %13, i64* %23, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i8**
  store i8* %22, i8** %27, align 8
  %28 = load i64, i64* %23, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i8**
  store i8* %22, i8** %32, align 8
  %33 = add i64 %28, %2
  ret i64 %33
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

define i32 @main() local_unnamed_addr {
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
  %8 = bitcast i8* %7 to i64*
  store i64 42, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 16)
  %14 = bitcast i8* %13 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %14, align 8
  %15 = getelementptr i8, i8* %13, i64 8
  %16 = bitcast i8* %15 to i8**
  store i8* %7, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %18, align 8
  %19 = getelementptr i8, i8* %17, i64 8
  %20 = bitcast i8* %19 to i8**
  store i8* %7, i8** %20, align 8
  %21 = load i64, i64* %8, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast i8* %22 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k1, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %23, align 8
  %24 = getelementptr i8, i8* %22, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %7, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %27, align 8
  %28 = getelementptr i8, i8* %26, i64 8
  %29 = bitcast i8* %28 to i8**
  store i8* %7, i8** %29, align 8
  %30 = tail call i8* @GC_malloc(i64 8)
  %31 = bitcast i8* %30 to i64*
  store i64 %21, i64* %31, align 8
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %33, align 8
  %34 = getelementptr i8, i8* %32, i64 8
  %35 = bitcast i8* %34 to i8**
  store i8* %30, i8** %35, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda13", i64 (i8*, i64)** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i8**
  store i8* %30, i8** %39, align 8
  ret i32 0
}
