; ModuleID = './examples/gen/test5.mlg.ll'
source_filename = "./examples/test5.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @"$lambda28"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda28", i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = add i64 %4, %1
  ret i64 %11
}

define { i64 (i8*, i64)*, i8* }* @k27(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = tail call i8* @GC_malloc(i64 8)
  %18 = bitcast i8* %17 to i64*
  store i64 %4, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to { i64 (i8*, i64)*, i8* }*
  %22 = bitcast i8* %20 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda28", i64 (i8*, i64)** %22, align 8
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = getelementptr i8, i8* %20, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %17, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 0)
  ret { i64 (i8*, i64)*, i8* }* %21
}

define i64 @f26(i8*, i64, i64) {
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = tail call { i64 (i8*, i64)*, i8* }* @k27(i8* %0, i64 undef)
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i64 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17, align 8
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %16, i64 0, i32 1
  %20 = load i8*, i8** %19, align 8
  %21 = tail call i64 %18(i8* %20, i64 %2)
  ret i64 %21
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @k27, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = bitcast i8* %10 to i64*
  store i64 42, i64* %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = tail call i8* @GC_malloc(i64 16)
  %14 = bitcast i8* %13 to i64 (i8*, i64, i64)**
  store i64 (i8*, i64, i64)* @f26, i64 (i8*, i64, i64)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %13, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %10, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = load i64 (i8*, i64, i64)*, i64 (i8*, i64, i64)** %14, align 8
  %20 = load i8*, i8** %17, align 8
  %21 = tail call i64 %19(i8* %20, i64 3, i64 4)
  ret i32 0
}
