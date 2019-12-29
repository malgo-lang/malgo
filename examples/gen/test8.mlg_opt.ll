; ModuleID = './examples/gen/test8.mlg.ll'
source_filename = "./examples/test8.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main25() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 8)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f27, i64 (i8*, i64)** %5, align 8
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
  %14 = bitcast i8* %13 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g26, i64 (i8*, i64)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %13, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %10, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  %19 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14, align 8
  %20 = load i8*, i8** %17, align 8
  %21 = tail call i64 %19(i8* %20, i64 4)
  %22 = load i64 (i8*, i64)*, i64 (i8*, i64)** %5, align 8
  %23 = load i8*, i8** %8, align 8
  %24 = tail call i64 %22(i8* %23, i64 5)
  ret i32 0
}

define i64 @g26(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f27, i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g26, i64 (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = load i64, i64* %3, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f27, i64 (i8*, i64)** %19, align 8
  %20 = tail call i8* @GC_malloc(i64 0)
  %21 = getelementptr i8, i8* %18, i64 8
  %22 = bitcast i8* %21 to i8**
  store i8* %0, i8** %22, align 8
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g26, i64 (i8*, i64)** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = getelementptr i8, i8* %24, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %0, i8** %28, align 8
  %29 = tail call i8* @GC_malloc(i64 0)
  %30 = add i64 %4, %1
  %31 = add i64 %30, %17
  ret i64 %31
}

define i64 @f27(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f27, i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @g26, i64 (i8*, i64)** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i8**
  store i8* %0, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = add i64 %4, %1
  ret i64 %17
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @main25()
  ret i32 0
}
