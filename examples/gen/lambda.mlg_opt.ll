; ModuleID = './examples/gen/lambda.mlg.ll'
source_filename = "./examples/lambda.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64 (i8*, i64)*, i8* }* @"$lambda.31"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.31", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i64*
  store i64 %1, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }*
  %11 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.32", i64 (i8*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { i64 (i8*, i64)*, i8* }* %10
}

define i64 @"$lambda.32"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.32", i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

define i64 @"$lambda.29"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.29", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = add i64 %1, 1
  ret i64 %7
}

declare {}* @newline({}*) local_unnamed_addr

define {}* @newline.10({}*) local_unnamed_addr {
  %2 = tail call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.9(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
then_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda.29", i64 (i8*, i64)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.31", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = load i64 (i8*, i64)*, i64 (i8*, i64)** %2, align 8
  %11 = load i8*, i8** %4, align 8
  %12 = tail call i64 %10(i8* %11, i64 41)
  %13 = tail call {}* @print_int(i64 %12)
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = bitcast i8* %14 to {}*
  %16 = tail call {}* @newline({}* %15)
  %17 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7, align 8
  %18 = load i8*, i8** %9, align 8
  %19 = tail call { i64 (i8*, i64)*, i8* }* %17(i8* %18, i64 2)
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i64 0, i32 0
  %21 = load i64 (i8*, i64)*, i64 (i8*, i64)** %20, align 8
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %19, i64 0, i32 1
  %23 = load i8*, i8** %22, align 8
  %24 = tail call i64 %21(i8* %23, i64 3)
  %25 = tail call {}* @print_int(i64 %24)
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %26 to {}*
  %28 = tail call {}* @newline({}* %27)
  ret i32 0
}
