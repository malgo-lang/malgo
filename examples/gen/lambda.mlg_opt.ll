; ModuleID = './examples/gen/lambda.mlg.ll'
source_filename = "./examples/lambda.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64 (i8*, i64)*, i8* }* @"$lambda25"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda25", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i64*
  store i64 %1, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }*
  %11 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda24", i64 (i8*, i64)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { i64 (i8*, i64)*, i8* }* %10
}

define i64 @"$lambda24"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda24", i64 (i8*, i64)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = add i64 %4, %1
  ret i64 %9
}

define i64 @"$lambda23"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda23", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = add i64 %1, 1
  ret i64 %7
}

declare {}* @newline() local_unnamed_addr

define {}* @newline1() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int0(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
then_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda23", i64 (i8*, i64)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda25", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = load i64 (i8*, i64)*, i64 (i8*, i64)** %2, align 8
  %11 = load i8*, i8** %4, align 8
  %12 = tail call i64 %10(i8* %11, i64 41)
  %13 = tail call {}* @print_int(i64 %12)
  %14 = tail call {}* @newline()
  %15 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %7, align 8
  %16 = load i8*, i8** %9, align 8
  %17 = tail call { i64 (i8*, i64)*, i8* }* %15(i8* %16, i64 2)
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i64 0, i32 0
  %19 = load i64 (i8*, i64)*, i64 (i8*, i64)** %18, align 8
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %17, i64 0, i32 1
  %21 = load i8*, i8** %20, align 8
  %22 = tail call i64 %19(i8* %21, i64 3)
  %23 = tail call {}* @print_int(i64 %22)
  %24 = tail call {}* @newline()
  ret i32 0
}
