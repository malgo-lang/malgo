; ModuleID = './examples/gen/lambda.mlg.ll'
source_filename = "./examples/lambda.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int45(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline44() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @"$lambda43"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda43", i64 (i8*, i64)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = add i64 %1, 1
  ret i64 %9
}

define i64 @"$lambda42"(i8*, i64) {
  %3 = bitcast i8* %0 to i64*
  %4 = load i64, i64* %3, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda42", i64 (i8*, i64)** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = getelementptr i8, i8* %5, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %0, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = add i64 %4, %1
  ret i64 %11
}

define { i64 (i8*, i64)*, i8* }* @"$lambda41"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda41", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %4, align 8
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
  store i64 (i8*, i64)* @"$lambda42", i64 (i8*, i64)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %12, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %9, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  ret { i64 (i8*, i64)*, i8* }* %13
}

define i32 @main() local_unnamed_addr {
then_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda43", i64 (i8*, i64)** %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to { i64 (i8*, i64)*, i8* }* (i8*, i64)**
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda41", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = load i64 (i8*, i64)*, i64 (i8*, i64)** %2, align 8
  %15 = load i8*, i8** %5, align 8
  %16 = tail call i64 %14(i8* %15, i64 41)
  %17 = tail call {}* @print_int(i64 %16)
  %18 = tail call {}* @newline()
  %19 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %9, align 8
  %20 = load i8*, i8** %12, align 8
  %21 = tail call { i64 (i8*, i64)*, i8* }* %19(i8* %20, i64 2)
  %22 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i64 0, i32 0
  %23 = load i64 (i8*, i64)*, i64 (i8*, i64)** %22, align 8
  %24 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %21, i64 0, i32 1
  %25 = load i8*, i8** %24, align 8
  %26 = tail call i64 %23(i8* %25, i64 3)
  %27 = tail call {}* @print_int(i64 %26)
  %28 = tail call {}* @newline()
  ret i32 0
}
