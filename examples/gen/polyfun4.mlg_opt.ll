; ModuleID = './examples/gen/polyfun4.mlg.ll'
source_filename = "./examples/polyfun4.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readonly
define i8* @snd0({ i8*, i8* }* nocapture readonly) local_unnamed_addr #0 {
  %2 = getelementptr { i8*, i8* }, { i8*, i8* }* %0, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8
  ret i8* %3
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda34"(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)**
  store { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)* @"$lambda34", { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i64*
  store i64 %1, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %11 = bitcast i8* %9 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i64, { i8**, i64 }* }* (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %10
}

define { i64, { i8**, i64 }* }* @"$lambda33"(i8*, i8*) {
body_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i64, { i8**, i64 }* }* (i8*, i8*)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 8)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i8**
  store i8* %8, i8** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i64*
  store i64 1, i64* %12, align 8
  %13 = bitcast i8* %8 to i8**
  store i8* %1, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to { i64, { i8**, i64 }* }*
  %16 = bitcast i8* %14 to i64*
  store i64 %3, i64* %16, align 8
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %9, i8** %18, align 8
  ret { i64, { i8**, i64 }* }* %15
}

define i64 @"$lambda26"(i8*, i64 returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda26", i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  ret i64 %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda26", i64 (i8*, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$lambda26", i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %1, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)**
  store { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)* @"$lambda34", { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %10, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i64*
  store i64 1, i64* %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %11, i8** %18, align 8
  %19 = load { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)*, { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i64)** %12, align 8
  %20 = load i8*, i8** %14, align 8
  %21 = tail call { { i64, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %19(i8* %20, i64 1)
  %22 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}

attributes #0 = { norecurse nounwind readonly }
