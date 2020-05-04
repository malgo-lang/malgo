; ModuleID = './examples/gen/map.mlg.ll'
source_filename = "./examples/map.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @add426(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add426, i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = add i64 %1, 42
  ret i64 %7
}

define { i64, i64 }* @map1({ i64 (i8*, i64)*, i8* }* nocapture readonly, { i64, i64 }* nocapture readonly) local_unnamed_addr {
  %3 = getelementptr { i64, i64 }, { i64, i64 }* %1, i64 0, i32 0
  %4 = load i64, i64* %3, align 8
  %5 = getelementptr { i64, i64 }, { i64, i64 }* %1, i64 0, i32 1
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i64 0, i32 0
  %8 = load i64 (i8*, i64)*, i64 (i8*, i64)** %7, align 8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %0, i64 0, i32 1
  %10 = load i8*, i8** %9, align 8
  %11 = tail call i64 %8(i8* %10, i64 %4)
  %12 = load i64 (i8*, i64)*, i64 (i8*, i64)** %7, align 8
  %13 = load i8*, i8** %9, align 8
  %14 = tail call i64 %12(i8* %13, i64 %6)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { i64, i64 }*
  %17 = bitcast i8* %15 to i64*
  store i64 %11, i64* %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 %14, i64* %19, align 8
  ret { i64, i64 }* %16
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int0(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
  tail call void @GC_init()
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add426, i64 (i8*, i64)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to <2 x i64>*
  store <2 x i64> <i64 1, i64 2>, <2 x i64>* %7, align 8
  %8 = load i64 (i8*, i64)*, i64 (i8*, i64)** %3, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = tail call i64 %8(i8* %9, i64 1)
  %11 = load i64 (i8*, i64)*, i64 (i8*, i64)** %3, align 8
  %12 = load i8*, i8** %5, align 8
  %13 = tail call i64 %11(i8* %12, i64 2)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i64*
  store i64 %10, i64* %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i64*
  store i64 %13, i64* %17, align 8
  %18 = tail call {}* @print_int(i64 %10)
  ret i32 0
}
