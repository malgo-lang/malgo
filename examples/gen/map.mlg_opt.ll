; ModuleID = './examples/gen/map.mlg.ll'
source_filename = "./examples/map.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @add42.20(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map.17, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add42.20, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = add i64 %1, 42
  ret i64 %11
}

define { i64, i64 }* @map.17(i8*, { i64 (i8*, i64)*, i8* }* nocapture readonly, { i64, i64 }* nocapture readonly) {
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map.17, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add42.20, i64 (i8*, i64)** %9, align 8
  %10 = getelementptr i8, i8* %8, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %0, i8** %11, align 8
  %12 = getelementptr { i64, i64 }, { i64, i64 }* %2, i64 0, i32 0
  %13 = load i64, i64* %12, align 8
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i64 0, i32 0
  %15 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14, align 8
  %16 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i64 0, i32 1
  %17 = load i8*, i8** %16, align 8
  %18 = tail call i64 %15(i8* %17, i64 %13)
  %19 = getelementptr { i64, i64 }, { i64, i64 }* %2, i64 0, i32 1
  %20 = load i64, i64* %19, align 8
  %21 = load i64 (i8*, i64)*, i64 (i8*, i64)** %14, align 8
  %22 = load i8*, i8** %16, align 8
  %23 = tail call i64 %21(i8* %22, i64 %20)
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to { i64, i64 }*
  %26 = bitcast i8* %24 to i64*
  store i64 %18, i64* %26, align 8
  %27 = getelementptr i8, i8* %24, i64 8
  %28 = bitcast i8* %27 to i64*
  store i64 %23, i64* %28, align 8
  ret { i64, i64 }* %25
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int.6(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map.17, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to { i64 (i8*, i64)*, i8* }*
  %9 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add42.20, i64 (i8*, i64)** %9, align 8
  %10 = getelementptr i8, i8* %7, i64 8
  %11 = bitcast i8* %10 to i8**
  store i8* %6, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to { i64, i64 }*
  %14 = bitcast i8* %12 to <2 x i64>*
  store <2 x i64> <i64 1, i64 2>, <2 x i64>* %14, align 8
  %15 = load { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %3, align 8
  %16 = load i8*, i8** %5, align 8
  %17 = tail call { i64, i64 }* %15(i8* %16, { i64 (i8*, i64)*, i8* }* %8, { i64, i64 }* %13)
  %18 = getelementptr { i64, i64 }, { i64, i64 }* %17, i64 0, i32 0
  %19 = load i64, i64* %18, align 8
  %20 = tail call {}* @print_int(i64 %19)
  ret i32 0
}
