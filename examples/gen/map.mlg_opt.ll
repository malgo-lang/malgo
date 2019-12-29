; ModuleID = './examples/gen/map.mlg.ll'
source_filename = "./examples/map.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main35() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { i64 (i8*, i64)*, i8* }*
  %11 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %9, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %8, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %16 to { i64, i64 }*
  %18 = bitcast i8* %16 to i64*
  store i64 1, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = getelementptr i8, i8* %16, i64 8
  %21 = bitcast i8* %20 to i64*
  store i64 2, i64* %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = load { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)*, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %3, align 8
  %24 = load i8*, i8** %6, align 8
  %25 = tail call { i64, i64 }* %23(i8* %24, { i64 (i8*, i64)*, i8* }* %10, { i64, i64 }* %17)
  %26 = getelementptr { i64, i64 }, { i64, i64 }* %25, i64 0, i32 0
  %27 = load i64, i64* %26, align 8
  %28 = tail call {}* @print_int(i64 %27)
  ret i32 0
}

define i64 @add4236(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = add i64 %1, 42
  ret i64 %15
}

define { i64, i64 }* @map37(i8*, { i64 (i8*, i64)*, i8* }* nocapture readonly, { i64, i64 }* nocapture readonly) {
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)**
  store { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)* @map37, { i64, i64 }* (i8*, { i64 (i8*, i64)*, i8* }*, { i64, i64 }*)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @add4236, i64 (i8*, i64)** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 0)
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr { i64, i64 }, { i64, i64 }* %2, i64 0, i32 0
  %17 = load i64, i64* %16, align 8
  %18 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i64 0, i32 0
  %19 = load i64 (i8*, i64)*, i64 (i8*, i64)** %18, align 8
  %20 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %1, i64 0, i32 1
  %21 = load i8*, i8** %20, align 8
  %22 = tail call i64 %19(i8* %21, i64 %17)
  %23 = getelementptr { i64, i64 }, { i64, i64 }* %2, i64 0, i32 1
  %24 = load i64, i64* %23, align 8
  %25 = load i64 (i8*, i64)*, i64 (i8*, i64)** %18, align 8
  %26 = load i8*, i8** %20, align 8
  %27 = tail call i64 %25(i8* %26, i64 %24)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to { i64, i64 }*
  %30 = bitcast i8* %28 to i64*
  store i64 %22, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = getelementptr i8, i8* %28, i64 8
  %33 = bitcast i8* %32 to i64*
  store i64 %27, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  ret { i64, i64 }* %29
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int38(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @main35()
  ret i32 0
}
