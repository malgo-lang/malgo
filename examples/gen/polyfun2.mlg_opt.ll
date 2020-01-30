; ModuleID = './examples/gen/polyfun2.mlg.ll'
source_filename = "./examples/polyfun2.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo128"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  %10 = inttoptr i64 %9 to i8*
  ret i8* %10
}

define i8* @"$fo113"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  %10 = inttoptr i64 %9 to i8*
  ret i8* %10
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i8* @f0(i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }* nocapture readonly) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 0
  %16 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %15, align 8
  %17 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 1
  %18 = load i8*, i8** %17, align 8
  %19 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %16, i64 0, i32 0
  %20 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %19, align 8
  %21 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %16, i64 0, i32 1
  %22 = load i8*, i8** %21, align 8
  %23 = tail call i8* %20(i8* %22, i8* %18)
  ret i8* %23
}

define i8* @id1(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  ret i8* %1
}

define i64 @addOne2(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %0, i8** %14, align 8
  %15 = add i64 %1, 1
  ret i64 %15
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int9(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %10, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i8**
  store i8* %11, i8** %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 1, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 8)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to i8**
  store i8* %19, i8** %21, align 8
  %22 = getelementptr i8, i8* %20, i64 8
  %23 = bitcast i8* %22 to i64*
  store i64 1, i64* %23, align 8
  %24 = bitcast i8* %19 to i8**
  store i8* %15, i8** %24, align 8
  %25 = bitcast i8* %20 to i64***
  %26 = load i64**, i64*** %25, align 8
  %27 = load i64*, i64** %26, align 8
  %28 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %29 = load i8*, i8** %4, align 8
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %32 = load i64, i64* %27, align 8
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo113", i8* (i8*, i8*)** %34, align 8
  %35 = getelementptr i8, i8* %33, i64 8
  %36 = bitcast i8* %35 to i64*
  store i64 %32, i64* %36, align 8
  %37 = bitcast i8* %30 to i8**
  store i8* %33, i8** %37, align 8
  %38 = load i64, i64* %27, align 8
  %39 = tail call i8* @GC_malloc(i64 16)
  %40 = bitcast i8* %39 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo128", i8* (i8*, i8*)** %40, align 8
  %41 = getelementptr i8, i8* %39, i64 8
  %42 = bitcast i8* %41 to i64*
  store i64 %38, i64* %42, align 8
  %43 = getelementptr i8, i8* %30, i64 8
  %44 = bitcast i8* %43 to i8**
  store i8* %39, i8** %44, align 8
  %45 = tail call i8* %28(i8* %29, { { i8* (i8*, i8*)*, i8* }*, i8* }* %31)
  %46 = ptrtoint i8* %45 to i64
  %47 = tail call {}* @print_int(i64 %46)
  ret i32 0
}
