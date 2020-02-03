; ModuleID = './examples/gen/polyfun1.mlg.ll'
source_filename = "./examples/polyfun1.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo86"(i8* nocapture readonly, i8*) {
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

define i64 @succ1(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = add i64 %1, 1
  ret i64 %11
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int7(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i8* @f0(i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }* nocapture readonly) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 0
  %12 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %11, align 8
  %13 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 1
  %14 = load i8*, i8** %13, align 8
  %15 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i64 0, i32 0
  %16 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %15, align 8
  %17 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i64 0, i32 1
  %18 = load i8*, i8** %17, align 8
  %19 = tail call i8* %16(i8* %18, i8* %14)
  ret i8* %19
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
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i8**
  store i8* %6, i8** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i64*
  store i64 1, i64* %13, align 8
  %14 = tail call i8* @GC_malloc(i64 8)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { { i64 (i8*, i64)*, i8* }*, i64 }***
  %17 = bitcast i8* %15 to i8**
  store i8* %14, i8** %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 1, i64* %19, align 8
  %20 = bitcast i8* %14 to i8**
  store i8* %10, i8** %20, align 8
  %21 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %16, align 8
  %22 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %21, align 8
  %23 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %24 = load i8*, i8** %4, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %27 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }* %22 to i64*
  %28 = load i64, i64* %27, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo86", i8* (i8*, i8*)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i64*
  store i64 %28, i64* %32, align 8
  %33 = bitcast i8* %25 to i8**
  store i8* %29, i8** %33, align 8
  %34 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %22, i64 0, i32 1
  %35 = load i64, i64* %34, align 8
  %36 = getelementptr i8, i8* %25, i64 8
  %37 = bitcast i8* %36 to i64*
  store i64 %35, i64* %37, align 8
  %38 = tail call i8* %23(i8* %24, { { i8* (i8*, i8*)*, i8* }*, i8* }* %26)
  %39 = ptrtoint i8* %38 to i64
  %40 = tail call {}* @print_int(i64 %39)
  ret i32 0
}
