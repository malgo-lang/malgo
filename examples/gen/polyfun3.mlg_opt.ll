; ModuleID = './examples/gen/polyfun3.mlg.ll'
source_filename = "./examples/polyfun3.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define i8* @"$fo53"(i8* nocapture readonly, i8* nocapture readonly) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %5 = bitcast i8* %1 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo60", i8* (i8*, i8*)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i64*
  store i64 %6, i64* %10, align 8
  %11 = bitcast i8* %3 to i8**
  store i8* %7, i8** %11, align 8
  %12 = getelementptr i8, i8* %1, i64 8
  %13 = bitcast i8* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = getelementptr i8, i8* %3, i64 8
  %16 = bitcast i8* %15 to i64*
  store i64 %14, i64* %16, align 8
  %17 = bitcast i8* %0 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  %18 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %17, align 8
  %19 = getelementptr i8, i8* %0, i64 8
  %20 = bitcast i8* %19 to i8**
  %21 = load i8*, i8** %20, align 8
  %22 = tail call i8* %18(i8* %21, { { i8* (i8*, i8*)*, i8* }*, i8* }* %4)
  ret i8* %22
}

define i8* @"$fo60"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int7(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
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
  ret i8* %1
}

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
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo53", i8* (i8*, i8*)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %9, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8**
  store i8* %10, i8** %15, align 8
  %16 = getelementptr i8, i8* %14, i64 8
  %17 = bitcast i8* %16 to i64*
  store i64 1, i64* %17, align 8
  %18 = tail call i8* @GC_malloc(i64 8)
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to { { i8* (i8*, i8*)*, i8* }*, i64 }***
  %21 = bitcast i8* %19 to i8**
  store i8* %18, i8** %21, align 8
  %22 = getelementptr i8, i8* %19, i64 8
  %23 = bitcast i8* %22 to i64*
  store i64 1, i64* %23, align 8
  %24 = bitcast i8* %18 to i8**
  store i8* %14, i8** %24, align 8
  %25 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %20, align 8
  %26 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %25, align 8
  %27 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %6, align 8
  %28 = load i8*, i8** %8, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %26 to i64*
  %31 = load i64, i64* %30, align 8
  %32 = bitcast i8* %29 to i64*
  store i64 %31, i64* %32, align 8
  %33 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %26, i64 0, i32 1
  %34 = load i64, i64* %33, align 8
  %35 = getelementptr i8, i8* %29, i64 8
  %36 = bitcast i8* %35 to i64*
  store i64 %34, i64* %36, align 8
  %37 = tail call i8* %27(i8* %28, i8* %29)
  %38 = ptrtoint i8* %37 to i64
  %39 = tail call {}* @print_int(i64 %38)
  ret i32 0
}
