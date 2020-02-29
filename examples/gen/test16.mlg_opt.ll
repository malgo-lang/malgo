; ModuleID = './examples/gen/test16.mlg.ll'
source_filename = "./examples/test16.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo60"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i64 @"$fo49"(i8* nocapture readonly, i64) {
  %3 = inttoptr i64 %1 to i8*
  %4 = bitcast i8* %0 to i8* (i8*, i8*)**
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i8* %5(i8* %8, i8* %3)
  %10 = ptrtoint i8* %9 to i64
  ret i64 %10
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int8(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i8* @id0(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  ret i8* %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %3, align 8
  %4 = getelementptr i8, i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %1, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i8**
  store i8* %2, i8** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %2, i8** %9, align 8
  %10 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %11 = load i8*, i8** %5, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %6 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = bitcast i8* %12 to i64*
  store i64 %14, i64* %15, align 8
  %16 = bitcast i8* %8 to i64*
  %17 = load i64, i64* %16, align 8
  %18 = getelementptr i8, i8* %12, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 %17, i64* %19, align 8
  %20 = tail call i8* %10(i8* %11, i8* %12)
  %21 = tail call i8* @GC_malloc(i64 16)
  %22 = bitcast i8* %20 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo49", i64 (i8*, i64)** %25, align 8
  %26 = getelementptr i8, i8* %24, i64 8
  %27 = bitcast i8* %26 to i64*
  store i64 %23, i64* %27, align 8
  %28 = bitcast i8* %21 to { i64 (i8*, i64)*, i8* }**
  %29 = bitcast i8* %21 to i8**
  store i8* %24, i8** %29, align 8
  %30 = getelementptr i8, i8* %20, i64 8
  %31 = bitcast i8* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo60", i8* (i8*, i8*)** %34, align 8
  %35 = getelementptr i8, i8* %33, i64 8
  %36 = bitcast i8* %35 to i64*
  store i64 %32, i64* %36, align 8
  %37 = getelementptr i8, i8* %21, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %33, i8** %38, align 8
  %39 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %28, align 8
  %40 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %39, i64 0, i32 0
  %41 = load i64 (i8*, i64)*, i64 (i8*, i64)** %40, align 8
  %42 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %39, i64 0, i32 1
  %43 = load i8*, i8** %42, align 8
  %44 = tail call i64 %41(i8* %43, i64 1)
  %45 = tail call {}* @print_int(i64 %44)
  ret i32 0
}
