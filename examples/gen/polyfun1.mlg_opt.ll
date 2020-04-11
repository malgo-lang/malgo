; ModuleID = './examples/gen/polyfun1.mlg.ll'
source_filename = "./examples/polyfun1.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$f95"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  %9 = ptrtoint i8* %8 to i64
  ret i64 %9
}

define i8* @"$f80"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i64 (i8*, i8*)**
  %4 = load i64 (i8*, i8*)*, i64 (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i64 %4(i8* %7, i8* %1)
  %9 = inttoptr i64 %8 to i8*
  ret i8* %9
}

define i64 @"$f55"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  ret i64 %9
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @succ4(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @succ4, i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = add i64 %1, 1
  ret i64 %7
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int7(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i8* @f0({ { i8* (i8*, i8*)*, i8* }*, i8* }* nocapture readonly) local_unnamed_addr {
  %2 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %0, i64 0, i32 0
  %3 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %2, align 8
  %4 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %0, i64 0, i32 1
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i64 0, i32 0
  %7 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %6, align 8
  %8 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i64 0, i32 1
  %9 = load i8*, i8** %8, align 8
  %10 = tail call i8* %7(i8* %9, i8* %5)
  ret i8* %10
}

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @succ4, i64 (i8*, i64)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$f55", i64 (i8*, i8*)** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i8**
  store i8* %5, i8** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i64*
  store i64 1, i64* %12, align 8
  %13 = tail call i8* @GC_malloc(i64 8)
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to { { i64 (i8*, i8*)*, i8* }*, i64 }***
  %16 = bitcast i8* %14 to i8**
  store i8* %13, i8** %16, align 8
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 1, i64* %18, align 8
  %19 = bitcast i8* %13 to i8**
  store i8* %9, i8** %19, align 8
  %20 = load { { i64 (i8*, i8*)*, i8* }*, i64 }**, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %15, align 8
  %21 = load { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %20, align 8
  %22 = tail call i8* @GC_malloc(i64 16)
  %23 = bitcast { { i64 (i8*, i8*)*, i8* }*, i64 }* %21 to i64*
  %24 = load i64, i64* %23, align 8
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$f80", i8* (i8*, i8*)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i64*
  store i64 %24, i64* %28, align 8
  %29 = bitcast i8* %22 to i8**
  store i8* %25, i8** %29, align 8
  %30 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %21, i64 0, i32 1
  %31 = load i64, i64* %30, align 8
  %32 = getelementptr i8, i8* %22, i64 8
  %33 = bitcast i8* %32 to i64*
  store i64 %31, i64* %33, align 8
  %.cast = inttoptr i64 %31 to i8*
  %34 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %26, align 8
  %35 = bitcast i8* %27 to i8**
  %36 = load i8*, i8** %35, align 8
  %37 = tail call i8* %34(i8* %36, i8* %.cast)
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %22 to i64*
  %40 = load i64, i64* %39, align 8
  %41 = tail call i8* @GC_malloc(i64 16)
  %42 = bitcast i8* %41 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$f95", i64 (i8*, i8*)** %42, align 8
  %43 = getelementptr i8, i8* %41, i64 8
  %44 = bitcast i8* %43 to i64*
  store i64 %40, i64* %44, align 8
  %45 = bitcast i8* %38 to i8**
  store i8* %41, i8** %45, align 8
  %46 = load i64, i64* %33, align 8
  %47 = getelementptr i8, i8* %38, i64 8
  %48 = bitcast i8* %47 to i64*
  store i64 %46, i64* %48, align 8
  %49 = ptrtoint i8* %37 to i64
  %50 = tail call {}* @print_int(i64 %49)
  ret i32 0
}
