; ModuleID = './examples/gen/test16.mlg.ll'
source_filename = "./examples/test16.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$fo72"(i8* nocapture readonly, i64) {
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

define i8* @"$fo59"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @"$fo47"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
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

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int6(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
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
  %16 = load i64, i64* %13, align 8
  %17 = getelementptr i8, i8* %12, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 %16, i64* %18, align 8
  %19 = tail call i8* %10(i8* %11, i8* %12)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %19 to i64*
  %22 = load i64, i64* %21, align 8
  %23 = tail call i8* @GC_malloc(i64 16)
  %24 = bitcast i8* %23 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo47", i8* (i8*, i8*)** %24, align 8
  %25 = getelementptr i8, i8* %23, i64 8
  %26 = bitcast i8* %25 to i64*
  store i64 %22, i64* %26, align 8
  %27 = bitcast i8* %20 to i8**
  store i8* %23, i8** %27, align 8
  %28 = load i64, i64* %21, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo59", i8* (i8*, i8*)** %30, align 8
  %31 = getelementptr i8, i8* %29, i64 8
  %32 = bitcast i8* %31 to i64*
  store i64 %28, i64* %32, align 8
  %33 = getelementptr i8, i8* %20, i64 8
  %34 = bitcast i8* %33 to i8**
  store i8* %29, i8** %34, align 8
  %35 = load i8*, i8** %27, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo72", i64 (i8*, i64)** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i8**
  store i8* %35, i8** %39, align 8
  %40 = bitcast i8* %35 to i8* (i8*, i8*)**
  %41 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %40, align 8
  %42 = getelementptr i8, i8* %35, i64 8
  %43 = bitcast i8* %42 to i8**
  %44 = load i8*, i8** %43, align 8
  %45 = tail call i8* %41(i8* %44, i8* inttoptr (i64 1 to i8*))
  %46 = ptrtoint i8* %45 to i64
  %47 = tail call {}* @print_int(i64 %46)
  ret i32 0
}
