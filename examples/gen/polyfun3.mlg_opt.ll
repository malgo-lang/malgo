; ModuleID = './examples/gen/polyfun3.mlg.ll'
source_filename = "./examples/polyfun3.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int7(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i8* @id4(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  ret i8* %1
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
  %2 = bitcast i8* %1 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = getelementptr i8, i8* %5, i64 8
  %8 = bitcast i8* %7 to i64*
  store i64 1, i64* %8, align 8
  %9 = tail call i8* @GC_malloc(i64 8)
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to { { i8* (i8*, i8*)*, i8* }*, i64 }***
  %12 = bitcast i8* %10 to i8**
  store i8* %9, i8** %12, align 8
  %13 = getelementptr i8, i8* %10, i64 8
  %14 = bitcast i8* %13 to i64*
  store i64 1, i64* %14, align 8
  %15 = bitcast i8* %9 to i8**
  store i8* %5, i8** %15, align 8
  %16 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %11, align 8
  %17 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %16, align 8
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %17 to i64*
  %20 = load i64, i64* %19, align 8
  %21 = bitcast i8* %18 to i64*
  store i64 %20, i64* %21, align 8
  %22 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %17, i64 0, i32 1
  %23 = load i64, i64* %22, align 8
  %24 = getelementptr i8, i8* %18, i64 8
  %25 = bitcast i8* %24 to i64*
  store i64 %23, i64* %25, align 8
  %26 = inttoptr i64 %20 to { i8* (i8*, i8*)*, i8* }*
  %.cast = inttoptr i64 %23 to i8*
  %27 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %26, i64 0, i32 0
  %28 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %27, align 8
  %29 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %26, i64 0, i32 1
  %30 = load i8*, i8** %29, align 8
  %31 = tail call i8* %28(i8* %30, i8* %.cast)
  %32 = ptrtoint i8* %31 to i64
  %33 = tail call {}* @print_int(i64 %32)
  ret i32 0
}
