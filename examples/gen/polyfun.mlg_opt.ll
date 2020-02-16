; ModuleID = './examples/gen/polyfun.mlg.ll'
source_filename = "./examples/polyfun.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$fo125"(i8* nocapture readonly, i64) {
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

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64, { i8**, i64 }* }* @"$fo109"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  %4 = load { i8*, { i8**, i64 }* }* (i8*, i8*)*, { i8*, { i8**, i64 }* }* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call { i8*, { i8**, i64 }* }* %4(i8* %7, i8* %1)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { i64, { i8**, i64 }* }*
  %11 = bitcast { i8*, { i8**, i64 }* }* %8 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = bitcast i8* %9 to i64*
  store i64 %12, i64* %13, align 8
  %14 = getelementptr { i8*, { i8**, i64 }* }, { i8*, { i8**, i64 }* }* %8, i64 0, i32 1
  %15 = bitcast { i8**, i64 }** %14 to i64*
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr i8, i8* %9, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 %16, i64* %18, align 8
  ret { i64, { i8**, i64 }* }* %10
}

define i8* @"$fo95"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int10(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i8* @id0(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  ret i8* %1
}

define i64 @addOne1(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %8, align 8
  %9 = getelementptr i8, i8* %7, i64 8
  %10 = bitcast i8* %9 to i8**
  store i8* %0, i8** %10, align 8
  %11 = add i64 %1, 1
  ret i64 %11
}

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda34"(i8*, i8*) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda34", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %11 = bitcast i8* %9 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i8*, { i8**, i64 }* }* (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %10
}

define { i8*, { i8**, i64 }* }* @"$lambda33"(i8*, i8*) {
body_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda33", { i8*, { i8**, i64 }* }* (i8*, i8*)** %5, align 8
  %6 = getelementptr i8, i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 8)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i8**
  store i8* %8, i8** %10, align 8
  %11 = getelementptr i8, i8* %9, i64 8
  %12 = bitcast i8* %11 to i64*
  store i64 1, i64* %12, align 8
  %13 = bitcast i8* %8 to i8**
  store i8* %1, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to { i8*, { i8**, i64 }* }*
  %16 = bitcast i8* %14 to i64*
  store i64 %3, i64* %16, align 8
  %17 = getelementptr i8, i8* %14, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %9, i8** %18, align 8
  ret { i8*, { i8**, i64 }* }* %15
}

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne1, i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to { i64 (i8*, i64)*, i8* }***
  %13 = bitcast i8* %11 to i8**
  store i8* %10, i8** %13, align 8
  %14 = getelementptr i8, i8* %11, i64 8
  %15 = bitcast i8* %14 to i64*
  store i64 1, i64* %15, align 8
  %16 = bitcast i8* %10 to i8**
  store i8* %6, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda34", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %20 = getelementptr i8, i8* %18, i64 8
  %21 = bitcast i8* %20 to i8**
  store i8* %17, i8** %21, align 8
  %22 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %2, align 8
  %23 = load i8*, i8** %4, align 8
  %24 = tail call i8* %22(i8* %23, i8* %1)
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo95", i8* (i8*, i8*)** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %24, i8** %28, align 8
  %29 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %30 = load i8*, i8** %21, align 8
  %31 = tail call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %29(i8* %30, i8* inttoptr (i64 1 to i8*))
  %32 = tail call i8* @GC_malloc(i64 16)
  %33 = bitcast i8* %32 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$fo109", { i64, { i8**, i64 }* }* (i8*, i8*)** %33, align 8
  %34 = getelementptr i8, i8* %32, i64 8
  %35 = bitcast i8* %34 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %31, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }** %35, align 8
  %36 = bitcast i8* %11 to i8***
  %37 = load i8**, i8*** %36, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast i8* %38 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo125", i64 (i8*, i64)** %39, align 8
  %40 = getelementptr i8, i8* %38, i64 8
  %41 = bitcast i8* %40 to i8**
  store i8* %1, i8** %41, align 8
  store i8* %38, i8** %37, align 8
  %42 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %12, align 8
  %43 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %42, align 8
  %44 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %43, i64 0, i32 0
  %45 = load i64 (i8*, i64)*, i64 (i8*, i64)** %44, align 8
  %46 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %43, i64 0, i32 1
  %47 = load i8*, i8** %46, align 8
  %48 = tail call i64 %45(i8* %47, i64 1)
  %49 = tail call {}* @print_int(i64 %48)
  ret i32 0
}
