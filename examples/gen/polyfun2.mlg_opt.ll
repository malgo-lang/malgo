; ModuleID = './examples/gen/polyfun2.mlg.ll'
source_filename = "./examples/polyfun2.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo167"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i64 (i8*, i8*)**
  %4 = load i64 (i8*, i8*)*, i64 (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i64 %4(i8* %7, i8* %1)
  %9 = inttoptr i64 %8 to i8*
  ret i8* %9
}

define i64 @"$fo116"(i8* nocapture readonly, i8*) {
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

define i8* @"$fo89"(i8* nocapture readonly, i8* nocapture readonly) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %5 = bitcast i8* %1 to i64*
  %6 = load i64, i64* %5, align 8
  %7 = tail call i8* @GC_malloc(i64 16)
  %8 = bitcast i8* %7 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo96", i8* (i8*, i8*)** %8, align 8
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

define i8* @"$fo96"(i8* nocapture readonly, i8*) {
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

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 16)
  %6 = bitcast i8* %5 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo89", i8* (i8*, i8*)** %6, align 8
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
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i8**
  store i8* %14, i8** %18, align 8
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$fo116", i64 (i8*, i8*)** %20, align 8
  %21 = getelementptr i8, i8* %19, i64 8
  %22 = bitcast i8* %21 to i8**
  store i8* %15, i8** %22, align 8
  %23 = tail call i8* @GC_malloc(i64 16)
  %24 = bitcast i8* %23 to i8**
  store i8* %10, i8** %24, align 8
  %25 = getelementptr i8, i8* %23, i64 8
  %26 = bitcast i8* %25 to i64*
  store i64 1, i64* %26, align 8
  %27 = tail call i8* @GC_malloc(i64 8)
  %28 = tail call i8* @GC_malloc(i64 16)
  %29 = bitcast i8* %28 to { { i8* (i8*, i8*)*, i8* }*, i64 }***
  %30 = bitcast i8* %28 to i8**
  store i8* %27, i8** %30, align 8
  %31 = getelementptr i8, i8* %28, i64 8
  %32 = bitcast i8* %31 to i64*
  store i64 1, i64* %32, align 8
  %33 = bitcast i8* %27 to i8**
  store i8* %23, i8** %33, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to i8**
  store i8* %19, i8** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i64*
  store i64 1, i64* %37, align 8
  %38 = tail call i8* @GC_malloc(i64 8)
  %39 = tail call i8* @GC_malloc(i64 16)
  %40 = bitcast i8* %39 to { { i64 (i8*, i8*)*, i8* }*, i64 }***
  %41 = bitcast i8* %39 to i8**
  store i8* %38, i8** %41, align 8
  %42 = getelementptr i8, i8* %39, i64 8
  %43 = bitcast i8* %42 to i64*
  store i64 1, i64* %43, align 8
  %44 = bitcast i8* %38 to i8**
  store i8* %34, i8** %44, align 8
  %45 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %29, align 8
  %46 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %45, align 8
  %47 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %6, align 8
  %48 = load i8*, i8** %8, align 8
  %49 = tail call i8* @GC_malloc(i64 16)
  %50 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %46 to i64*
  %51 = load i64, i64* %50, align 8
  %52 = bitcast i8* %49 to i64*
  store i64 %51, i64* %52, align 8
  %53 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %46, i64 0, i32 1
  %54 = load i64, i64* %53, align 8
  %55 = getelementptr i8, i8* %49, i64 8
  %56 = bitcast i8* %55 to i64*
  store i64 %54, i64* %56, align 8
  %57 = tail call i8* %47(i8* %48, i8* %49)
  %58 = ptrtoint i8* %57 to i64
  %59 = tail call {}* @print_int(i64 %58)
  %60 = load { { i64 (i8*, i8*)*, i8* }*, i64 }**, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %40, align 8
  %61 = load { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %60, align 8
  %62 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %6, align 8
  %63 = load i8*, i8** %8, align 8
  %64 = tail call i8* @GC_malloc(i64 16)
  %65 = bitcast { { i64 (i8*, i8*)*, i8* }*, i64 }* %61 to i64*
  %66 = load i64, i64* %65, align 8
  %67 = tail call i8* @GC_malloc(i64 16)
  %68 = bitcast i8* %67 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo167", i8* (i8*, i8*)** %68, align 8
  %69 = getelementptr i8, i8* %67, i64 8
  %70 = bitcast i8* %69 to i64*
  store i64 %66, i64* %70, align 8
  %71 = bitcast i8* %64 to i8**
  store i8* %67, i8** %71, align 8
  %72 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %61, i64 0, i32 1
  %73 = load i64, i64* %72, align 8
  %74 = getelementptr i8, i8* %64, i64 8
  %75 = bitcast i8* %74 to i64*
  store i64 %73, i64* %75, align 8
  %76 = tail call i8* %62(i8* %63, i8* %64)
  %77 = ptrtoint i8* %76 to i64
  %78 = tail call {}* @print_int(i64 %77)
  ret i32 0
}
