; ModuleID = './examples/gen/polyfun2.mlg.ll'
source_filename = "./examples/polyfun2.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$f148"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  %9 = ptrtoint i8* %8 to i64
  ret i64 %9
}

define i8* @"$f133"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i64 (i8*, i8*)**
  %4 = load i64 (i8*, i8*)*, i64 (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i64 %4(i8* %7, i8* %1)
  %9 = inttoptr i64 %8 to i8*
  ret i8* %9
}

define i64 @"$f83"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  ret i64 %9
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int10(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i64 @addOne6(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne6, i64 (i8*, i64)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = add i64 %1, 1
  ret i64 %7
}

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
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne6, i64 (i8*, i64)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$f83", i64 (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %6, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 16)
  %15 = bitcast i8* %14 to i8**
  store i8* %1, i8** %15, align 8
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
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to i8**
  store i8* %10, i8** %26, align 8
  %27 = getelementptr i8, i8* %25, i64 8
  %28 = bitcast i8* %27 to i64*
  store i64 1, i64* %28, align 8
  %29 = tail call i8* @GC_malloc(i64 8)
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to { { i64 (i8*, i8*)*, i8* }*, i64 }***
  %32 = bitcast i8* %30 to i8**
  store i8* %29, i8** %32, align 8
  %33 = getelementptr i8, i8* %30, i64 8
  %34 = bitcast i8* %33 to i64*
  store i64 1, i64* %34, align 8
  %35 = bitcast i8* %29 to i8**
  store i8* %25, i8** %35, align 8
  %36 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %20, align 8
  %37 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %36, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %37 to i64*
  %40 = load i64, i64* %39, align 8
  %41 = bitcast i8* %38 to i64*
  store i64 %40, i64* %41, align 8
  %42 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %37, i64 0, i32 1
  %43 = load i64, i64* %42, align 8
  %44 = getelementptr i8, i8* %38, i64 8
  %45 = bitcast i8* %44 to i64*
  store i64 %43, i64* %45, align 8
  %46 = inttoptr i64 %40 to { i8* (i8*, i8*)*, i8* }*
  %.cast = inttoptr i64 %43 to i8*
  %47 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %46, i64 0, i32 0
  %48 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %47, align 8
  %49 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %46, i64 0, i32 1
  %50 = load i8*, i8** %49, align 8
  %51 = tail call i8* %48(i8* %50, i8* %.cast)
  %52 = tail call i8* @GC_malloc(i64 16)
  %53 = load i64, i64* %41, align 8
  %54 = bitcast i8* %52 to i64*
  store i64 %53, i64* %54, align 8
  %55 = load i64, i64* %45, align 8
  %56 = getelementptr i8, i8* %52, i64 8
  %57 = bitcast i8* %56 to i64*
  store i64 %55, i64* %57, align 8
  %58 = ptrtoint i8* %51 to i64
  %59 = tail call {}* @print_int(i64 %58)
  %60 = load { { i64 (i8*, i8*)*, i8* }*, i64 }**, { { i64 (i8*, i8*)*, i8* }*, i64 }*** %31, align 8
  %61 = load { { i64 (i8*, i8*)*, i8* }*, i64 }*, { { i64 (i8*, i8*)*, i8* }*, i64 }** %60, align 8
  %62 = tail call i8* @GC_malloc(i64 16)
  %63 = bitcast { { i64 (i8*, i8*)*, i8* }*, i64 }* %61 to i64*
  %64 = load i64, i64* %63, align 8
  %65 = tail call i8* @GC_malloc(i64 16)
  %66 = bitcast i8* %65 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$f133", i8* (i8*, i8*)** %66, align 8
  %67 = getelementptr i8, i8* %65, i64 8
  %68 = bitcast i8* %67 to i64*
  store i64 %64, i64* %68, align 8
  %69 = bitcast i8* %62 to i8**
  store i8* %65, i8** %69, align 8
  %70 = getelementptr { { i64 (i8*, i8*)*, i8* }*, i64 }, { { i64 (i8*, i8*)*, i8* }*, i64 }* %61, i64 0, i32 1
  %71 = load i64, i64* %70, align 8
  %72 = getelementptr i8, i8* %62, i64 8
  %73 = bitcast i8* %72 to i64*
  store i64 %71, i64* %73, align 8
  %.cast2 = inttoptr i64 %71 to i8*
  %74 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %66, align 8
  %75 = bitcast i8* %67 to i8**
  %76 = load i8*, i8** %75, align 8
  %77 = tail call i8* %74(i8* %76, i8* %.cast2)
  %78 = tail call i8* @GC_malloc(i64 16)
  %79 = bitcast i8* %62 to i64*
  %80 = load i64, i64* %79, align 8
  %81 = tail call i8* @GC_malloc(i64 16)
  %82 = bitcast i8* %81 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$f148", i64 (i8*, i8*)** %82, align 8
  %83 = getelementptr i8, i8* %81, i64 8
  %84 = bitcast i8* %83 to i64*
  store i64 %80, i64* %84, align 8
  %85 = bitcast i8* %78 to i8**
  store i8* %81, i8** %85, align 8
  %86 = load i64, i64* %73, align 8
  %87 = getelementptr i8, i8* %78, i64 8
  %88 = bitcast i8* %87 to i64*
  store i64 %86, i64* %88, align 8
  %89 = ptrtoint i8* %77 to i64
  %90 = tail call {}* @print_int(i64 %89)
  ret i32 0
}
