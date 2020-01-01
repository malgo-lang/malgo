; ModuleID = './examples/gen/polyfun.mlg.ll'
source_filename = "./examples/polyfun.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @fo187(i8* nocapture readonly, i64) {
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

define { i64, { i8**, i64 }* }* @fo156(i8* nocapture readonly, i8*) {
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
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = bitcast { i8*, { i8**, i64 }* }* %8 to { i8**, i64 }**
  %16 = load { i8**, i64 }*, { i8**, i64 }** %15, align 8
  %17 = getelementptr { i8**, i64 }, { i8**, i64 }* %16, i64 0, i32 0
  %18 = load i8**, i8*** %17, align 8
  %19 = getelementptr { i8**, i64 }, { i8**, i64 }* %16, i64 0, i32 1
  %20 = load i64, i64* %19, align 8
  %21 = shl i64 %20, 3
  %22 = tail call i8* @GC_malloc(i64 %21)
  %23 = tail call i8* @GC_malloc(i64 16)
  %24 = bitcast i8* %23 to i8**
  store i8* %22, i8** %24, align 8
  %25 = getelementptr i8, i8* %23, i64 8
  %26 = bitcast i8* %25 to i64*
  %.cast = bitcast i8* %22 to i8**
  store i64 %20, i64* %26, align 8
  %27 = tail call i8* @GC_malloc(i64 0)
  %28 = icmp sgt i64 %20, 0
  br i1 %28, label %body_0, label %end_0

body_0:                                           ; preds = %2, %body_0
  %.01 = phi i64 [ %35, %body_0 ], [ 0, %2 ]
  %29 = getelementptr i8*, i8** %18, i64 %.01
  %30 = bitcast i8** %29 to i64*
  %31 = load i64, i64* %30, align 8
  %32 = getelementptr i8*, i8** %.cast, i64 %.01
  %33 = bitcast i8** %32 to i64*
  store i64 %31, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = add nuw nsw i64 %.01, 1
  %exitcond = icmp eq i64 %35, %20
  br i1 %exitcond, label %end_0, label %body_0

end_0:                                            ; preds = %body_0, %2
  %36 = tail call i8* @GC_malloc(i64 0)
  %37 = getelementptr i8, i8* %9, i64 8
  %38 = bitcast i8* %37 to i8**
  store i8* %23, i8** %38, align 8
  %39 = tail call i8* @GC_malloc(i64 0)
  ret { i64, { i8**, i64 }* }* %10
}

define i8* @fo140(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @id60(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  ret i8* %1
}

define i64 @addOne59(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = add i64 %1, 1
  ret i64 %15
}

define { i8*, { i8**, i64 }* }* @"$lambda58"(i8*, i8*) {
body_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda58", { i8*, { i8**, i64 }* }* (i8*, i8*)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i8**
  store i8* %10, i8** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i64*
  store i64 1, i64* %14, align 8
  %15 = bitcast i8* %10 to i8**
  store i8* %1, i8** %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { i8*, { i8**, i64 }* }*
  %20 = bitcast i8* %18 to i64*
  store i64 %3, i64* %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = getelementptr i8, i8* %18, i64 8
  %23 = bitcast i8* %22 to i8**
  store i8* %11, i8** %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  ret { i8*, { i8**, i64 }* }* %19
}

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda57"(i8*, i8*) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda57", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 8)
  %10 = bitcast i8* %9 to i8**
  store i8* %1, i8** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %14 = bitcast i8* %12 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda58", { i8*, { i8**, i64 }* }* (i8*, i8*)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %12, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %9, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %13
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int56(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id60, i8* (i8*, i8*)** %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne59, i64 (i8*, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 8)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { i64 (i8*, i64)*, i8* }***
  %17 = bitcast i8* %15 to i8**
  store i8* %14, i8** %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 1, i64* %19, align 8
  %20 = bitcast i8* %14 to i8**
  store i8* %8, i8** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = tail call i8* @GC_malloc(i64 16)
  %25 = bitcast i8* %24 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda57", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = getelementptr i8, i8* %24, i64 8
  %28 = bitcast i8* %27 to i8**
  store i8* %23, i8** %28, align 8
  %29 = tail call i8* @GC_malloc(i64 0)
  %30 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %2, align 8
  %31 = load i8*, i8** %5, align 8
  %32 = tail call i8* %30(i8* %31, i8* %1)
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast i8* %33 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fo140, i8* (i8*, i8*)** %34, align 8
  %35 = tail call i8* @GC_malloc(i64 0)
  %36 = getelementptr i8, i8* %33, i64 8
  %37 = bitcast i8* %36 to i8**
  store i8* %32, i8** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 0)
  %39 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %25, align 8
  %40 = load i8*, i8** %28, align 8
  %41 = tail call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %39(i8* %40, i8* inttoptr (i64 1 to i8*))
  %42 = tail call i8* @GC_malloc(i64 16)
  %43 = bitcast i8* %42 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @fo156, { i64, { i8**, i64 }* }* (i8*, i8*)** %43, align 8
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = getelementptr i8, i8* %42, i64 8
  %46 = bitcast i8* %45 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %41, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }** %46, align 8
  %47 = tail call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %15 to i8***
  %49 = load i8**, i8*** %48, align 8
  %50 = tail call i8* @GC_malloc(i64 16)
  %51 = bitcast i8* %50 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @fo187, i64 (i8*, i64)** %51, align 8
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = getelementptr i8, i8* %50, i64 8
  %54 = bitcast i8* %53 to i8**
  store i8* %1, i8** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  store i8* %50, i8** %49, align 8
  %56 = tail call i8* @GC_malloc(i64 0)
  %57 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %16, align 8
  %58 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %57, align 8
  %59 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %58, i64 0, i32 0
  %60 = load i64 (i8*, i64)*, i64 (i8*, i64)** %59, align 8
  %61 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %58, i64 0, i32 1
  %62 = load i8*, i8** %61, align 8
  %63 = tail call i64 %60(i8* %62, i64 1)
  %64 = tail call {}* @print_int(i64 %63)
  ret i32 0
}
