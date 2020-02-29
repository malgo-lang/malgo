; ModuleID = './examples/gen/polyfun.mlg.ll'
source_filename = "./examples/polyfun.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @"$fo156"(i8* nocapture readonly, i64) {
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

define { i64, { i8**, i64 }* }* @"$fo140"(i8* nocapture readonly, i8*) {
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

define i8* @"$fo126"(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i64 @"$fo109"(i8* nocapture readonly, i64) {
  %3 = inttoptr i64 %1 to i8*
  %4 = bitcast i8* %0 to i64 (i8*, i8*)**
  %5 = load i64 (i8*, i8*)*, i64 (i8*, i8*)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i8* %3)
  ret i64 %9
}

define i64 @"$fo85"(i8* nocapture readonly, i8*) {
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

define { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* @"$lambda36"(i8*, i8*) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda36", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %4, align 8
  %5 = getelementptr i8, i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %0, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 8)
  %8 = bitcast i8* %7 to i8**
  store i8* %1, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }*
  %11 = bitcast i8* %9 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda35", { i8*, { i8**, i64 }* }* (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %7, i8** %13, align 8
  ret { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %10
}

define { i8*, { i8**, i64 }* }* @"$lambda35"(i8*, i8*) {
body_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i8*, { i8**, i64 }* }* (i8*, i8*)**
  store { i8*, { i8**, i64 }* }* (i8*, i8*)* @"$lambda35", { i8*, { i8**, i64 }* }* (i8*, i8*)** %5, align 8
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

declare void @GC_init() local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
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
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i64 (i8*, i8*)**
  store i64 (i8*, i8*)* @"$fo85", i64 (i8*, i8*)** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %6, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 8)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { i64 (i8*, i8*)*, i8* }***
  %17 = bitcast i8* %15 to i8**
  store i8* %14, i8** %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 1, i64* %19, align 8
  %20 = bitcast i8* %14 to i8**
  store i8* %10, i8** %20, align 8
  %21 = load { i64 (i8*, i8*)*, i8* }**, { i64 (i8*, i8*)*, i8* }*** %16, align 8
  %22 = load i64, i64* %19, align 8
  %23 = shl i64 %22, 3
  %24 = tail call i8* @GC_malloc(i64 %23)
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to { i64 (i8*, i64)*, i8* }***
  %27 = bitcast i8* %25 to i8**
  store i8* %24, i8** %27, align 8
  %28 = getelementptr i8, i8* %25, i64 8
  %29 = bitcast i8* %28 to i64*
  %.cast = bitcast i8* %24 to { i64 (i8*, i64)*, i8* }**
  store i64 %22, i64* %29, align 8
  %30 = icmp sgt i64 %22, 0
  br i1 %30, label %body_1, label %end_1

body_1:                                           ; preds = %body_0, %body_1
  %storemerge2 = phi i64 [ %40, %body_1 ], [ 0, %body_0 ]
  %31 = getelementptr { i64 (i8*, i8*)*, i8* }*, { i64 (i8*, i8*)*, i8* }** %21, i64 %storemerge2
  %32 = bitcast { i64 (i8*, i8*)*, i8* }** %31 to i64*
  %33 = load i64, i64* %32, align 8
  %34 = tail call i8* @GC_malloc(i64 16)
  %35 = bitcast i8* %34 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo109", i64 (i8*, i64)** %35, align 8
  %36 = getelementptr i8, i8* %34, i64 8
  %37 = bitcast i8* %36 to i64*
  store i64 %33, i64* %37, align 8
  %38 = getelementptr { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %.cast, i64 %storemerge2
  %39 = bitcast { i64 (i8*, i64)*, i8* }** %38 to i8**
  store i8* %34, i8** %39, align 8
  %40 = add nuw nsw i64 %storemerge2, 1
  %exitcond = icmp eq i64 %40, %22
  br i1 %exitcond, label %end_1, label %body_1

end_1:                                            ; preds = %body_1, %body_0
  %41 = tail call i8* @GC_malloc(i64 0)
  %42 = tail call i8* @GC_malloc(i64 16)
  %43 = bitcast i8* %42 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda36", { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %43, align 8
  %44 = getelementptr i8, i8* %42, i64 8
  %45 = bitcast i8* %44 to i8**
  store i8* %41, i8** %45, align 8
  %46 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %2, align 8
  %47 = load i8*, i8** %4, align 8
  %48 = tail call i8* %46(i8* %47, i8* %1)
  %49 = tail call i8* @GC_malloc(i64 16)
  %50 = bitcast i8* %49 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo126", i8* (i8*, i8*)** %50, align 8
  %51 = getelementptr i8, i8* %49, i64 8
  %52 = bitcast i8* %51 to i8**
  store i8* %48, i8** %52, align 8
  %53 = load { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* (i8*, i8*)** %43, align 8
  %54 = load i8*, i8** %45, align 8
  %55 = tail call { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %53(i8* %54, i8* inttoptr (i64 1 to i8*))
  %56 = tail call i8* @GC_malloc(i64 16)
  %57 = bitcast i8* %56 to { i64, { i8**, i64 }* }* (i8*, i8*)**
  store { i64, { i8**, i64 }* }* (i8*, i8*)* @"$fo140", { i64, { i8**, i64 }* }* (i8*, i8*)** %57, align 8
  %58 = getelementptr i8, i8* %56, i64 8
  %59 = bitcast i8* %58 to { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }**
  store { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }* %55, { { i8*, { i8**, i64 }* }* (i8*, i8*)*, i8* }** %59, align 8
  %60 = bitcast i8* %25 to i8***
  %61 = load i8**, i8*** %60, align 8
  %62 = tail call i8* @GC_malloc(i64 16)
  %63 = bitcast i8* %62 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo156", i64 (i8*, i64)** %63, align 8
  %64 = getelementptr i8, i8* %62, i64 8
  %65 = bitcast i8* %64 to i8**
  store i8* %1, i8** %65, align 8
  store i8* %62, i8** %61, align 8
  %66 = load { i64 (i8*, i64)*, i8* }**, { i64 (i8*, i64)*, i8* }*** %26, align 8
  %67 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %66, align 8
  %68 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %67, i64 0, i32 0
  %69 = load i64 (i8*, i64)*, i64 (i8*, i64)** %68, align 8
  %70 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %67, i64 0, i32 1
  %71 = load i8*, i8** %70, align 8
  %72 = tail call i64 %69(i8* %71, i64 1)
  %73 = tail call {}* @print_int(i64 %72)
  ret i32 0
}
