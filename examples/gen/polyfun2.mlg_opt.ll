; ModuleID = './examples/gen/polyfun2.mlg.ll'
source_filename = "./examples/polyfun2.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo161"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  %10 = inttoptr i64 %9 to i8*
  ret i8* %10
}

define i8* @"$fo137"(i8* nocapture readonly, i8*) {
  %3 = ptrtoint i8* %1 to i64
  %4 = bitcast i8* %0 to i64 (i8*, i64)**
  %5 = load i64 (i8*, i64)*, i64 (i8*, i64)** %4, align 8
  %6 = getelementptr i8, i8* %0, i64 8
  %7 = bitcast i8* %6 to i8**
  %8 = load i8*, i8** %7, align 8
  %9 = tail call i64 %5(i8* %8, i64 %3)
  %10 = inttoptr i64 %9 to i8*
  ret i8* %10
}

define i64 @"$fo107"(i8* nocapture readonly, i64) {
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

define {}* @print_int10(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare i8* @GC_malloc(i64) local_unnamed_addr

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

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)**
  store i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)* @f0, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* %0, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = tail call i8* @GC_malloc(i64 16)
  %7 = bitcast i8* %6 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id1, i8* (i8*, i8*)** %7, align 8
  %8 = getelementptr i8, i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  store i8* %5, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = tail call i8* @GC_malloc(i64 16)
  %12 = bitcast i8* %11 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne2, i64 (i8*, i64)** %12, align 8
  %13 = getelementptr i8, i8* %11, i64 8
  %14 = bitcast i8* %13 to i8**
  store i8* %10, i8** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i8**
  store i8* %6, i8** %16, align 8
  %17 = getelementptr i8, i8* %15, i64 8
  %18 = bitcast i8* %17 to i64*
  store i64 1, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 8)
  %20 = tail call i8* @GC_malloc(i64 16)
  %21 = bitcast i8* %20 to { { i8* (i8*, i8*)*, i8* }*, i64 }***
  %22 = bitcast i8* %20 to i8**
  store i8* %19, i8** %22, align 8
  %23 = getelementptr i8, i8* %20, i64 8
  %24 = bitcast i8* %23 to i64*
  store i64 1, i64* %24, align 8
  %25 = bitcast i8* %19 to i8**
  store i8* %15, i8** %25, align 8
  %26 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %21, align 8
  %27 = load i64, i64* %24, align 8
  %28 = shl i64 %27, 3
  %29 = tail call i8* @GC_malloc(i64 %28)
  %30 = tail call i8* @GC_malloc(i64 16)
  %31 = bitcast i8* %30 to { { i64 (i8*, i64)*, i8* }*, i64 }***
  %32 = bitcast i8* %30 to i8**
  store i8* %29, i8** %32, align 8
  %33 = getelementptr i8, i8* %30, i64 8
  %34 = bitcast i8* %33 to i64*
  %.cast = bitcast i8* %29 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  store i64 %27, i64* %34, align 8
  %35 = icmp sgt i64 %27, 0
  br i1 %35, label %body_1, label %end_1

body_1:                                           ; preds = %body_0, %body_1
  %storemerge35 = phi i64 [ %52, %body_1 ], [ 0, %body_0 ]
  %36 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %26, i64 %storemerge35
  %37 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %36, align 8
  %38 = tail call i8* @GC_malloc(i64 16)
  %39 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %37 to i64*
  %40 = load i64, i64* %39, align 8
  %41 = tail call i8* @GC_malloc(i64 16)
  %42 = bitcast i8* %41 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo107", i64 (i8*, i64)** %42, align 8
  %43 = getelementptr i8, i8* %41, i64 8
  %44 = bitcast i8* %43 to i64*
  store i64 %40, i64* %44, align 8
  %45 = bitcast i8* %38 to i8**
  store i8* %41, i8** %45, align 8
  %46 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %37, i64 0, i32 1
  %47 = load i64, i64* %46, align 8
  %48 = getelementptr i8, i8* %38, i64 8
  %49 = bitcast i8* %48 to i64*
  store i64 %47, i64* %49, align 8
  %50 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %.cast, i64 %storemerge35
  %51 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }** %50 to i8**
  store i8* %38, i8** %51, align 8
  %52 = add nuw nsw i64 %storemerge35, 1
  %exitcond = icmp eq i64 %52, %27
  br i1 %exitcond, label %end_1, label %body_1

end_1:                                            ; preds = %body_1, %body_0
  %53 = tail call i8* @GC_malloc(i64 16)
  %54 = bitcast i8* %53 to i8**
  store i8* %11, i8** %54, align 8
  %55 = getelementptr i8, i8* %53, i64 8
  %56 = bitcast i8* %55 to i64*
  store i64 1, i64* %56, align 8
  %57 = tail call i8* @GC_malloc(i64 8)
  %58 = tail call i8* @GC_malloc(i64 16)
  %59 = bitcast i8* %58 to { { i64 (i8*, i64)*, i8* }*, i64 }***
  %60 = bitcast i8* %58 to i8**
  store i8* %57, i8** %60, align 8
  %61 = getelementptr i8, i8* %58, i64 8
  %62 = bitcast i8* %61 to i64*
  store i64 1, i64* %62, align 8
  %63 = bitcast i8* %57 to i8**
  store i8* %53, i8** %63, align 8
  %64 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %31, align 8
  %65 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %64, align 8
  %66 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %67 = load i8*, i8** %4, align 8
  %68 = tail call i8* @GC_malloc(i64 16)
  %69 = bitcast i8* %68 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %70 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }* %65 to i64*
  %71 = load i64, i64* %70, align 8
  %72 = tail call i8* @GC_malloc(i64 16)
  %73 = bitcast i8* %72 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo137", i8* (i8*, i8*)** %73, align 8
  %74 = getelementptr i8, i8* %72, i64 8
  %75 = bitcast i8* %74 to i64*
  store i64 %71, i64* %75, align 8
  %76 = bitcast i8* %68 to i8**
  store i8* %72, i8** %76, align 8
  %77 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %65, i64 0, i32 1
  %78 = load i64, i64* %77, align 8
  %79 = getelementptr i8, i8* %68, i64 8
  %80 = bitcast i8* %79 to i64*
  store i64 %78, i64* %80, align 8
  %81 = tail call i8* %66(i8* %67, { { i8* (i8*, i8*)*, i8* }*, i8* }* %69)
  %82 = ptrtoint i8* %81 to i64
  %83 = tail call {}* @print_int(i64 %82)
  %84 = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %59, align 8
  %85 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %84, align 8
  %86 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %87 = load i8*, i8** %4, align 8
  %88 = tail call i8* @GC_malloc(i64 16)
  %89 = bitcast i8* %88 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %90 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }* %85 to i64*
  %91 = load i64, i64* %90, align 8
  %92 = tail call i8* @GC_malloc(i64 16)
  %93 = bitcast i8* %92 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo161", i8* (i8*, i8*)** %93, align 8
  %94 = getelementptr i8, i8* %92, i64 8
  %95 = bitcast i8* %94 to i64*
  store i64 %91, i64* %95, align 8
  %96 = bitcast i8* %88 to i8**
  store i8* %92, i8** %96, align 8
  %97 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %85, i64 0, i32 1
  %98 = load i64, i64* %97, align 8
  %99 = getelementptr i8, i8* %88, i64 8
  %100 = bitcast i8* %99 to i64*
  store i64 %98, i64* %100, align 8
  %101 = tail call i8* %86(i8* %87, { { i8* (i8*, i8*)*, i8* }*, i8* }* %89)
  %102 = ptrtoint i8* %101 to i64
  %103 = tail call {}* @print_int(i64 %102)
  ret i32 0
}
