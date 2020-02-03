; ModuleID = './examples/gen/polyfun3.mlg.ll'
source_filename = "./examples/polyfun3.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i8* @"$fo111"(i8* nocapture readonly, i8*) {
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

define i64 @"$fo85"(i8* nocapture readonly, i64) {
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

define {}* @print_int7(i64) local_unnamed_addr {
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
  %11 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 0
  %12 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %11, align 8
  %13 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i8* }, { { i8* (i8*, i8*)*, i8* }*, i8* }* %1, i64 0, i32 1
  %14 = load i8*, i8** %13, align 8
  %15 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i64 0, i32 0
  %16 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %15, align 8
  %17 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %12, i64 0, i32 1
  %18 = load i8*, i8** %17, align 8
  %19 = tail call i8* %16(i8* %18, i8* %14)
  ret i8* %19
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
  %10 = tail call i8* @GC_malloc(i64 16)
  %11 = bitcast i8* %10 to i8**
  store i8* %6, i8** %11, align 8
  %12 = getelementptr i8, i8* %10, i64 8
  %13 = bitcast i8* %12 to i64*
  store i64 1, i64* %13, align 8
  %14 = tail call i8* @GC_malloc(i64 8)
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to { { i8* (i8*, i8*)*, i8* }*, i64 }***
  %17 = bitcast i8* %15 to i8**
  store i8* %14, i8** %17, align 8
  %18 = getelementptr i8, i8* %15, i64 8
  %19 = bitcast i8* %18 to i64*
  store i64 1, i64* %19, align 8
  %20 = bitcast i8* %14 to i8**
  store i8* %10, i8** %20, align 8
  %21 = load { { i8* (i8*, i8*)*, i8* }*, i64 }**, { { i8* (i8*, i8*)*, i8* }*, i64 }*** %16, align 8
  %22 = load i64, i64* %19, align 8
  %23 = shl i64 %22, 3
  %24 = tail call i8* @GC_malloc(i64 %23)
  %25 = tail call i8* @GC_malloc(i64 16)
  %26 = bitcast i8* %25 to { { i64 (i8*, i64)*, i8* }*, i64 }***
  %27 = bitcast i8* %25 to i8**
  store i8* %24, i8** %27, align 8
  %28 = getelementptr i8, i8* %25, i64 8
  %29 = bitcast i8* %28 to i64*
  %.cast = bitcast i8* %24 to { { i64 (i8*, i64)*, i8* }*, i64 }**
  store i64 %22, i64* %29, align 8
  %30 = icmp sgt i64 %22, 0
  br i1 %30, label %body_1, label %end_1

body_1:                                           ; preds = %body_0, %body_1
  %storemerge2 = phi i64 [ %47, %body_1 ], [ 0, %body_0 ]
  %31 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %21, i64 %storemerge2
  %32 = load { { i8* (i8*, i8*)*, i8* }*, i64 }*, { { i8* (i8*, i8*)*, i8* }*, i64 }** %31, align 8
  %33 = tail call i8* @GC_malloc(i64 16)
  %34 = bitcast { { i8* (i8*, i8*)*, i8* }*, i64 }* %32 to i64*
  %35 = load i64, i64* %34, align 8
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @"$fo85", i64 (i8*, i64)** %37, align 8
  %38 = getelementptr i8, i8* %36, i64 8
  %39 = bitcast i8* %38 to i64*
  store i64 %35, i64* %39, align 8
  %40 = bitcast i8* %33 to i8**
  store i8* %36, i8** %40, align 8
  %41 = getelementptr { { i8* (i8*, i8*)*, i8* }*, i64 }, { { i8* (i8*, i8*)*, i8* }*, i64 }* %32, i64 0, i32 1
  %42 = load i64, i64* %41, align 8
  %43 = getelementptr i8, i8* %33, i64 8
  %44 = bitcast i8* %43 to i64*
  store i64 %42, i64* %44, align 8
  %45 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %.cast, i64 %storemerge2
  %46 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }** %45 to i8**
  store i8* %33, i8** %46, align 8
  %47 = add nuw nsw i64 %storemerge2, 1
  %exitcond = icmp eq i64 %47, %22
  br i1 %exitcond, label %end_1.loopexit, label %body_1

end_1.loopexit:                                   ; preds = %body_1
  %.pre = load { { i64 (i8*, i64)*, i8* }*, i64 }**, { { i64 (i8*, i64)*, i8* }*, i64 }*** %26, align 8
  br label %end_1

end_1:                                            ; preds = %end_1.loopexit, %body_0
  %48 = phi { { i64 (i8*, i64)*, i8* }*, i64 }** [ %.pre, %end_1.loopexit ], [ %.cast, %body_0 ]
  %49 = load { { i64 (i8*, i64)*, i8* }*, i64 }*, { { i64 (i8*, i64)*, i8* }*, i64 }** %48, align 8
  %50 = load i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)*, i8* (i8*, { { i8* (i8*, i8*)*, i8* }*, i8* }*)** %2, align 8
  %51 = load i8*, i8** %4, align 8
  %52 = tail call i8* @GC_malloc(i64 16)
  %53 = bitcast i8* %52 to { { i8* (i8*, i8*)*, i8* }*, i8* }*
  %54 = bitcast { { i64 (i8*, i64)*, i8* }*, i64 }* %49 to i64*
  %55 = load i64, i64* %54, align 8
  %56 = tail call i8* @GC_malloc(i64 16)
  %57 = bitcast i8* %56 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @"$fo111", i8* (i8*, i8*)** %57, align 8
  %58 = getelementptr i8, i8* %56, i64 8
  %59 = bitcast i8* %58 to i64*
  store i64 %55, i64* %59, align 8
  %60 = bitcast i8* %52 to i8**
  store i8* %56, i8** %60, align 8
  %61 = getelementptr { { i64 (i8*, i64)*, i8* }*, i64 }, { { i64 (i8*, i64)*, i8* }*, i64 }* %49, i64 0, i32 1
  %62 = load i64, i64* %61, align 8
  %63 = getelementptr i8, i8* %52, i64 8
  %64 = bitcast i8* %63 to i64*
  store i64 %62, i64* %64, align 8
  %65 = tail call i8* %50(i8* %51, { { i8* (i8*, i8*)*, i8* }*, i8* }* %53)
  %66 = ptrtoint i8* %65 to i64
  %67 = tail call {}* @print_int(i64 %66)
  ret i32 0
}
