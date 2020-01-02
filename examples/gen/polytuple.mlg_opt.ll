; ModuleID = './examples/gen/polytuple.mlg.ll'
source_filename = "./examples/polytuple.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare i8* @GC_malloc(i64) local_unnamed_addr

define {}* @f21({ i8**, i64 }* nocapture readonly, i8*) local_unnamed_addr {
  %3 = getelementptr { i8**, i64 }, { i8**, i64 }* %0, i64 0, i32 0
  %4 = load i8**, i8*** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  ret {}* %6
}

define i32 @main() local_unnamed_addr {
body_0:
  %0 = tail call i8* @GC_malloc(i64 80)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 10, i64* %5, align 8
  %6 = bitcast i8* %0 to i64*
  store i64 1, i64* %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load i64*, i64** %2, align 8
  %9 = getelementptr i64, i64* %8, i64 1
  store i64 1, i64* %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = load i64*, i64** %2, align 8
  %12 = getelementptr i64, i64* %11, i64 2
  store i64 1, i64* %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = load i64*, i64** %2, align 8
  %15 = getelementptr i64, i64* %14, i64 3
  store i64 1, i64* %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = load i64*, i64** %2, align 8
  %18 = getelementptr i64, i64* %17, i64 4
  store i64 1, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = load i64*, i64** %2, align 8
  %21 = getelementptr i64, i64* %20, i64 5
  store i64 1, i64* %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = load i64*, i64** %2, align 8
  %24 = getelementptr i64, i64* %23, i64 6
  store i64 1, i64* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = load i64*, i64** %2, align 8
  %27 = getelementptr i64, i64* %26, i64 7
  store i64 1, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = load i64*, i64** %2, align 8
  %30 = getelementptr i64, i64* %29, i64 8
  store i64 1, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = load i64*, i64** %2, align 8
  %33 = getelementptr i64, i64* %32, i64 9
  store i64 1, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = tail call i8* @GC_malloc(i64 0)
  %36 = load i64*, i64** %2, align 8
  %37 = load i64, i64* %5, align 8
  %38 = shl i64 %37, 3
  %39 = tail call i8* @GC_malloc(i64 %38)
  %40 = tail call i8* @GC_malloc(i64 16)
  %41 = bitcast i8* %40 to i8**
  store i8* %39, i8** %41, align 8
  %42 = getelementptr i8, i8* %40, i64 8
  %43 = bitcast i8* %42 to i64*
  %.cast = bitcast i8* %39 to i8**
  store i64 %37, i64* %43, align 8
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = icmp sgt i64 %37, 0
  br i1 %45, label %body_1, label %end_1

body_1:                                           ; preds = %body_0, %body_1
  %storemerge2 = phi i64 [ %51, %body_1 ], [ 0, %body_0 ]
  %46 = getelementptr i64, i64* %36, i64 %storemerge2
  %47 = load i64, i64* %46, align 8
  %48 = getelementptr i8*, i8** %.cast, i64 %storemerge2
  %49 = bitcast i8** %48 to i64*
  store i64 %47, i64* %49, align 8
  %50 = tail call i8* @GC_malloc(i64 0)
  %51 = add nuw nsw i64 %storemerge2, 1
  %exitcond = icmp eq i64 %51, %37
  br i1 %exitcond, label %end_1, label %body_1

end_1:                                            ; preds = %body_1, %body_0
  %52 = tail call i8* @GC_malloc(i64 0)
  %53 = bitcast i8* %40 to i8***
  %54 = load i8**, i8*** %53, align 8
  store i8* inttoptr (i64 10 to i8*), i8** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = tail call i8* @GC_malloc(i64 0)
  ret i32 0
}
