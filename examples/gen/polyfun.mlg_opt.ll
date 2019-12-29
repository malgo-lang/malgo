; ModuleID = './examples/gen/polyfun.mlg.ll'
source_filename = "./examples/polyfun.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @f147(i8* nocapture readonly, i64) {
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

define i8* @fw128(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main56() local_unnamed_addr {
copyelem_0:
  %0 = tail call i8* @GC_malloc(i64 0)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id61, i8* (i8*, i8*)** %2, align 8
  %3 = tail call i8* @GC_malloc(i64 0)
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* %0, i8** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne60, i64 (i8*, i64)** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %7, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = tail call i8* @GC_malloc(i64 8)
  %15 = bitcast i8* %14 to { i64 (i8*, i64)*, i8* }**
  %16 = bitcast i8* %14 to i8**
  store i8* %8, i8** %16, align 8
  %17 = tail call i8* @GC_malloc(i64 0)
  %18 = tail call i8* @GC_malloc(i64 16)
  %19 = bitcast i8* %18 to { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda58", { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %20 = tail call i8* @GC_malloc(i64 0)
  %21 = getelementptr i8, i8* %18, i64 8
  %22 = bitcast i8* %21 to i8**
  store i8* %17, i8** %22, align 8
  %23 = tail call i8* @GC_malloc(i64 0)
  %24 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %2, align 8
  %25 = load i8*, i8** %5, align 8
  %26 = tail call i8* @GC_malloc(i64 16)
  %27 = bitcast i8* %26 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fw128, i8* (i8*, i8*)** %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = getelementptr i8, i8* %26, i64 8
  %30 = bitcast i8* %29 to i8**
  store i8* %1, i8** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = tail call i8* %24(i8* %25, i8* %26)
  %33 = load { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)*, { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)** %19, align 8
  %34 = load i8*, i8** %22, align 8
  %35 = tail call { { i8*, i8** }* (i8*, i8*)*, i8* }* %33(i8* %34, i8* inttoptr (i64 1 to i8*))
  %36 = tail call i8* @GC_malloc(i64 16)
  %37 = bitcast i8* %36 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f147, i64 (i8*, i64)** %37, align 8
  %38 = tail call i8* @GC_malloc(i64 0)
  %39 = getelementptr i8, i8* %36, i64 8
  %40 = bitcast i8* %39 to i8**
  store i8* %1, i8** %40, align 8
  %41 = tail call i8* @GC_malloc(i64 0)
  %42 = bitcast i8* %14 to i8**
  store i8* %36, i8** %42, align 8
  %43 = tail call i8* @GC_malloc(i64 0)
  %44 = load { i64 (i8*, i64)*, i8* }*, { i64 (i8*, i64)*, i8* }** %15, align 8
  %45 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %44, i64 0, i32 0
  %46 = load i64 (i8*, i64)*, i64 (i8*, i64)** %45, align 8
  %47 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %44, i64 0, i32 1
  %48 = load i8*, i8** %47, align 8
  %49 = tail call i64 %46(i8* %48, i64 1)
  %50 = tail call {}* @print_int(i64 %49)
  ret i32 0
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int57(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define { { i8*, i8** }* (i8*, i8*)*, i8* }* @"$lambda58"(i8*, i8*) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)**
  store { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)* @"$lambda58", { { i8*, i8** }* (i8*, i8*)*, i8* }* (i8*, i8*)** %4, align 8
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
  %13 = bitcast i8* %12 to { { i8*, i8** }* (i8*, i8*)*, i8* }*
  %14 = bitcast i8* %12 to { i8*, i8** }* (i8*, i8*)**
  store { i8*, i8** }* (i8*, i8*)* @"$lambda59", { i8*, i8** }* (i8*, i8*)** %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %12, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %9, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  ret { { i8*, i8** }* (i8*, i8*)*, i8* }* %13
}

define { i8*, i8** }* @"$lambda59"(i8*, i8*) {
copyelem_0:
  %2 = bitcast i8* %0 to i64*
  %3 = load i64, i64* %2, align 8
  %4 = tail call i8* @GC_malloc(i64 16)
  %5 = bitcast i8* %4 to { i8*, i8** }* (i8*, i8*)**
  store { i8*, i8** }* (i8*, i8*)* @"$lambda59", { i8*, i8** }* (i8*, i8*)** %5, align 8
  %6 = tail call i8* @GC_malloc(i64 0)
  %7 = getelementptr i8, i8* %4, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* %0, i8** %8, align 8
  %9 = tail call i8* @GC_malloc(i64 0)
  %10 = tail call i8* @GC_malloc(i64 8)
  %11 = bitcast i8* %10 to i8**
  store i8* %1, i8** %11, align 8
  %12 = tail call i8* @GC_malloc(i64 16)
  %13 = bitcast i8* %12 to { i8*, i8** }*
  %14 = bitcast i8* %12 to i64*
  store i64 %3, i64* %14, align 8
  %15 = tail call i8* @GC_malloc(i64 0)
  %16 = getelementptr i8, i8* %12, i64 8
  %17 = bitcast i8* %16 to i8**
  store i8* %10, i8** %17, align 8
  %18 = tail call i8* @GC_malloc(i64 0)
  ret { i8*, i8** }* %13
}

define i64 @addOne60(i8*, i64) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id61, i8* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne60, i64 (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  %15 = add i64 %1, 1
  ret i64 %15
}

define i8* @id61(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id61, i8* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  %9 = tail call i8* @GC_malloc(i64 16)
  %10 = bitcast i8* %9 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @addOne60, i64 (i8*, i64)** %10, align 8
  %11 = tail call i8* @GC_malloc(i64 0)
  %12 = getelementptr i8, i8* %9, i64 8
  %13 = bitcast i8* %12 to i8**
  store i8* %0, i8** %13, align 8
  %14 = tail call i8* @GC_malloc(i64 0)
  ret i8* %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @main56()
  ret i32 0
}
