; ModuleID = './examples/gen/test16.mlg.ll'
source_filename = "./examples/test16.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

define i64 @f106(i8* nocapture readonly, i64) {
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

define i8* @fo93(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fo81(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fw64(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fw50(i8* nocapture readonly, i8*) {
  %3 = bitcast i8* %0 to i8* (i8*, i8*)**
  %4 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %5 = getelementptr i8, i8* %0, i64 8
  %6 = bitcast i8* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  %8 = tail call i8* %4(i8* %7, i8* %1)
  ret i8* %8
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i8* @id28(i8*, i8* readnone returned) {
  %3 = tail call i8* @GC_malloc(i64 16)
  %4 = bitcast i8* %3 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id28, i8* (i8*, i8*)** %4, align 8
  %5 = tail call i8* @GC_malloc(i64 0)
  %6 = getelementptr i8, i8* %3, i64 8
  %7 = bitcast i8* %6 to i8**
  store i8* %0, i8** %7, align 8
  %8 = tail call i8* @GC_malloc(i64 0)
  ret i8* %1
}

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int27(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 0)
  %2 = tail call i8* @GC_malloc(i64 16)
  %3 = bitcast i8* %2 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @id28, i8* (i8*, i8*)** %3, align 8
  %4 = tail call i8* @GC_malloc(i64 0)
  %5 = getelementptr i8, i8* %2, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %1, i8** %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = tail call i8* @GC_malloc(i64 16)
  %9 = bitcast i8* %8 to i8**
  store i8* %2, i8** %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = getelementptr i8, i8* %8, i64 8
  %12 = bitcast i8* %11 to i8**
  store i8* %2, i8** %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %3, align 8
  %15 = load i8*, i8** %6, align 8
  %16 = tail call i8* @GC_malloc(i64 16)
  %17 = bitcast i8* %8 to i64*
  %18 = load i64, i64* %17, align 8
  %19 = tail call i8* @GC_malloc(i64 16)
  %20 = bitcast i8* %19 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fw50, i8* (i8*, i8*)** %20, align 8
  %21 = tail call i8* @GC_malloc(i64 0)
  %22 = getelementptr i8, i8* %19, i64 8
  %23 = bitcast i8* %22 to i64*
  store i64 %18, i64* %23, align 8
  %24 = tail call i8* @GC_malloc(i64 0)
  %25 = bitcast i8* %16 to i8**
  store i8* %19, i8** %25, align 8
  %26 = tail call i8* @GC_malloc(i64 0)
  %27 = bitcast i8* %11 to i64*
  %28 = load i64, i64* %27, align 8
  %29 = tail call i8* @GC_malloc(i64 16)
  %30 = bitcast i8* %29 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fw64, i8* (i8*, i8*)** %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = getelementptr i8, i8* %29, i64 8
  %33 = bitcast i8* %32 to i64*
  store i64 %28, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = getelementptr i8, i8* %16, i64 8
  %36 = bitcast i8* %35 to i8**
  store i8* %29, i8** %36, align 8
  %37 = tail call i8* @GC_malloc(i64 0)
  %38 = tail call i8* %14(i8* %15, i8* %16)
  %39 = tail call i8* @GC_malloc(i64 16)
  %40 = bitcast i8* %38 to i64*
  %41 = load i64, i64* %40, align 8
  %42 = tail call i8* @GC_malloc(i64 16)
  %43 = bitcast i8* %42 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fo81, i8* (i8*, i8*)** %43, align 8
  %44 = tail call i8* @GC_malloc(i64 0)
  %45 = getelementptr i8, i8* %42, i64 8
  %46 = bitcast i8* %45 to i64*
  store i64 %41, i64* %46, align 8
  %47 = tail call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %39 to i8**
  store i8* %42, i8** %48, align 8
  %49 = tail call i8* @GC_malloc(i64 0)
  %50 = getelementptr i8, i8* %38, i64 8
  %51 = bitcast i8* %50 to i64*
  %52 = load i64, i64* %51, align 8
  %53 = tail call i8* @GC_malloc(i64 16)
  %54 = bitcast i8* %53 to i8* (i8*, i8*)**
  store i8* (i8*, i8*)* @fo93, i8* (i8*, i8*)** %54, align 8
  %55 = tail call i8* @GC_malloc(i64 0)
  %56 = getelementptr i8, i8* %53, i64 8
  %57 = bitcast i8* %56 to i64*
  store i64 %52, i64* %57, align 8
  %58 = tail call i8* @GC_malloc(i64 0)
  %59 = getelementptr i8, i8* %39, i64 8
  %60 = bitcast i8* %59 to i8**
  store i8* %53, i8** %60, align 8
  %61 = tail call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %39 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = tail call i8* @GC_malloc(i64 16)
  %65 = bitcast i8* %64 to i64 (i8*, i64)**
  store i64 (i8*, i64)* @f106, i64 (i8*, i64)** %65, align 8
  %66 = tail call i8* @GC_malloc(i64 0)
  %67 = getelementptr i8, i8* %64, i64 8
  %68 = bitcast i8* %67 to i8**
  %69 = bitcast i8* %67 to i64*
  store i64 %63, i64* %69, align 8
  %70 = tail call i8* @GC_malloc(i64 0)
  %71 = load i64 (i8*, i64)*, i64 (i8*, i64)** %65, align 8
  %72 = load i8*, i8** %68, align 8
  %73 = tail call i64 %71(i8* %72, i64 1)
  %74 = tail call {}* @print_int(i64 %73)
  ret i32 0
}
