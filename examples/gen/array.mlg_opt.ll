; ModuleID = './examples/gen/array.mlg.ll'
source_filename = "./examples/array.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int28(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline27() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare i8* @GC_malloc(i64) local_unnamed_addr

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
  store i64 0, i64* %6, align 8
  %7 = tail call i8* @GC_malloc(i64 0)
  %8 = load i64*, i64** %2, align 8
  %9 = getelementptr i64, i64* %8, i64 1
  store i64 0, i64* %9, align 8
  %10 = tail call i8* @GC_malloc(i64 0)
  %11 = load i64*, i64** %2, align 8
  %12 = getelementptr i64, i64* %11, i64 2
  store i64 0, i64* %12, align 8
  %13 = tail call i8* @GC_malloc(i64 0)
  %14 = load i64*, i64** %2, align 8
  %15 = getelementptr i64, i64* %14, i64 3
  store i64 0, i64* %15, align 8
  %16 = tail call i8* @GC_malloc(i64 0)
  %17 = load i64*, i64** %2, align 8
  %18 = getelementptr i64, i64* %17, i64 4
  store i64 0, i64* %18, align 8
  %19 = tail call i8* @GC_malloc(i64 0)
  %20 = load i64*, i64** %2, align 8
  %21 = getelementptr i64, i64* %20, i64 5
  store i64 0, i64* %21, align 8
  %22 = tail call i8* @GC_malloc(i64 0)
  %23 = load i64*, i64** %2, align 8
  %24 = getelementptr i64, i64* %23, i64 6
  store i64 0, i64* %24, align 8
  %25 = tail call i8* @GC_malloc(i64 0)
  %26 = load i64*, i64** %2, align 8
  %27 = getelementptr i64, i64* %26, i64 7
  store i64 0, i64* %27, align 8
  %28 = tail call i8* @GC_malloc(i64 0)
  %29 = load i64*, i64** %2, align 8
  %30 = getelementptr i64, i64* %29, i64 8
  store i64 0, i64* %30, align 8
  %31 = tail call i8* @GC_malloc(i64 0)
  %32 = load i64*, i64** %2, align 8
  %33 = getelementptr i64, i64* %32, i64 9
  store i64 0, i64* %33, align 8
  %34 = tail call i8* @GC_malloc(i64 0)
  %35 = tail call i8* @GC_malloc(i64 0)
  %36 = load i64*, i64** %2, align 8
  %37 = getelementptr i64, i64* %36, i64 1
  %38 = load i64, i64* %37, align 8
  %39 = getelementptr i64, i64* %36, i64 2
  store i64 42, i64* %39, align 8
  %40 = tail call i8* @GC_malloc(i64 0)
  %41 = load i64*, i64** %2, align 8
  %42 = getelementptr i64, i64* %41, i64 2
  %43 = load i64, i64* %42, align 8
  %44 = tail call {}* @print_int(i64 %38)
  %45 = tail call {}* @newline()
  %46 = tail call {}* @print_int(i64 %43)
  %47 = tail call {}* @newline()
  ret i32 0
}
