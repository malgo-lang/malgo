; ModuleID = './examples/test16.mlg'
source_filename = "./examples/test16.mlg"

define i64 @f106(i8*, i64) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = inttoptr i64 %1 to i8*
  %5 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %6 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %5
  %7 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %8 = load i8*, i8** %7
  %9 = call i8* %6(i8* %8, i8* %4)
  %10 = ptrtoint i8* %9 to i64
  ret i64 %10
}

define i8* @fo93(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fo81(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fw64(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fw50(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

declare i8* @GC_malloc(i64)

define i8* @id28(i8*, i8*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id28, i8* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  ret i8* %1
}

declare {}* @print_int(i64)

define {}* @print_int27(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8* (i8*, i8*)*, i8* }*
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  store i8* (i8*, i8*)* @id28, i8* (i8*, i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }*
  %14 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %13, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %5, { i8* (i8*, i8*)*, i8* }** %14
  %15 = call i8* @GC_malloc(i64 0)
  %16 = bitcast i8* %15 to {}*
  %17 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %13, i32 0, i32 1
  store { i8* (i8*, i8*)*, i8* }* %5, { i8* (i8*, i8*)*, i8* }** %17
  %18 = call i8* @GC_malloc(i64 0)
  %19 = bitcast i8* %18 to {}*
  %20 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 0
  %21 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %20
  %22 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %5, i32 0, i32 1
  %23 = load i8*, i8** %22
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { i8*, i8* }*
  %26 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %13, i32 0, i32 0
  %27 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %26
  %28 = bitcast { i8* (i8*, i8*)*, i8* }* %27 to i8*
  %29 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %30 = bitcast i8* %29 to { i8* (i8*, i8*)*, i8* }*
  %31 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %30, i32 0, i32 0
  store i8* (i8*, i8*)* @fw50, i8* (i8*, i8*)** %31
  %32 = call i8* @GC_malloc(i64 0)
  %33 = bitcast i8* %32 to {}*
  %34 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %30, i32 0, i32 1
  store i8* %28, i8** %34
  %35 = call i8* @GC_malloc(i64 0)
  %36 = bitcast i8* %35 to {}*
  %37 = bitcast { i8* (i8*, i8*)*, i8* }* %30 to i8*
  %38 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 0
  store i8* %37, i8** %38
  %39 = call i8* @GC_malloc(i64 0)
  %40 = bitcast i8* %39 to {}*
  %41 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %13, i32 0, i32 1
  %42 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %41
  %43 = bitcast { i8* (i8*, i8*)*, i8* }* %42 to i8*
  %44 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %45 = bitcast i8* %44 to { i8* (i8*, i8*)*, i8* }*
  %46 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %45, i32 0, i32 0
  store i8* (i8*, i8*)* @fw64, i8* (i8*, i8*)** %46
  %47 = call i8* @GC_malloc(i64 0)
  %48 = bitcast i8* %47 to {}*
  %49 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %45, i32 0, i32 1
  store i8* %43, i8** %49
  %50 = call i8* @GC_malloc(i64 0)
  %51 = bitcast i8* %50 to {}*
  %52 = bitcast { i8* (i8*, i8*)*, i8* }* %45 to i8*
  %53 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 1
  store i8* %52, i8** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = bitcast { i8*, i8* }* %25 to i8*
  %57 = call i8* %21(i8* %23, i8* %56)
  %58 = bitcast i8* %57 to { i8*, i8* }*
  %59 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %60 = bitcast i8* %59 to { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }*
  %61 = getelementptr { i8*, i8* }, { i8*, i8* }* %58, i32 0, i32 0
  %62 = load i8*, i8** %61
  %63 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %64 = bitcast i8* %63 to { i8* (i8*, i8*)*, i8* }*
  %65 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %64, i32 0, i32 0
  store i8* (i8*, i8*)* @fo81, i8* (i8*, i8*)** %65
  %66 = call i8* @GC_malloc(i64 0)
  %67 = bitcast i8* %66 to {}*
  %68 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %64, i32 0, i32 1
  store i8* %62, i8** %68
  %69 = call i8* @GC_malloc(i64 0)
  %70 = bitcast i8* %69 to {}*
  %71 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %60, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %64, { i8* (i8*, i8*)*, i8* }** %71
  %72 = call i8* @GC_malloc(i64 0)
  %73 = bitcast i8* %72 to {}*
  %74 = getelementptr { i8*, i8* }, { i8*, i8* }* %58, i32 0, i32 1
  %75 = load i8*, i8** %74
  %76 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %77 = bitcast i8* %76 to { i8* (i8*, i8*)*, i8* }*
  %78 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %77, i32 0, i32 0
  store i8* (i8*, i8*)* @fo93, i8* (i8*, i8*)** %78
  %79 = call i8* @GC_malloc(i64 0)
  %80 = bitcast i8* %79 to {}*
  %81 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %77, i32 0, i32 1
  store i8* %75, i8** %81
  %82 = call i8* @GC_malloc(i64 0)
  %83 = bitcast i8* %82 to {}*
  %84 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %60, i32 0, i32 1
  store { i8* (i8*, i8*)*, i8* }* %77, { i8* (i8*, i8*)*, i8* }** %84
  %85 = call i8* @GC_malloc(i64 0)
  %86 = bitcast i8* %85 to {}*
  %87 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %60, i32 0, i32 0
  %88 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %87
  %89 = bitcast { i8* (i8*, i8*)*, i8* }* %88 to i8*
  %90 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %91 = bitcast i8* %90 to { i64 (i8*, i64)*, i8* }*
  %92 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %91, i32 0, i32 0
  store i64 (i8*, i64)* @f106, i64 (i8*, i64)** %92
  %93 = call i8* @GC_malloc(i64 0)
  %94 = bitcast i8* %93 to {}*
  %95 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %91, i32 0, i32 1
  store i8* %89, i8** %95
  %96 = call i8* @GC_malloc(i64 0)
  %97 = bitcast i8* %96 to {}*
  %98 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %91, i32 0, i32 0
  %99 = load i64 (i8*, i64)*, i64 (i8*, i64)** %98
  %100 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %91, i32 0, i32 1
  %101 = load i8*, i8** %100
  %102 = call i64 %99(i8* %101, i64 1)
  %103 = call {}* @print_int27(i64 %102)
  ret i32 0
}
