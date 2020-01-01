; ModuleID = './examples/test16.mlg'
source_filename = "./examples/test16.mlg"

define i64 @fo84(i8*, i64) {
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

define i8* @fo71(i8*, i8*) {
  %3 = bitcast i8* %0 to { i8* (i8*, i8*)*, i8* }*
  %4 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 0
  %5 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %4
  %6 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %3, i32 0, i32 1
  %7 = load i8*, i8** %6
  %8 = call i8* %5(i8* %7, i8* %1)
  ret i8* %8
}

define i8* @fo59(i8*, i8*) {
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
  %29 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 0
  store i8* %28, i8** %29
  %30 = call i8* @GC_malloc(i64 0)
  %31 = bitcast i8* %30 to {}*
  %32 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %13, i32 0, i32 0
  %33 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %32
  %34 = bitcast { i8* (i8*, i8*)*, i8* }* %33 to i8*
  %35 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 1
  store i8* %34, i8** %35
  %36 = call i8* @GC_malloc(i64 0)
  %37 = bitcast i8* %36 to {}*
  %38 = bitcast { i8*, i8* }* %25 to i8*
  %39 = call i8* %21(i8* %23, i8* %38)
  %40 = bitcast i8* %39 to { i8*, i8* }*
  %41 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %42 = bitcast i8* %41 to { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }*
  %43 = getelementptr { i8*, i8* }, { i8*, i8* }* %40, i32 0, i32 0
  %44 = load i8*, i8** %43
  %45 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %46 = bitcast i8* %45 to { i8* (i8*, i8*)*, i8* }*
  %47 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %46, i32 0, i32 0
  store i8* (i8*, i8*)* @fo59, i8* (i8*, i8*)** %47
  %48 = call i8* @GC_malloc(i64 0)
  %49 = bitcast i8* %48 to {}*
  %50 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %46, i32 0, i32 1
  store i8* %44, i8** %50
  %51 = call i8* @GC_malloc(i64 0)
  %52 = bitcast i8* %51 to {}*
  %53 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %42, i32 0, i32 0
  store { i8* (i8*, i8*)*, i8* }* %46, { i8* (i8*, i8*)*, i8* }** %53
  %54 = call i8* @GC_malloc(i64 0)
  %55 = bitcast i8* %54 to {}*
  %56 = getelementptr { i8*, i8* }, { i8*, i8* }* %40, i32 0, i32 0
  %57 = load i8*, i8** %56
  %58 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %59 = bitcast i8* %58 to { i8* (i8*, i8*)*, i8* }*
  %60 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %59, i32 0, i32 0
  store i8* (i8*, i8*)* @fo71, i8* (i8*, i8*)** %60
  %61 = call i8* @GC_malloc(i64 0)
  %62 = bitcast i8* %61 to {}*
  %63 = getelementptr { i8* (i8*, i8*)*, i8* }, { i8* (i8*, i8*)*, i8* }* %59, i32 0, i32 1
  store i8* %57, i8** %63
  %64 = call i8* @GC_malloc(i64 0)
  %65 = bitcast i8* %64 to {}*
  %66 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %42, i32 0, i32 1
  store { i8* (i8*, i8*)*, i8* }* %59, { i8* (i8*, i8*)*, i8* }** %66
  %67 = call i8* @GC_malloc(i64 0)
  %68 = bitcast i8* %67 to {}*
  %69 = getelementptr { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }, { { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }* }* %42, i32 0, i32 0
  %70 = load { i8* (i8*, i8*)*, i8* }*, { i8* (i8*, i8*)*, i8* }** %69
  %71 = bitcast { i8* (i8*, i8*)*, i8* }* %70 to i8*
  %72 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %73 = bitcast i8* %72 to { i64 (i8*, i64)*, i8* }*
  %74 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %73, i32 0, i32 0
  store i64 (i8*, i64)* @fo84, i64 (i8*, i64)** %74
  %75 = call i8* @GC_malloc(i64 0)
  %76 = bitcast i8* %75 to {}*
  %77 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %73, i32 0, i32 1
  store i8* %71, i8** %77
  %78 = call i8* @GC_malloc(i64 0)
  %79 = bitcast i8* %78 to {}*
  %80 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %73, i32 0, i32 0
  %81 = load i64 (i8*, i64)*, i64 (i8*, i64)** %80
  %82 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %73, i32 0, i32 1
  %83 = load i8*, i8** %82
  %84 = call i64 %81(i8* %83, i64 1)
  %85 = call {}* @print_int27(i64 %84)
  ret i32 0
}
