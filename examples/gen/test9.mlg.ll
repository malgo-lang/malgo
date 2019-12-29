; ModuleID = './examples/test9.mlg'
source_filename = "./examples/test9.mlg"

declare i8* @GC_malloc(i64)

define i32 @main13() {
  %1 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  store i64 42, i64* %3
  %4 = call i8* @GC_malloc(i64 0)
  %5 = bitcast i8* %4 to {}*
  %6 = bitcast { i64 }* %2 to i8*
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { i64 (i8*)*, i8* }*
  %9 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %8, i32 0, i32 0
  store i64 (i8*)* @f14, i64 (i8*)** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %8, i32 0, i32 1
  store i8* %6, i8** %12
  %13 = call i8* @GC_malloc(i64 0)
  %14 = bitcast i8* %13 to {}*
  %15 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %8, i32 0, i32 0
  %16 = load i64 (i8*)*, i64 (i8*)** %15
  %17 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %8, i32 0, i32 1
  %18 = load i8*, i8** %17
  %19 = call i64 %16(i8* %18)
  %20 = add i64 %19, 42
  ret i32 0
}

define i64 @f14(i8*) {
  %2 = bitcast i8* %0 to { i64 }*
  %3 = getelementptr { i64 }, { i64 }* %2, i32 0, i32 0
  %4 = load i64, i64* %3
  %5 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %6 = bitcast i8* %5 to { i64 (i8*)*, i8* }*
  %7 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %6, i32 0, i32 0
  store i64 (i8*)* @f14, i64 (i8*)** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = getelementptr { i64 (i8*)*, i8* }, { i64 (i8*)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %10
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  ret i64 %4
}

define i32 @main() {
  %1 = call i32 @main13()
  ret i32 0
}
