; ModuleID = './examples/test2.mlg'
source_filename = "./examples/test2.mlg"

declare i8* @GC_malloc(i64)

define {}* @f7(i8*) {
  %2 = bitcast i8* %0 to {}*
  %3 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %4 = bitcast i8* %3 to { {}* (i8*)*, i8* }*
  %5 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %4, i32 0, i32 0
  store {}* (i8*)* @f7, {}* (i8*)** %5
  %6 = call i8* @GC_malloc(i64 0)
  %7 = bitcast i8* %6 to {}*
  %8 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %4, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = call i8* @GC_malloc(i64 0)
  %10 = bitcast i8* %9 to {}*
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  ret {}* %12
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*)*, i8* }*
  %6 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*)* @f7, {}* (i8*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %5, i32 0, i32 0
  %13 = load {}* (i8*)*, {}* (i8*)** %12
  %14 = getelementptr { {}* (i8*)*, i8* }, { {}* (i8*)*, i8* }* %5, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call {}* %13(i8* %15)
  ret i32 0
}
