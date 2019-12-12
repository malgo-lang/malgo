; ModuleID = './examples/test2.mlg'
source_filename = "./examples/test2.mlg"

declare i8* @GC_malloc(i64)

define {}* @f.6(i8*, {}*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, {}*)*, i8* }*
  %6 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, {}*)* @f.6, {}* (i8*, {}*)** %6
  %7 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  ret {}* %9
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, {}*)*, i8* }*
  %6 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, {}*)* @f.6, {}* (i8*, {}*)** %6
  %7 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  %11 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %10
  %12 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  %13 = load i8*, i8** %12
  %14 = call {}* %11(i8* %13, {}* %9)
  ret i32 0
}
