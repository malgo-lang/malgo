; ModuleID = './examples/test3.mlg'
source_filename = "./examples/test3.mlg"

declare i8* @GC_malloc(i64)

define {}* @f12(i8*, {}*) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, {}*)*, i8* }*
  %6 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, {}*)* @f12, {}* (i8*, {}*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  ret {}* %1
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { {}* (i8*, {}*)*, i8* }*
  %6 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  store {}* (i8*, {}*)* @f12, {}* (i8*, {}*)** %6
  %7 = call i8* @GC_malloc(i64 0)
  %8 = bitcast i8* %7 to {}*
  %9 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %9
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  %12 = call i8* @GC_malloc(i64 0)
  %13 = bitcast i8* %12 to {}*
  %14 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  %15 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %14
  %16 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  %17 = load i8*, i8** %16
  %18 = call {}* %15(i8* %17, {}* %13)
  %19 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 0
  %20 = load {}* (i8*, {}*)*, {}* (i8*, {}*)** %19
  %21 = getelementptr { {}* (i8*, {}*)*, i8* }, { {}* (i8*, {}*)*, i8* }* %5, i32 0, i32 1
  %22 = load i8*, i8** %21
  %23 = call {}* %20(i8* %22, {}* %18)
  ret i32 0
}
