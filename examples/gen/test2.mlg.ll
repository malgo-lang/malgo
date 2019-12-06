; ModuleID = './examples/test2.mlg'
source_filename = "./examples/test2.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.23() {
  %1 = getelementptr {}, {}* null, i64 1
  %2 = ptrtoint {}* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to {}*
  %5 = bitcast {}* %4 to i8*
  %6 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* null, i64 1
  %7 = ptrtoint { {} (i8*, {})*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { {} (i8*, {})*, i8* }*
  %10 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %9, i32 0, i32 0
  store {} (i8*, {})* @f.11, {} (i8*, {})** %10
  %11 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %9, i32 0, i32 1
  store i8* %5, i8** %11
  %12 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %9, i32 0, i32 0
  %13 = load {} (i8*, {})*, {} (i8*, {})** %12
  %14 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %9, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call {} %13(i8* %15, {} undef)
  ret {} %16
}

define {} @f.11(i8*, {}) {
  %3 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, {})*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, {})*, i8* }*
  %7 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %6, i32 0, i32 0
  store {} (i8*, {})* @f.11, {} (i8*, {})** %7
  %8 = getelementptr { {} (i8*, {})*, i8* }, { {} (i8*, {})*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  ret {} undef
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.23()
  ret i32 0
}
