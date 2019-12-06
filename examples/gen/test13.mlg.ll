; ModuleID = './examples/test13.mlg'
source_filename = "./examples/test13.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.37() {
  %1 = getelementptr {}, {}* null, i64 1
  %2 = ptrtoint {}* %1 to i64
  %3 = call i8* @malloc_gc(i64 %2)
  %4 = bitcast i8* %3 to {}*
  %5 = bitcast {}* %4 to i8*
  %6 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %7 = ptrtoint { {} (i8*, i64)*, i8* }* %6 to i64
  %8 = call i8* @malloc_gc(i64 %7)
  %9 = bitcast i8* %8 to { {} (i8*, i64)*, i8* }*
  %10 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %9, i32 0, i32 0
  store {} (i8*, i64)* @f.21, {} (i8*, i64)** %10
  %11 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %9, i32 0, i32 1
  store i8* %5, i8** %11
  %12 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %9, i32 0, i32 0
  %13 = load {} (i8*, i64)*, {} (i8*, i64)** %12
  %14 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %9, i32 0, i32 1
  %15 = load i8*, i8** %14
  %16 = call {} %13(i8* %15, i64 5)
  ret {} %16
}

declare i1 @lt_i64(i64, i64)

define {} @f.21(i8*, i64) {
  %3 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* null, i64 1
  %4 = ptrtoint { {} (i8*, i64)*, i8* }* %3 to i64
  %5 = call i8* @malloc_gc(i64 %4)
  %6 = bitcast i8* %5 to { {} (i8*, i64)*, i8* }*
  %7 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 0
  store {} (i8*, i64)* @f.21, {} (i8*, i64)** %7
  %8 = getelementptr { {} (i8*, i64)*, i8* }, { {} (i8*, i64)*, i8* }* %6, i32 0, i32 1
  store i8* %0, i8** %8
  %9 = bitcast i8* %0 to {}*
  %10 = call i1 @lt_i64(i64 %1, i64 0)
  %11 = alloca {}
  br i1 %10, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %11
  br label %endif_0

else_0:                                           ; preds = %2
  %12 = getelementptr { { {} (i8*, i64)*, i8* }*, {} }, { { {} (i8*, i64)*, i8* }*, {} }* null, i64 1
  %13 = ptrtoint { { {} (i8*, i64)*, i8* }*, {} }* %12 to i64
  %14 = call i8* @malloc_gc(i64 %13)
  %15 = bitcast i8* %14 to { { {} (i8*, i64)*, i8* }*, {} }*
  %16 = getelementptr { { {} (i8*, i64)*, i8* }*, {} }, { { {} (i8*, i64)*, i8* }*, {} }* %15, i32 0, i32 0
  store { {} (i8*, i64)*, i8* }* %6, { {} (i8*, i64)*, i8* }** %16
  %17 = getelementptr { { {} (i8*, i64)*, i8* }*, {} }, { { {} (i8*, i64)*, i8* }*, {} }* %15, i32 0, i32 1
  store {} undef, {}* %17
  %18 = getelementptr { { {} (i8*, i64)*, i8* }*, {} }, { { {} (i8*, i64)*, i8* }*, {} }* %15, i32 0, i32 1
  %19 = load {}, {}* %18
  store {} %19, {}* %11
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %20 = load {}, {}* %11
  ret {} %20
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.37()
  ret i32 0
}
