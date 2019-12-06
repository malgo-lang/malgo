; ModuleID = './examples/tuple.mlg'
source_filename = "./examples/tuple.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.71() {
  %1 = call i8* @malloc_gc(i64 15)
  %2 = getelementptr i8, i8* %1, i64 0
  store i8 32, i8* %2
  %3 = getelementptr i8, i8* %1, i64 1
  store i8 105, i8* %3
  %4 = getelementptr i8, i8* %1, i64 2
  store i8 115, i8* %4
  %5 = getelementptr i8, i8* %1, i64 3
  store i8 32, i8* %5
  %6 = getelementptr i8, i8* %1, i64 4
  store i8 116, i8* %6
  %7 = getelementptr i8, i8* %1, i64 5
  store i8 104, i8* %7
  %8 = getelementptr i8, i8* %1, i64 6
  store i8 101, i8* %8
  %9 = getelementptr i8, i8* %1, i64 7
  store i8 32, i8* %9
  %10 = getelementptr i8, i8* %1, i64 8
  store i8 97, i8* %10
  %11 = getelementptr i8, i8* %1, i64 9
  store i8 110, i8* %11
  %12 = getelementptr i8, i8* %1, i64 10
  store i8 115, i8* %12
  %13 = getelementptr i8, i8* %1, i64 11
  store i8 119, i8* %13
  %14 = getelementptr i8, i8* %1, i64 12
  store i8 101, i8* %14
  %15 = getelementptr i8, i8* %1, i64 13
  store i8 114, i8* %15
  %16 = getelementptr i8, i8* %1, i64 14
  store i8 0, i8* %16
  %17 = getelementptr { i64, i8* }, { i64, i8* }* null, i64 1
  %18 = ptrtoint { i64, i8* }* %17 to i64
  %19 = call i8* @malloc_gc(i64 %18)
  %20 = bitcast i8* %19 to { i64, i8* }*
  %21 = getelementptr { i64, i8* }, { i64, i8* }* %20, i32 0, i32 0
  store i64 42, i64* %21
  %22 = getelementptr { i64, i8* }, { i64, i8* }* %20, i32 0, i32 1
  store i8* %1, i8** %22
  %23 = call i64 @fst_int.60({ i64, i8* }* %20)
  %24 = call {} @print_int.51(i64 %23)
  %25 = call i8* @snd_str.64({ i64, i8* }* %20)
  %26 = call {} @println.54(i8* %25)
  ret {} %26
}

define i64 @fst_int.60({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 0
  %3 = load i64, i64* %2
  ret i64 %3
}

define i8* @snd_str.62({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 1
  %3 = load i8*, i8** %2
  ret i8* %3
}

define i8* @snd_str.64({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 1
  %3 = load i8*, i8** %2
  ret i8* %3
}

define i64 @fst_int.66({ i64, i8* }*) {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i32 0, i32 0
  %3 = load i64, i64* %2
  ret i64 %3
}

declare {} @newline({})

define {} @newline.57({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @println(i8*)

define {} @println.54(i8*) {
  %2 = call {} @println(i8* %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.51(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.71()
  ret i32 0
}
