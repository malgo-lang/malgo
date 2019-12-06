; ModuleID = './examples/hello.mlg'
source_filename = "./examples/hello.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.13() {
  %1 = call i8* @malloc_gc(i64 13)
  %2 = getelementptr i8, i8* %1, i64 0
  store i8 72, i8* %2
  %3 = getelementptr i8, i8* %1, i64 1
  store i8 101, i8* %3
  %4 = getelementptr i8, i8* %1, i64 2
  store i8 108, i8* %4
  %5 = getelementptr i8, i8* %1, i64 3
  store i8 108, i8* %5
  %6 = getelementptr i8, i8* %1, i64 4
  store i8 111, i8* %6
  %7 = getelementptr i8, i8* %1, i64 5
  store i8 44, i8* %7
  %8 = getelementptr i8, i8* %1, i64 6
  store i8 32, i8* %8
  %9 = getelementptr i8, i8* %1, i64 7
  store i8 119, i8* %9
  %10 = getelementptr i8, i8* %1, i64 8
  store i8 111, i8* %10
  %11 = getelementptr i8, i8* %1, i64 9
  store i8 114, i8* %11
  %12 = getelementptr i8, i8* %1, i64 10
  store i8 108, i8* %12
  %13 = getelementptr i8, i8* %1, i64 11
  store i8 100, i8* %13
  %14 = getelementptr i8, i8* %1, i64 12
  store i8 0, i8* %14
  %15 = call {} @println.9(i8* %1)
  ret {} %15
}

declare {} @println(i8*)

define {} @println.9(i8*) {
  %2 = call {} @println(i8* %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.13()
  ret i32 0
}
