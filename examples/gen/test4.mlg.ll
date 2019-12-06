; ModuleID = './examples/test4.mlg'
source_filename = "./examples/test4.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define i8* @main.18() {
  %1 = call i8* @f.13(i64 42, i8 97)
  ret i8* %1
}

define i8* @f.13(i64, i8) {
  %3 = call i8* @malloc_gc(i64 6)
  %4 = getelementptr i8, i8* %3, i64 0
  store i8 104, i8* %4
  %5 = getelementptr i8, i8* %3, i64 1
  store i8 101, i8* %5
  %6 = getelementptr i8, i8* %3, i64 2
  store i8 108, i8* %6
  %7 = getelementptr i8, i8* %3, i64 3
  store i8 108, i8* %7
  %8 = getelementptr i8, i8* %3, i64 4
  store i8 111, i8* %8
  %9 = getelementptr i8, i8* %3, i64 5
  store i8 0, i8* %9
  ret i8* %3
}

define i32 @main() {
  call void @init_gc()
  %1 = call i8* @main.18()
  ret i32 0
}
