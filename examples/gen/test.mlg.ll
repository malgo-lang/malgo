; ModuleID = './examples/test.mlg'
source_filename = "./examples/test.mlg"

declare i8* @GC_malloc(i64)

define i32 @main4() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  ret i32 0
}

define i64 @answer5() {
  ret i64 42
}

define i32 @main() {
  %1 = call i32 @main4()
  ret i32 0
}
