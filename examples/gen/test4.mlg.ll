; ModuleID = './examples/test4.mlg'
source_filename = "./examples/test4.mlg"

@0 = global [6 x i8] c"hello\00"

define i8* @f11(i64, i8) {
  %3 = bitcast [6 x i8]* @0 to i8*
  ret i8* %3
}

define i32 @main() {
  %1 = call i8* @f11(i64 42, i8 97)
  ret i32 0
}
