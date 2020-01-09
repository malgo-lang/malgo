; ModuleID = './examples/test4.mlg'
source_filename = "./examples/test4.mlg"

@0 = unnamed_addr constant [6 x i8] c"hello\00"

define i8* @f0(i64, i8) {
  ret i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i32 0, i32 0)
}

define i32 @main() {
  %1 = call i8* @f0(i64 42, i8 97)
  ret i32 0
}
