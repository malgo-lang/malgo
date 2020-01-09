; ModuleID = './examples/hello.mlg'
source_filename = "./examples/hello.mlg"

@0 = unnamed_addr constant [13 x i8] c"Hello, world\00"

declare {}* @println(i8*)

define {}* @println0(i8*) {
  %2 = call {}* @println(i8* %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call {}* @println0(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @0, i32 0, i32 0))
  ret i32 0
}
