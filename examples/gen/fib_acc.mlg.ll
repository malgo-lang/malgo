; ModuleID = './examples/fib_acc.mlg'
source_filename = "./examples/fib_acc.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.164() {
  %1 = call {} @fib_loop.131(i64 30)
  ret {} %1
}

declare i1 @le_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

declare i64 @add_i64(i64, i64)

define i64 @fib_acc.99(i64, i64, i64) {
  %4 = call i1 @le_i64(i64 %0, i64 0)
  %5 = alloca i64
  br i1 %4, label %then_0, label %else_0

then_0:                                           ; preds = %3
  store i64 %1, i64* %5
  br label %endif_0

else_0:                                           ; preds = %3
  %6 = call i64 @sub_i64(i64 %0, i64 1)
  %7 = call i64 @add_i64(i64 %1, i64 %2)
  %8 = call i64 @fib_acc.99(i64 %6, i64 %2, i64 %7)
  store i64 %8, i64* %5
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %5
  ret i64 %9
}

define {} @fib_loop.103(i64) {
  %2 = call i1 @le_i64(i64 %0, i64 0)
  %3 = alloca {}
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  %4 = call i64 @fib_acc.99(i64 0, i64 1, i64 1)
  %5 = call {} @print_int.93(i64 %4)
  %6 = call {} @newline.96({} undef)
  store {} %6, {}* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %7 = call i64 @fib_acc.99(i64 %0, i64 1, i64 1)
  %8 = call {} @print_int.93(i64 %7)
  %9 = call {} @newline.96({} undef)
  %10 = call i64 @sub_i64(i64 %0, i64 1)
  %11 = call {} @fib_loop.103(i64 %10)
  store {} %11, {}* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %12 = load {}, {}* %3
  ret {} %12
}

define {} @fib_loop.131(i64) {
  %2 = call i1 @le_i64(i64 %0, i64 0)
  %3 = alloca {}
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  %4 = call i64 @fib_acc.133(i64 0, i64 1, i64 1)
  %5 = call {} @print_int.93(i64 %4)
  %6 = call {} @newline.96({} undef)
  store {} %6, {}* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %7 = call i64 @fib_acc.133(i64 %0, i64 1, i64 1)
  %8 = call {} @print_int.93(i64 %7)
  %9 = call {} @newline.96({} undef)
  %10 = call i64 @sub_i64(i64 %0, i64 1)
  %11 = call {} @fib_loop.131(i64 %10)
  store {} %11, {}* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %12 = load {}, {}* %3
  ret {} %12
}

define i64 @fib_acc.133(i64, i64, i64) {
  %4 = call i1 @le_i64(i64 %0, i64 0)
  %5 = alloca i64
  br i1 %4, label %then_0, label %else_0

then_0:                                           ; preds = %3
  store i64 %1, i64* %5
  br label %endif_0

else_0:                                           ; preds = %3
  %6 = call i64 @sub_i64(i64 %0, i64 1)
  %7 = call i64 @add_i64(i64 %1, i64 %2)
  %8 = call i64 @fib_acc.133(i64 %6, i64 %2, i64 %7)
  store i64 %8, i64* %5
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %5
  ret i64 %9
}

declare {} @newline({})

define {} @newline.96({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.93(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.164()
  ret i32 0
}
