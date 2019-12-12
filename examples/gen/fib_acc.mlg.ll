; ModuleID = './examples/fib_acc.mlg'
source_filename = "./examples/fib_acc.mlg"

declare i8* @GC_malloc(i64)

define {}* @fib_loop.34(i64) {
  %2 = icmp sle i64 %0, 0
  %3 = alloca {}*
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  %4 = call i64 @fib_acc.22(i64 0, i64 1, i64 1)
  %5 = call {}* @print_int.8(i64 %4)
  %6 = call i8* @GC_malloc(i64 0)
  %7 = bitcast i8* %6 to {}*
  %8 = call {}* @newline.9({}* %7)
  store {}* %8, {}** %3
  br label %endif_0

else_0:                                           ; preds = %1
  %9 = call i64 @fib_acc.22(i64 %0, i64 1, i64 1)
  %10 = call {}* @print_int.8(i64 %9)
  %11 = call i8* @GC_malloc(i64 0)
  %12 = bitcast i8* %11 to {}*
  %13 = call {}* @newline.9({}* %12)
  %14 = sub i64 %0, 1
  %15 = call {}* @fib_loop.34(i64 %14)
  store {}* %15, {}** %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %16 = load {}*, {}** %3
  ret {}* %16
}

define i64 @fib_acc.22(i64, i64, i64) {
  %4 = icmp sle i64 %0, 0
  %5 = alloca i64
  br i1 %4, label %then_0, label %else_0

then_0:                                           ; preds = %3
  store i64 %1, i64* %5
  br label %endif_0

else_0:                                           ; preds = %3
  %6 = sub i64 %0, 1
  %7 = add i64 %1, %2
  %8 = call i64 @fib_acc.22(i64 %6, i64 %2, i64 %7)
  store i64 %8, i64* %5
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %5
  ret i64 %9
}

declare {}* @newline({}*)

define {}* @newline.9({}*) {
  %2 = call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int.8(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call {}* @fib_loop.34(i64 30)
  ret i32 0
}
