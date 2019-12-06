; ModuleID = './examples/fib.mlg'
source_filename = "./examples/fib.mlg"

declare i8* @malloc_gc(i64)

declare void @init_gc()

define {} @main.144() {
  %1 = call {} @fib_loop.116(i64 30, i64 0)
  ret {} %1
}

declare i1 @le_i64(i64, i64)

declare i64 @sub_i64(i64, i64)

declare i64 @add_i64(i64, i64)

define i64 @fib.90(i64) {
  %2 = call i1 @le_i64(i64 %0, i64 1)
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %4 = call i64 @sub_i64(i64 %0, i64 1)
  %5 = call i64 @fib.90(i64 %4)
  %6 = call i64 @sub_i64(i64 %0, i64 2)
  %7 = call i64 @fib.90(i64 %6)
  %8 = call i64 @add_i64(i64 %5, i64 %7)
  store i64 %8, i64* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

define {} @fib_loop.92(i64, i64) {
  %3 = call i64 @fib.90(i64 %1)
  %4 = call {} @print_int.84(i64 %3)
  %5 = call {} @newline.87({} undef)
  %6 = call i1 @le_i64(i64 %0, i64 %1)
  %7 = alloca {}
  br i1 %6, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %7
  br label %endif_0

else_0:                                           ; preds = %2
  %8 = call i64 @add_i64(i64 %1, i64 1)
  %9 = call {} @fib_loop.92(i64 %0, i64 %8)
  store {} %9, {}* %7
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %10 = load {}, {}* %7
  ret {} %10
}

define {} @fib_loop.116(i64, i64) {
  %3 = call i64 @fib.119(i64 %1)
  %4 = call {} @print_int.84(i64 %3)
  %5 = call {} @newline.87({} undef)
  %6 = call i1 @le_i64(i64 %0, i64 %1)
  %7 = alloca {}
  br i1 %6, label %then_0, label %else_0

then_0:                                           ; preds = %2
  store {} undef, {}* %7
  br label %endif_0

else_0:                                           ; preds = %2
  %8 = call i64 @add_i64(i64 %1, i64 1)
  %9 = call {} @fib_loop.116(i64 %0, i64 %8)
  store {} %9, {}* %7
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %10 = load {}, {}* %7
  ret {} %10
}

define i64 @fib.119(i64) {
  %2 = call i1 @le_i64(i64 %0, i64 1)
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %4 = call i64 @sub_i64(i64 %0, i64 1)
  %5 = call i64 @fib.119(i64 %4)
  %6 = call i64 @sub_i64(i64 %0, i64 2)
  %7 = call i64 @fib.119(i64 %6)
  %8 = call i64 @add_i64(i64 %5, i64 %7)
  store i64 %8, i64* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

declare {} @newline({})

define {} @newline.87({}) {
  %2 = call {} @newline({} %0)
  ret {} %2
}

declare {} @print_int(i64)

define {} @print_int.84(i64) {
  %2 = call {} @print_int(i64 %0)
  ret {} %2
}

define i32 @main() {
  call void @init_gc()
  %1 = call {} @main.144()
  ret i32 0
}
