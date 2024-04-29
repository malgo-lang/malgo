; ModuleID = './test/testcases/malgo/Primitive.mlg'
source_filename = "./test/testcases/malgo/Primitive.mlg"

@Builtin.undefined = global ptr undef

declare void @GC_init()

declare ptr @malgo_panic(ptr)

declare ptr @malgo_unsafe_cast(ptr)

declare i32 @malgo_add_int32_t(i32, i32)

declare i32 @malgo_sub_int32_t(i32, i32)

declare i32 @malgo_mul_int32_t(i32, i32)

declare i32 @malgo_div_int32_t(i32, i32)

declare i64 @malgo_add_int64_t(i64, i64)

declare i64 @malgo_sub_int64_t(i64, i64)

declare i64 @malgo_mul_int64_t(i64, i64)

declare i64 @malgo_div_int64_t(i64, i64)

declare float @malgo_add_float(float, float)

declare float @malgo_sub_float(float, float)

declare float @malgo_mul_float(float, float)

declare float @malgo_div_float(float, float)

declare double @malgo_add_double(double, double)

declare double @malgo_sub_double(double, double)

declare double @malgo_mul_double(double, double)

declare double @malgo_div_double(double, double)

declare float @sqrtf(float)

declare double @sqrt(double)

declare i32 @malgo_eq_int32_t(i32, i32)

declare i32 @malgo_ne_int32_t(i32, i32)

declare i32 @malgo_lt_int32_t(i32, i32)

declare i32 @malgo_gt_int32_t(i32, i32)

declare i32 @malgo_le_int32_t(i32, i32)

declare i32 @malgo_ge_int32_t(i32, i32)

declare i32 @malgo_eq_int64_t(i64, i64)

declare i32 @malgo_ne_int64_t(i64, i64)

declare i32 @malgo_lt_int64_t(i64, i64)

declare i32 @malgo_gt_int64_t(i64, i64)

declare i32 @malgo_le_int64_t(i64, i64)

declare i32 @malgo_ge_int64_t(i64, i64)

declare i32 @malgo_eq_float(float, float)

declare i32 @malgo_ne_float(float, float)

declare i32 @malgo_lt_float(float, float)

declare i32 @malgo_gt_float(float, float)

declare i32 @malgo_le_float(float, float)

declare i32 @malgo_ge_float(float, float)

declare i32 @malgo_eq_double(double, double)

declare i32 @malgo_ne_double(double, double)

declare i32 @malgo_lt_double(double, double)

declare i32 @malgo_gt_double(double, double)

declare i32 @malgo_le_double(double, double)

declare i32 @malgo_ge_double(double, double)

declare i32 @malgo_eq_char(i8, i8)

declare i32 @malgo_ne_char(i8, i8)

declare i32 @malgo_lt_char(i8, i8)

declare i32 @malgo_gt_char(i8, i8)

declare i32 @malgo_le_char(i8, i8)

declare i32 @malgo_ge_char(i8, i8)

declare i32 @malgo_eq_string(ptr, ptr)

declare i32 @malgo_ne_string(ptr, ptr)

declare i32 @malgo_lt_string(ptr, ptr)

declare i32 @malgo_gt_string(ptr, ptr)

declare i32 @malgo_le_string(ptr, ptr)

declare i32 @malgo_ge_string(ptr, ptr)

declare i32 @malgo_char_ord(i8)

declare i32 @malgo_is_digit(i8)

declare i32 @malgo_is_lower(i8)

declare i32 @malgo_is_upper(i8)

declare i32 @malgo_is_alphanum(i8)

declare i64 @malgo_string_length(ptr)

declare i8 @malgo_string_at(i64, ptr)

declare ptr @malgo_string_cons(i8, ptr)

declare ptr @malgo_string_append(ptr, ptr)

declare ptr @malgo_substring(ptr, i64, i64)

declare ptr @malgo_int32_t_to_string(i32)

declare ptr @malgo_int64_t_to_string(i64)

declare ptr @malgo_float_to_string(float)

declare ptr @malgo_double_to_string(double)

declare ptr @malgo_char_to_string(i8)

declare ptr @malgo_exit_failure(ptr)

declare ptr @malgo_newline(ptr)

declare ptr @malgo_print_char(i8)

declare ptr @malgo_print_string(ptr)

declare ptr @malgo_flush(ptr)

declare i8 @malgo_get_char(ptr)

declare ptr @malgo_get_contents(ptr)

declare ptr @malgo_malloc(i64)

define internal i32 @"Primitive.#let_closure_22"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$gtFloat#_curry_3270"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtFloat#"(ptr %0, float %"Builtin.$x_3265_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_3265_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_22", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Primitive.#let_closure_23"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"Builtin.$malgo_mul_double_curry_1894"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_mul_double(ptr %0, double %"Builtin.$p_1892_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1892_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_23", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$ltFloat#_curry_2665"(ptr %0, float %"Builtin.$x_2666_0", float %"Builtin.$y_2667_0") {
  %2 = call ptr @Builtin.malgo_lt_float(ptr null, float %"Builtin.$x_2666_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_2667_0")
  ret i32 %7
}

define internal ptr @"Builtin.$eqChar_curry_3756"(ptr %0, ptr %"Builtin.$char#_3757_0", ptr %"Builtin.$char#_3758_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3757_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3757_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3758_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3758_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.eqChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.String#"(ptr %0, ptr %"Builtin.$p_1802_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { ptr } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { ptr } }, ptr %2, i32 0, i32 1, i32 0
  store ptr %"Builtin.$p_1802_0", ptr %4, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_24"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$addInt64_curry_4016"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.addInt64(ptr %0, ptr %"Builtin.$int64#_4007_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_4007_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_24", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_25"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$neString#_curry_2429"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.neString#"(ptr %0, ptr %"Builtin.$x_2424_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_2424_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_25", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_eq_double_curry_2016"(ptr %0, double %"Builtin.$p_2017_0", double %"Builtin.$p_2018_0") {
  %2 = call i32 @malgo_eq_double(double %"Builtin.$p_2017_0", double %"Builtin.$p_2018_0")
  ret i32 %2
}

define internal float @"Primitive.#let_closure_26"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"Builtin.$malgo_div_float_curry_1876"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @Builtin.malgo_div_float(ptr %0, float %"Builtin.$p_1874_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1874_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_26", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_char_to_string(ptr %0, i8 %"Builtin.$p_2159_0") {
  %2 = call ptr @malgo_char_to_string(i8 %"Builtin.$p_2159_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_27"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_mul_int32_t_curry_1822"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_mul_int32_t(ptr %0, i32 %"Builtin.$p_1820_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1820_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_27", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_28"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_lt_double_curry_2028"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_double(ptr %0, double %"Builtin.$p_2026_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2026_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_28", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_29"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$leDouble_curry_2813"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leDouble(ptr %0, ptr %"Builtin.$double#_2804_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_2804_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_29", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$subInt32#_curry_2292"(ptr %0, i32 %"Builtin.$x_2293_0", i32 %"Builtin.$y_2294_0") {
  %2 = call ptr @Builtin.malgo_sub_int32_t(ptr null, i32 %"Builtin.$x_2293_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_2294_0")
  ret i32 %7
}

define internal i32 @"Builtin.$ltDouble#_curry_2677"(ptr %0, double %"Builtin.$x_2678_0", double %"Builtin.$y_2679_0") {
  %2 = call ptr @Builtin.malgo_lt_double(ptr null, double %"Builtin.$x_2678_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_2679_0")
  ret i32 %7
}

define internal ptr @"Builtin.string#"(ptr %0, ptr %"Builtin.$x_2383_0") {
  %2 = call ptr @"Builtin.String#"(ptr null, ptr %"Builtin.$x_2383_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_30"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_ge_string_curry_2118"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_string(ptr %0, ptr %"Builtin.$p_2116_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2116_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_30", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_31"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %p_addr_1 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 1
  %p_1 = load i64, ptr %p_addr_1, align 4
  %3 = call ptr @"Builtin.$malgo_substring_curry_2149"(ptr null, ptr %p_0, i64 %p_1, i64 %1)
  ret ptr %3
}

define internal ptr @"Primitive.$raw_let_19"(ptr %0, ptr %"Builtin.$p_2146_0", i64 %"Builtin.$p_2147_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2146_0", ptr %p_0, align 8
  %p_1 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 1
  store i64 %"Builtin.$p_2147_0", ptr %p_1, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_31", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_string(ptr %0, ptr %"Builtin.$p_2163_0") {
  %2 = call ptr @malgo_print_string(ptr %"Builtin.$p_2163_0")
  ret ptr %2
}

define internal ptr @"Builtin.$divFloat_curry_3854"(ptr %0, ptr %"Builtin.$float#_3855_0", ptr %"Builtin.$float#_3856_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3855_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3855_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3856_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3856_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.divFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call float %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.Float#"(ptr null, float %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_gt_int64_t_curry_1962"(ptr %0, i64 %"Builtin.$p_1963_0", i64 %"Builtin.$p_1964_0") {
  %2 = call i32 @malgo_gt_int64_t(i64 %"Builtin.$p_1963_0", i64 %"Builtin.$p_1964_0")
  ret i32 %2
}

define internal ptr @"Builtin.$divDouble_curry_3886"(ptr %0, ptr %"Builtin.$double#_3887_0", ptr %"Builtin.$double#_3888_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3887_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3887_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3888_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3888_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.divDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call double %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.Double#"(ptr null, double %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$geInt32#_curry_3442"(ptr %0, i32 %"Builtin.$x_3443_0", i32 %"Builtin.$y_3444_0") {
  %2 = call ptr @Builtin.malgo_ge_int32_t(ptr null, i32 %"Builtin.$x_3443_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_3444_0")
  ret i32 %7
}

define internal ptr @Builtin.sqrtFloat(ptr %0, ptr %"Builtin.$float#_2387_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2387_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2387_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = call float @"Builtin.sqrtFloat#"(ptr null, float %6)
  %8 = call ptr @"Builtin.Float#"(ptr null, float %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$geInt32_curry_3458"(ptr %0, ptr %"Builtin.$int32#_3459_0", ptr %"Builtin.$int32#_3460_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3459_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3459_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3460_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3460_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.geInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.toStringInt32(ptr %0, ptr %"Builtin.$int32#_2181_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2181_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2181_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = call ptr @"Builtin.toStringInt32#"(ptr null, i32 %6)
  %8 = call ptr @"Builtin.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.printChar#"(ptr %0, i8 %"Builtin.$x_2404_0") {
  %2 = call ptr @Builtin.malgo_print_char(ptr null, i8 %"Builtin.$x_2404_0")
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_32"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqFloat_curry_3692"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqFloat(ptr %0, ptr %"Builtin.$float#_3683_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_3683_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_32", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_33"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_gt_float_curry_1998"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_float(ptr %0, float %"Builtin.$p_1996_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1996_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_33", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_34"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_gt_double_curry_2034"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_double(ptr %0, double %"Builtin.$p_2032_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2032_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_34", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_35"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltInt64_curry_2993"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltInt64(ptr %0, ptr %"Builtin.$int64#_2984_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_2984_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_35", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_36"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$neFloat#_curry_2465"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.neFloat#"(ptr %0, float %"Builtin.$x_2460_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_2460_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_36", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_37"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$neDouble_curry_3053"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neDouble(ptr %0, ptr %"Builtin.$double#_3044_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_3044_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_37", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$geChar#_curry_3538"(ptr %0, i8 %"Builtin.$x_3539_0", i8 %"Builtin.$y_3540_0") {
  %2 = call ptr @Builtin.malgo_ge_char(ptr null, i8 %"Builtin.$x_3539_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_3540_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_38"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$ltChar#_curry_2689"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltChar#"(ptr %0, i8 %"Builtin.$x_2684_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_2684_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_38", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_39"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$gtInt32#_curry_3238"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtInt32#"(ptr %0, i32 %"Builtin.$x_3233_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_3233_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_39", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$mulDouble#_curry_2597"(ptr %0, double %"Builtin.$x_2598_0", double %"Builtin.$y_2599_0") {
  %2 = call ptr @Builtin.malgo_mul_double(ptr null, double %"Builtin.$x_2598_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"Builtin.$y_2599_0")
  ret double %7
}

define internal i8 @"Builtin.$atString#_curry_3936"(ptr %0, i64 %"Builtin.$i_3937_0", ptr %"Builtin.$x_3938_0") {
  %2 = call ptr @Builtin.malgo_string_at(ptr null, i64 %"Builtin.$i_3937_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i8 %6(ptr %4, ptr %"Builtin.$x_3938_0")
  ret i8 %7
}

define internal ptr @Builtin.isAlphanum(ptr %0, ptr %"Builtin.$char#_3160_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3160_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3160_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call i32 @"Builtin.isAlphanum#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.isTrue#"(ptr null, i32 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i64 @"Builtin.$malgo_sub_int64_t_curry_1840"(ptr %0, i64 %"Builtin.$p_1841_0", i64 %"Builtin.$p_1842_0") {
  %2 = call i64 @malgo_sub_int64_t(i64 %"Builtin.$p_1841_0", i64 %"Builtin.$p_1842_0")
  ret i64 %2
}

define internal i32 @"Builtin.$malgo_lt_double_curry_2028"(ptr %0, double %"Builtin.$p_2029_0", double %"Builtin.$p_2030_0") {
  %2 = call i32 @malgo_lt_double(double %"Builtin.$p_2029_0", double %"Builtin.$p_2030_0")
  ret i32 %2
}

define internal ptr @Builtin.exitFailure(ptr %0, ptr %"Builtin.$__3571_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Builtin.malgo_exit_failure(ptr null, ptr %2)
  ret ptr %4
}

define internal ptr @"Builtin.$addInt64_curry_4016"(ptr %0, ptr %"Builtin.$int64#_4017_0", ptr %"Builtin.$int64#_4018_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_4017_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_4017_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_4018_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_4018_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.addInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.Int64#"(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_40"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ge_int64_t_curry_1974"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_int64_t(ptr %0, i64 %"Builtin.$p_1972_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1972_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_40", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_41"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$mulInt32#_curry_2533"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.mulInt32#"(ptr %0, i32 %"Builtin.$x_2528_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_2528_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_41", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_42"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$mulFloat_curry_2581"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.mulFloat(ptr %0, ptr %"Builtin.$float#_2572_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_2572_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_42", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_43"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_ge_char_curry_2082"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_char(ptr %0, i8 %"Builtin.$p_2080_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2080_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_43", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_44"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltDouble_curry_2933"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltDouble(ptr %0, ptr %"Builtin.$double#_2924_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_2924_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_44", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$eqFloat#_curry_3676"(ptr %0, float %"Builtin.$x_3677_0", float %"Builtin.$y_3678_0") {
  %2 = call ptr @Builtin.malgo_eq_float(ptr null, float %"Builtin.$x_3677_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_3678_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_45"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$mulInt64_curry_2517"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.mulInt64(ptr %0, ptr %"Builtin.$int64#_2508_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_2508_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_45", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_46"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$geFloat_curry_3490"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geFloat(ptr %0, ptr %"Builtin.$float#_3481_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_3481_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_46", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_47"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_eq_int32_t_curry_1908"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_int32_t(ptr %0, i32 %"Builtin.$p_1906_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1906_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_47", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_ne_string_curry_2094"(ptr %0, ptr %"Builtin.$p_2095_0", ptr %"Builtin.$p_2096_0") {
  %2 = call i32 @malgo_ne_string(ptr %"Builtin.$p_2095_0", ptr %"Builtin.$p_2096_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_48"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$gtChar#_curry_3334"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtChar#"(ptr %0, i8 %"Builtin.$x_3329_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_3329_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_48", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Primitive.#let_closure_49"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"Builtin.$malgo_div_double_curry_1900"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_div_double(ptr %0, double %"Builtin.$p_1898_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1898_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_49", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_50"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$subInt32_curry_2308"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.subInt32(ptr %0, ptr %"Builtin.$int32#_2299_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_2299_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_50", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$gtFloat#_curry_3270"(ptr %0, float %"Builtin.$x_3271_0", float %"Builtin.$y_3272_0") {
  %2 = call ptr @Builtin.malgo_gt_float(ptr null, float %"Builtin.$x_3271_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_3272_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_51"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_eq_float_curry_1980"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_float(ptr %0, float %"Builtin.$p_1978_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1978_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_51", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_52"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtInt32_curry_3254"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtInt32(ptr %0, ptr %"Builtin.$int32#_3245_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_3245_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_52", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_53"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$eqChar#_curry_3740"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqChar#"(ptr %0, i8 %"Builtin.$x_3735_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_3735_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_53", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$leDouble#_curry_2756"(ptr %0, double %"Builtin.$x_2757_0", double %"Builtin.$y_2758_0") {
  %2 = call ptr @Builtin.malgo_le_double(ptr null, double %"Builtin.$x_2757_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_2758_0")
  ret i32 %7
}

define internal i32 @"Builtin.$malgo_ne_char_curry_2058"(ptr %0, i8 %"Builtin.$p_2059_0", i8 %"Builtin.$p_2060_0") {
  %2 = call i32 @malgo_ne_char(i8 %"Builtin.$p_2059_0", i8 %"Builtin.$p_2060_0")
  ret i32 %2
}

define internal ptr @"Builtin.$ltFloat_curry_2953"(ptr %0, ptr %"Builtin.$float#_2954_0", ptr %"Builtin.$float#_2955_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2954_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2954_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2955_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2955_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.ltFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_54"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$neDouble#_curry_2477"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.neDouble#"(ptr %0, double %"Builtin.$x_2472_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_2472_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_54", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_55"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$divDouble_curry_3886"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.divDouble(ptr %0, ptr %"Builtin.$double#_3877_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_3877_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_55", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_56"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_le_string_curry_2112"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_string(ptr %0, ptr %"Builtin.$p_2110_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2110_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_56", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.float#"(ptr %0, float %"Builtin.$x_3569_0") {
  %2 = call ptr @"Builtin.Float#"(ptr null, float %"Builtin.$x_3569_0")
  ret ptr %2
}

define internal ptr @"Builtin.$mulFloat_curry_2581"(ptr %0, ptr %"Builtin.$float#_2582_0", ptr %"Builtin.$float#_2583_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2582_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2582_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2583_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2583_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.mulFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call float %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.Float#"(ptr null, float %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.Float#"(ptr %0, float %"Builtin.$p_1796_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { float } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { float } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { float } }, ptr %2, i32 0, i32 1, i32 0
  store float %"Builtin.$p_1796_0", ptr %4, align 4
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_57"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_eq_char_curry_2052"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_char(ptr %0, i8 %"Builtin.$p_2050_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2050_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_57", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$ltInt64_curry_2993"(ptr %0, ptr %"Builtin.$int64#_2994_0", ptr %"Builtin.$int64#_2995_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2994_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2994_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2995_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2995_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.ltInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"Builtin.$malgo_mul_float_curry_1870"(ptr %0, float %"Builtin.$p_1871_0", float %"Builtin.$p_1872_0") {
  %2 = call float @malgo_mul_float(float %"Builtin.$p_1871_0", float %"Builtin.$p_1872_0")
  ret float %2
}

define internal i32 @"Builtin.$malgo_gt_double_curry_2034"(ptr %0, double %"Builtin.$p_2035_0", double %"Builtin.$p_2036_0") {
  %2 = call i32 @malgo_gt_double(double %"Builtin.$p_2035_0", double %"Builtin.$p_2036_0")
  ret i32 %2
}

define internal ptr @Builtin.printChar(ptr %0, ptr %"Builtin.$char#_2406_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2406_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2406_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @"Builtin.printChar#"(ptr null, i8 %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Primitive.main(ptr %0, ptr %"Primitive.$$__11_0") {
  %2 = call ptr @"Builtin.addInt64#"(ptr null, i64 40)
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 2)
  %8 = call ptr @"Builtin.toStringInt64#"(ptr null, i64 %7)
  %9 = call ptr @"Builtin.printString#"(ptr null, ptr %8)
  ret ptr %9
}

define internal i32 @"Primitive.#let_closure_58"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_gt_char_curry_2070"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_char(ptr %0, i8 %"Builtin.$p_2068_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2068_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_58", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.toStringFloat#"(ptr %0, float %"Builtin.$x_2186_0") {
  %2 = call ptr @Builtin.malgo_float_to_string(ptr null, float %"Builtin.$x_2186_0")
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_59"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$leFloat_curry_2833"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leFloat(ptr %0, ptr %"Builtin.$float#_2824_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_2824_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_59", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.toStringInt64(ptr %0, ptr %"Builtin.$int64#_2174_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2174_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2174_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = call ptr @"Builtin.toStringInt64#"(ptr null, i64 %6)
  %8 = call ptr @"Builtin.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.malgo_unsafe_cast(ptr %0, ptr %"Builtin.$p_1807_0") {
  %2 = call ptr @malgo_unsafe_cast(ptr %"Builtin.$p_1807_0")
  ret ptr %2
}

define internal ptr @"Builtin.$subDouble_curry_2372"(ptr %0, ptr %"Builtin.$double#_2373_0", ptr %"Builtin.$double#_2374_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2373_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2373_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2374_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2374_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.subDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call double %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.Double#"(ptr null, double %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_60"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$ltDouble#_curry_2677"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltDouble#"(ptr %0, double %"Builtin.$x_2672_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_2672_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_60", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$neInt32#_curry_2453"(ptr %0, i32 %"Builtin.$x_2454_0", i32 %"Builtin.$y_2455_0") {
  %2 = call ptr @Builtin.malgo_ne_int32_t(ptr null, i32 %"Builtin.$x_2454_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_2455_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_61"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtChar_curry_3350"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtChar(ptr %0, ptr %"Builtin.$char#_3341_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_3341_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_61", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$leDouble_curry_2813"(ptr %0, ptr %"Builtin.$double#_2814_0", ptr %"Builtin.$double#_2815_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2814_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2814_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2815_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2815_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.leDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_62"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$leChar_curry_2793"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leChar(ptr %0, ptr %"Builtin.$char#_2784_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_2784_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_62", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$malgo_string_cons_curry_2136"(ptr %0, i8 %"Builtin.$p_2137_0", ptr %"Builtin.$p_2138_0") {
  %2 = call ptr @malgo_string_cons(i8 %"Builtin.$p_2137_0", ptr %"Builtin.$p_2138_0")
  ret ptr %2
}

define internal i32 @"Builtin.$divInt32#_curry_3806"(ptr %0, i32 %"Builtin.$x_3807_0", i32 %"Builtin.$y_3808_0") {
  %2 = call ptr @Builtin.malgo_div_int32_t(ptr null, i32 %"Builtin.$x_3807_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_3808_0")
  ret i32 %7
}

define internal i32 @"Builtin.$eqDouble#_curry_3708"(ptr %0, double %"Builtin.$x_3709_0", double %"Builtin.$y_3710_0") {
  %2 = call ptr @Builtin.malgo_eq_double(ptr null, double %"Builtin.$x_3709_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_3710_0")
  ret i32 %7
}

define internal ptr @"Builtin.$divInt32_curry_3822"(ptr %0, ptr %"Builtin.$int32#_3823_0", ptr %"Builtin.$int32#_3824_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3823_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3823_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3824_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3824_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.divInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.isUpper(ptr %0, ptr %"Builtin.$char#_2779_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2779_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2779_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call i32 @"Builtin.isUpper#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.isTrue#"(ptr null, i32 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_63"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$subInt64_curry_2276"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.subInt64(ptr %0, ptr %"Builtin.$int64#_2267_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_2267_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_63", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$eqString#_curry_3580"(ptr %0, ptr %"Builtin.$x_3581_0", ptr %"Builtin.$y_3582_0") {
  %2 = call ptr @Builtin.malgo_eq_string(ptr null, ptr %"Builtin.$x_3581_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_3582_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_64"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$geDouble#_curry_3506"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.geDouble#"(ptr %0, double %"Builtin.$x_3501_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_3501_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_64", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$leInt32_curry_2853"(ptr %0, ptr %"Builtin.$int32#_2854_0", ptr %"Builtin.$int32#_2855_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2854_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2854_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2855_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2855_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.leInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.getChar(ptr %0, ptr %"Builtin.$__3367_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call i8 @Builtin.malgo_get_char(ptr null, ptr %2)
  %5 = call ptr @"Builtin.Char#"(ptr null, i8 %4)
  ret ptr %5
}

define internal ptr @"Primitive.#let_closure_65"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtInt64_curry_3222"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtInt64(ptr %0, ptr %"Builtin.$int64#_3213_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3213_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_65", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_66"(ptr %0, i64 %1) {
  %str_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %start_addr_0 = getelementptr { ptr, i64 }, ptr %0, i32 0, i32 1
  %start_0 = load i64, ptr %start_addr_0, align 4
  %3 = call ptr @"Builtin.$substring#_curry_2215"(ptr null, ptr %str_0, i64 %start_0, i64 %1)
  ret ptr %3
}

define internal ptr @"Primitive.$raw_let_20"(ptr %0, ptr %"Builtin.$str_2207_0", i64 %"Builtin.$start_2208_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, i64 }, ptr null, i32 1) to i64))
  %str_0 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$str_2207_0", ptr %str_0, align 8
  %start_0 = getelementptr { ptr, i64 }, ptr %let_capture_0, i32 0, i32 1
  store i64 %"Builtin.$start_2208_0", ptr %start_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_66", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.toStringFloat(ptr %0, ptr %"Builtin.$float#_2188_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2188_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2188_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = call ptr @"Builtin.toStringFloat#"(ptr null, float %6)
  %8 = call ptr @"Builtin.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_67"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqInt64_curry_3628"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqInt64(ptr %0, ptr %"Builtin.$int64#_3619_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3619_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_67", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Builtin.$malgo_mul_int64_t_curry_1846"(ptr %0, i64 %"Builtin.$p_1847_0", i64 %"Builtin.$p_1848_0") {
  %2 = call i64 @malgo_mul_int64_t(i64 %"Builtin.$p_1847_0", i64 %"Builtin.$p_1848_0")
  ret i64 %2
}

define internal i64 @"Primitive.#let_closure_68"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"Builtin.$subInt64#_curry_2260"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"Builtin.subInt64#"(ptr %0, i64 %"Builtin.$x_2255_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_2255_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_68", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$gtDouble_curry_3318"(ptr %0, ptr %"Builtin.$double#_3319_0", ptr %"Builtin.$double#_3320_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3319_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3319_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3320_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3320_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.gtDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"Builtin.$subFloat#_curry_2324"(ptr %0, float %"Builtin.$x_2325_0", float %"Builtin.$y_2326_0") {
  %2 = call ptr @Builtin.malgo_sub_float(ptr null, float %"Builtin.$x_2325_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call float %6(ptr %4, float %"Builtin.$y_2326_0")
  ret float %7
}

define internal i32 @"Builtin.$malgo_lt_int64_t_curry_1956"(ptr %0, i64 %"Builtin.$p_1957_0", i64 %"Builtin.$p_1958_0") {
  %2 = call i32 @malgo_lt_int64_t(i64 %"Builtin.$p_1957_0", i64 %"Builtin.$p_1958_0")
  ret i32 %2
}

define internal ptr @"Builtin.$ltString_curry_3013"(ptr %0, ptr %"Builtin.$string#_3014_0", ptr %"Builtin.$string#_3015_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3014_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3014_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3015_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3015_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.ltString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$leString#_curry_2708"(ptr %0, ptr %"Builtin.$x_2709_0", ptr %"Builtin.$y_2710_0") {
  %2 = call ptr @Builtin.malgo_le_string(ptr null, ptr %"Builtin.$x_2709_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_2710_0")
  ret i32 %7
}

define internal ptr @Builtin.ordChar(ptr %0, ptr %"Builtin.$char#_2414_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2414_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2414_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call i32 @"Builtin.ordChar#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.Int32#"(ptr null, i32 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_69"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$subDouble_curry_2372"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.subDouble(ptr %0, ptr %"Builtin.$double#_2363_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_2363_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_69", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$leFloat_curry_2833"(ptr %0, ptr %"Builtin.$float#_2834_0", ptr %"Builtin.$float#_2835_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2834_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2834_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2835_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2835_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.leFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$neFloat#_curry_2465"(ptr %0, float %"Builtin.$x_2466_0", float %"Builtin.$y_2467_0") {
  %2 = call ptr @Builtin.malgo_ne_float(ptr null, float %"Builtin.$x_2466_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_2467_0")
  ret i32 %7
}

define internal ptr @"Builtin.int64#"(ptr %0, i64 %"Builtin.$x_3165_0") {
  %2 = call ptr @"Builtin.Int64#"(ptr null, i64 %"Builtin.$x_3165_0")
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_70"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Builtin.$malgo_string_append_curry_2142"(ptr null, ptr %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.malgo_string_append(ptr %0, ptr %"Builtin.$p_2140_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2140_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_70", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_71"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$eqString#_curry_3580"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqString#"(ptr %0, ptr %"Builtin.$x_3575_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_3575_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_71", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_72"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$geInt32#_curry_3442"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.geInt32#"(ptr %0, i32 %"Builtin.$x_3437_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_3437_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_72", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_73"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_div_int32_t_curry_1828"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_div_int32_t(ptr %0, i32 %"Builtin.$p_1826_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1826_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_73", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_74"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqDouble_curry_3724"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqDouble(ptr %0, ptr %"Builtin.$double#_3715_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_3715_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_74", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Primitive.#let_closure_75"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"Builtin.$malgo_sub_float_curry_1864"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @Builtin.malgo_sub_float(ptr %0, float %"Builtin.$p_1862_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1862_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_75", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.getContents(ptr %0, ptr %"Builtin.$__3361_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Builtin.malgo_get_contents(ptr null, ptr %2)
  %5 = call ptr @"Builtin.String#"(ptr null, ptr %4)
  ret ptr %5
}

define internal i32 @"Builtin.$malgo_gt_float_curry_1998"(ptr %0, float %"Builtin.$p_1999_0", float %"Builtin.$p_2000_0") {
  %2 = call i32 @malgo_gt_float(float %"Builtin.$p_1999_0", float %"Builtin.$p_2000_0")
  ret i32 %2
}

define internal ptr @"Builtin.Int64#"(ptr %0, i64 %"Builtin.$p_1794_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i64 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i64 } }, ptr %2, i32 0, i32 1, i32 0
  store i64 %"Builtin.$p_1794_0", ptr %4, align 4
  ret ptr %2
}

define internal ptr @"Builtin.printString#"(ptr %0, ptr %"Builtin.$x_2399_0") {
  %2 = call ptr @Builtin.malgo_print_string(ptr null, ptr %"Builtin.$x_2399_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_76"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$neInt64#_curry_2441"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.neInt64#"(ptr %0, i64 %"Builtin.$x_2436_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_2436_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_76", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.printString(ptr %0, ptr %"Builtin.$string#_2401_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2401_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2401_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @"Builtin.printString#"(ptr null, ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$neChar_curry_3033"(ptr %0, ptr %"Builtin.$char#_3034_0", ptr %"Builtin.$char#_3035_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3034_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3034_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3035_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3035_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.neChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_77"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqChar_curry_3756"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqChar(ptr %0, ptr %"Builtin.$char#_3747_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_3747_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_77", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$neChar#_curry_2489"(ptr %0, i8 %"Builtin.$x_2490_0", i8 %"Builtin.$y_2491_0") {
  %2 = call ptr @Builtin.malgo_ne_char(ptr null, i8 %"Builtin.$x_2490_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_2491_0")
  ret i32 %7
}

define internal float @"Primitive.#let_closure_78"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"Builtin.$mulFloat#_curry_2565"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"Builtin.mulFloat#"(ptr %0, float %"Builtin.$x_2560_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_2560_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_78", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.toStringInt64#"(ptr %0, i64 %"Builtin.$x_2172_0") {
  %2 = call ptr @Builtin.malgo_int64_t_to_string(ptr null, i64 %"Builtin.$x_2172_0")
  ret ptr %2
}

define internal ptr @"Builtin.$eqInt64_curry_3628"(ptr %0, ptr %"Builtin.$int64#_3629_0", ptr %"Builtin.$int64#_3630_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3629_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3629_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3630_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3630_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.eqInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_ge_float_curry_2010"(ptr %0, float %"Builtin.$p_2011_0", float %"Builtin.$p_2012_0") {
  %2 = call i32 @malgo_ge_float(float %"Builtin.$p_2011_0", float %"Builtin.$p_2012_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_lt_string_curry_2100"(ptr %0, ptr %"Builtin.$p_2101_0", ptr %"Builtin.$p_2102_0") {
  %2 = call i32 @malgo_lt_string(ptr %"Builtin.$p_2101_0", ptr %"Builtin.$p_2102_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_gt_int32_t_curry_1926"(ptr %0, i32 %"Builtin.$p_1927_0", i32 %"Builtin.$p_1928_0") {
  %2 = call i32 @malgo_gt_int32_t(i32 %"Builtin.$p_1927_0", i32 %"Builtin.$p_1928_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_79"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_le_int32_t_curry_1932"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_int32_t(ptr %0, i32 %"Builtin.$p_1930_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1930_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_79", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$neInt64_curry_3113"(ptr %0, ptr %"Builtin.$int64#_3114_0", ptr %"Builtin.$int64#_3115_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3114_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3114_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3115_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3115_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.neInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$gtString#_curry_3174"(ptr %0, ptr %"Builtin.$x_3175_0", ptr %"Builtin.$y_3176_0") {
  %2 = call ptr @Builtin.malgo_gt_string(ptr null, ptr %"Builtin.$x_3175_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_3176_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_80"(ptr %0, i64 %1) {
  %str_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %str_0 = load ptr, ptr %str_addr_0, align 8
  %3 = call ptr @"Primitive.$raw_let_20"(ptr null, ptr %str_0, i64 %1)
  ret ptr %3
}

define internal ptr @"Builtin.substring#"(ptr %0, ptr %"Builtin.$str_2207_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %str_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$str_2207_0", ptr %str_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_80", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_print_char(ptr %0, i8 %"Builtin.$p_2162_0") {
  %2 = call ptr @malgo_print_char(i8 %"Builtin.$p_2162_0")
  ret ptr %2
}

define internal i32 @"Builtin.$neDouble#_curry_2477"(ptr %0, double %"Builtin.$x_2478_0", double %"Builtin.$y_2479_0") {
  %2 = call ptr @Builtin.malgo_ne_double(ptr null, double %"Builtin.$x_2478_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_2479_0")
  ret i32 %7
}

define internal float @"Builtin.$divFloat#_curry_3838"(ptr %0, float %"Builtin.$x_3839_0", float %"Builtin.$y_3840_0") {
  %2 = call ptr @Builtin.malgo_div_float(ptr null, float %"Builtin.$x_3839_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call float %6(ptr %4, float %"Builtin.$y_3840_0")
  ret float %7
}

define internal ptr @"Builtin.$neFloat_curry_3073"(ptr %0, ptr %"Builtin.$float#_3074_0", ptr %"Builtin.$float#_3075_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3074_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3074_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3075_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3075_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.neFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$ltChar_curry_2913"(ptr %0, ptr %"Builtin.$char#_2914_0", ptr %"Builtin.$char#_2915_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2914_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2914_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2915_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2915_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.ltChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_81"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$geFloat#_curry_3474"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.geFloat#"(ptr %0, float %"Builtin.$x_3469_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_3469_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_81", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @Builtin.malgo_is_alphanum(ptr %0, i8 %"Builtin.$p_2126_0") {
  %2 = call i32 @malgo_is_alphanum(i8 %"Builtin.$p_2126_0")
  ret i32 %2
}

define internal i32 @"Builtin.$gtInt64#_curry_3206"(ptr %0, i64 %"Builtin.$x_3207_0", i64 %"Builtin.$y_3208_0") {
  %2 = call ptr @Builtin.malgo_gt_int64_t(ptr null, i64 %"Builtin.$x_3207_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_3208_0")
  ret i32 %7
}

define internal i32 @"Builtin.$malgo_le_string_curry_2112"(ptr %0, ptr %"Builtin.$p_2113_0", ptr %"Builtin.$p_2114_0") {
  %2 = call i32 @malgo_le_string(ptr %"Builtin.$p_2113_0", ptr %"Builtin.$p_2114_0")
  ret i32 %2
}

define internal i32 @"Builtin.$gtChar#_curry_3334"(ptr %0, i8 %"Builtin.$x_3335_0", i8 %"Builtin.$y_3336_0") {
  %2 = call ptr @Builtin.malgo_gt_char(ptr null, i8 %"Builtin.$x_3335_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_3336_0")
  ret i32 %7
}

define internal ptr @"Builtin.$substring#_curry_2215"(ptr %0, ptr %"Builtin.$str_2216_0", i64 %"Builtin.$start_2217_0", i64 %"Builtin.$end_2218_0") {
  %2 = call ptr @Builtin.malgo_substring(ptr null, ptr %"Builtin.$str_2216_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, i64 %"Builtin.$start_2217_0")
  %8 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 0
  %9 = load ptr, ptr %8, align 8
  %10 = getelementptr { ptr, ptr }, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr %11(ptr %9, i64 %"Builtin.$end_2218_0")
  ret ptr %12
}

define internal ptr @"Builtin.$mulDouble_curry_2613"(ptr %0, ptr %"Builtin.$double#_2614_0", ptr %"Builtin.$double#_2615_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2614_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2614_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2615_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2615_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.mulDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call double %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.Double#"(ptr null, double %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$eqString_curry_3596"(ptr %0, ptr %"Builtin.$string#_3597_0", ptr %"Builtin.$string#_3598_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3597_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3597_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3598_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3598_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.eqString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"Builtin.$addInt64#_curry_4000"(ptr %0, i64 %"Builtin.$x_4001_0", i64 %"Builtin.$y_4002_0") {
  %2 = call ptr @Builtin.malgo_add_int64_t(ptr null, i64 %"Builtin.$x_4001_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %"Builtin.$y_4002_0")
  ret i64 %7
}

define internal ptr @"Primitive.#let_closure_82"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$divInt32_curry_3822"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.divInt32(ptr %0, ptr %"Builtin.$int32#_3813_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_3813_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_82", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$addDouble_curry_4112"(ptr %0, ptr %"Builtin.$double#_4113_0", ptr %"Builtin.$double#_4114_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_4113_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_4113_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_4114_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_4114_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.addDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call double %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.Double#"(ptr null, double %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i64 @"Builtin.$malgo_div_int64_t_curry_1852"(ptr %0, i64 %"Builtin.$p_1853_0", i64 %"Builtin.$p_1854_0") {
  %2 = call i64 @malgo_div_int64_t(i64 %"Builtin.$p_1853_0", i64 %"Builtin.$p_1854_0")
  ret i64 %2
}

define internal ptr @Builtin.newline(ptr %0, ptr %"Builtin.$__2420_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Builtin.malgo_newline(ptr null, ptr %2)
  ret ptr %4
}

define internal i32 @Builtin.malgo_is_upper(ptr %0, i8 %"Builtin.$p_2125_0") {
  %2 = call i32 @malgo_is_upper(i8 %"Builtin.$p_2125_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_le_int32_t_curry_1932"(ptr %0, i32 %"Builtin.$p_1933_0", i32 %"Builtin.$p_1934_0") {
  %2 = call i32 @malgo_le_int32_t(i32 %"Builtin.$p_1933_0", i32 %"Builtin.$p_1934_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_83"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$ltInt64#_curry_2641"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltInt64#"(ptr %0, i64 %"Builtin.$x_2636_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_2636_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_83", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Primitive.#let_closure_84"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"Builtin.$malgo_mul_int64_t_curry_1846"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_mul_int64_t(ptr %0, i64 %"Builtin.$p_1844_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1844_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_84", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$malgo_mul_double_curry_1894"(ptr %0, double %"Builtin.$p_1895_0", double %"Builtin.$p_1896_0") {
  %2 = call double @malgo_mul_double(double %"Builtin.$p_1895_0", double %"Builtin.$p_1896_0")
  ret double %2
}

define internal ptr @"Builtin.$consString_curry_3918"(ptr %0, ptr %"Builtin.$char#_3919_0", ptr %"Builtin.$string#_3920_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3919_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3919_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3920_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3920_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.consString#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$mulInt64_curry_2517"(ptr %0, ptr %"Builtin.$int64#_2518_0", ptr %"Builtin.$int64#_2519_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2518_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2518_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2519_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2519_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.mulInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.Int64#"(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.False(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_85"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ne_int32_t_curry_1914"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_int32_t(ptr %0, i32 %"Builtin.$p_1912_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1912_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_85", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$ltDouble_curry_2933"(ptr %0, ptr %"Builtin.$double#_2934_0", ptr %"Builtin.$double#_2935_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2934_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2934_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2935_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2935_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.ltDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_86"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_lt_int32_t_curry_1920"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_int32_t(ptr %0, i32 %"Builtin.$p_1918_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1918_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_86", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @Builtin.malgo_is_lower(ptr %0, i8 %"Builtin.$p_2124_0") {
  %2 = call i32 @malgo_is_lower(i8 %"Builtin.$p_2124_0")
  ret i32 %2
}

define internal double @"Builtin.$malgo_div_double_curry_1900"(ptr %0, double %"Builtin.$p_1901_0", double %"Builtin.$p_1902_0") {
  %2 = call double @malgo_div_double(double %"Builtin.$p_1901_0", double %"Builtin.$p_1902_0")
  ret double %2
}

define internal i32 @"Builtin.$malgo_add_int32_t_curry_1810"(ptr %0, i32 %"Builtin.$p_1811_0", i32 %"Builtin.$p_1812_0") {
  %2 = call i32 @malgo_add_int32_t(i32 %"Builtin.$p_1811_0", i32 %"Builtin.$p_1812_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_87"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$geInt32_curry_3458"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geInt32(ptr %0, ptr %"Builtin.$int32#_3449_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_3449_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_87", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_88"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$eqFloat#_curry_3676"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqFloat#"(ptr %0, float %"Builtin.$x_3671_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_3671_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_88", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$leInt64#_curry_2720"(ptr %0, i64 %"Builtin.$x_2721_0", i64 %"Builtin.$y_2722_0") {
  %2 = call ptr @Builtin.malgo_le_int64_t(ptr null, i64 %"Builtin.$x_2721_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_2722_0")
  ret i32 %7
}

define internal ptr @"Builtin.$appendString_curry_3984"(ptr %0, ptr %"Builtin.$string#_3985_0", ptr %"Builtin.$string#_3986_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3985_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3985_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3986_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3986_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.appendString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call ptr %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.String#"(ptr null, ptr %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.isUpper#"(ptr %0, i8 %"Builtin.$x_2775_0") {
  %2 = call i32 @Builtin.malgo_is_upper(ptr null, i8 %"Builtin.$x_2775_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_89"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call ptr @"Builtin.$malgo_string_cons_curry_2136"(ptr null, i8 %p_0, ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.malgo_string_cons(ptr %0, i8 %"Builtin.$p_2134_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2134_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_89", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_eq_int32_t_curry_1908"(ptr %0, i32 %"Builtin.$p_1909_0", i32 %"Builtin.$p_1910_0") {
  %2 = call i32 @malgo_eq_int32_t(i32 %"Builtin.$p_1909_0", i32 %"Builtin.$p_1910_0")
  ret i32 %2
}

define internal i64 @"Primitive.#let_closure_90"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"Builtin.$divInt64#_curry_3774"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"Builtin.divInt64#"(ptr %0, i64 %"Builtin.$x_3769_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_3769_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_90", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_91"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$neChar#_curry_2489"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.neChar#"(ptr %0, i8 %"Builtin.$x_2484_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_2484_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_91", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.sqrtDouble(ptr %0, ptr %"Builtin.$double#_2394_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2394_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2394_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = call double @"Builtin.sqrtDouble#"(ptr null, double %6)
  %8 = call ptr @"Builtin.Double#"(ptr null, double %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$mulInt32#_curry_2533"(ptr %0, i32 %"Builtin.$x_2534_0", i32 %"Builtin.$y_2535_0") {
  %2 = call ptr @Builtin.malgo_mul_int32_t(ptr null, i32 %"Builtin.$x_2534_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_2535_0")
  ret i32 %7
}

define internal i32 @Builtin.malgo_is_digit(ptr %0, i8 %"Builtin.$p_2123_0") {
  %2 = call i32 @malgo_is_digit(i8 %"Builtin.$p_2123_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_92"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$neChar_curry_3033"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neChar(ptr %0, ptr %"Builtin.$char#_3024_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_3024_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_92", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Primitive.#let_closure_93"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$subDouble#_curry_2356"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"Builtin.subDouble#"(ptr %0, double %"Builtin.$x_2351_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_2351_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_93", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_94"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$eqInt64#_curry_3612"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqInt64#"(ptr %0, i64 %"Builtin.$x_3607_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_3607_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_94", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_95"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$neInt32_curry_3093"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neInt32(ptr %0, ptr %"Builtin.$int32#_3084_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_3084_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_95", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.isAlphanum#"(ptr %0, i8 %"Builtin.$x_3158_0") {
  %2 = call i32 @Builtin.malgo_is_alphanum(ptr null, i8 %"Builtin.$x_3158_0")
  ret i32 %2
}

define internal i32 @"Builtin.isLower#"(ptr %0, i8 %"Builtin.$x_3144_0") {
  %2 = call i32 @Builtin.malgo_is_lower(ptr null, i8 %"Builtin.$x_3144_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_eq_float_curry_1980"(ptr %0, float %"Builtin.$p_1981_0", float %"Builtin.$p_1982_0") {
  %2 = call i32 @malgo_eq_float(float %"Builtin.$p_1981_0", float %"Builtin.$p_1982_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_ge_int64_t_curry_1974"(ptr %0, i64 %"Builtin.$p_1975_0", i64 %"Builtin.$p_1976_0") {
  %2 = call i32 @malgo_ge_int64_t(i64 %"Builtin.$p_1975_0", i64 %"Builtin.$p_1976_0")
  ret i32 %2
}

define internal ptr @"Builtin.toStringInt32#"(ptr %0, i32 %"Builtin.$x_2179_0") {
  %2 = call ptr @Builtin.malgo_int32_t_to_string(ptr null, i32 %"Builtin.$x_2179_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_96"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_lt_float_curry_1992"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_float(ptr %0, float %"Builtin.$p_1990_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1990_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_96", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.isDigit#"(ptr %0, i8 %"Builtin.$x_3151_0") {
  %2 = call i32 @Builtin.malgo_is_digit(ptr null, i8 %"Builtin.$x_3151_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_97"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$geChar#_curry_3538"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.geChar#"(ptr %0, i8 %"Builtin.$x_3533_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_3533_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_97", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$consString#_curry_3902"(ptr %0, i8 %"Builtin.$c_3903_0", ptr %"Builtin.$cs_3904_0") {
  %2 = call ptr @Builtin.malgo_string_cons(ptr null, i8 %"Builtin.$c_3903_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Builtin.$cs_3904_0")
  ret ptr %7
}

define internal i32 @"Primitive.#let_closure_98"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$subInt32#_curry_2292"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.subInt32#"(ptr %0, i32 %"Builtin.$x_2287_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_2287_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_98", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$ltInt32#_curry_2653"(ptr %0, i32 %"Builtin.$x_2654_0", i32 %"Builtin.$y_2655_0") {
  %2 = call ptr @Builtin.malgo_lt_int32_t(ptr null, i32 %"Builtin.$x_2654_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_2655_0")
  ret i32 %7
}

define internal i64 @"Builtin.$subInt64#_curry_2260"(ptr %0, i64 %"Builtin.$x_2261_0", i64 %"Builtin.$y_2262_0") {
  %2 = call ptr @Builtin.malgo_sub_int64_t(ptr null, i64 %"Builtin.$x_2261_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %"Builtin.$y_2262_0")
  ret i64 %7
}

define internal ptr @Builtin.malgo_newline(ptr %0, ptr %"Builtin.$p_2161_0") {
  %2 = call ptr @malgo_newline(ptr %"Builtin.$p_2161_0")
  ret ptr %2
}

define internal double @"Primitive.#let_closure_99"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$addDouble#_curry_4096"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"Builtin.addDouble#"(ptr %0, double %"Builtin.$x_4091_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_4091_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_99", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_sub_int32_t_curry_1816"(ptr %0, i32 %"Builtin.$p_1817_0", i32 %"Builtin.$p_1818_0") {
  %2 = call i32 @malgo_sub_int32_t(i32 %"Builtin.$p_1817_0", i32 %"Builtin.$p_1818_0")
  ret i32 %2
}

define internal ptr @"Builtin.$gtFloat_curry_3286"(ptr %0, ptr %"Builtin.$float#_3287_0", ptr %"Builtin.$float#_3288_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3287_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3287_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3288_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3288_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.gtFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_le_double_curry_2040"(ptr %0, double %"Builtin.$p_2041_0", double %"Builtin.$p_2042_0") {
  %2 = call i32 @malgo_le_double(double %"Builtin.$p_2041_0", double %"Builtin.$p_2042_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_100"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_ne_double_curry_2022"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_double(ptr %0, double %"Builtin.$p_2020_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2020_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_100", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.toStringDouble#"(ptr %0, double %"Builtin.$x_2193_0") {
  %2 = call ptr @Builtin.malgo_double_to_string(ptr null, double %"Builtin.$x_2193_0")
  ret ptr %2
}

define internal ptr @"Builtin.$mulInt32_curry_2549"(ptr %0, ptr %"Builtin.$int32#_2550_0", ptr %"Builtin.$int32#_2551_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2550_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2550_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2551_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2551_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.mulInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"Builtin.$malgo_add_double_curry_1882"(ptr %0, double %"Builtin.$p_1883_0", double %"Builtin.$p_1884_0") {
  %2 = call double @malgo_add_double(double %"Builtin.$p_1883_0", double %"Builtin.$p_1884_0")
  ret double %2
}

define internal ptr @"Builtin.$geFloat_curry_3490"(ptr %0, ptr %"Builtin.$float#_3491_0", ptr %"Builtin.$float#_3492_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3491_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3491_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3492_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3492_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.geFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$neString#_curry_2429"(ptr %0, ptr %"Builtin.$x_2430_0", ptr %"Builtin.$y_2431_0") {
  %2 = call ptr @Builtin.malgo_ne_string(ptr null, ptr %"Builtin.$x_2430_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_2431_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_101"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$geString#_curry_3378"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.geString#"(ptr %0, ptr %"Builtin.$x_3373_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_3373_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_101", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Primitive.#let_closure_102"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"Builtin.$divFloat#_curry_3838"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"Builtin.divFloat#"(ptr %0, float %"Builtin.$x_3833_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_3833_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_102", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$geDouble_curry_3522"(ptr %0, ptr %"Builtin.$double#_3523_0", ptr %"Builtin.$double#_3524_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3523_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3523_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3524_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3524_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.geDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_ne_int64_t_curry_1950"(ptr %0, i64 %"Builtin.$p_1951_0", i64 %"Builtin.$p_1952_0") {
  %2 = call i32 @malgo_ne_int64_t(i64 %"Builtin.$p_1951_0", i64 %"Builtin.$p_1952_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_103"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call ptr @"Primitive.$raw_let_19"(ptr null, ptr %p_0, i64 %1)
  ret ptr %3
}

define internal ptr @Builtin.malgo_substring(ptr %0, ptr %"Builtin.$p_2146_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2146_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_103", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_104"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$addInt32_curry_4048"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.addInt32(ptr %0, ptr %"Builtin.$int32#_4039_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_4039_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_104", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_105"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$gtInt64#_curry_3206"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtInt64#"(ptr %0, i64 %"Builtin.$x_3201_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_3201_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_105", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_106"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$ltString#_curry_2629"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltString#"(ptr %0, ptr %"Builtin.$x_2624_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_2624_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_106", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i8 @"Builtin.$malgo_string_at_curry_2130"(ptr %0, i64 %"Builtin.$p_2131_0", ptr %"Builtin.$p_2132_0") {
  %2 = call i8 @malgo_string_at(i64 %"Builtin.$p_2131_0", ptr %"Builtin.$p_2132_0")
  ret i8 %2
}

define internal ptr @"Builtin.isTrue#"(ptr %0, i32 %"Builtin.$unboxed_2777_0") {
  switch i32 %"Builtin.$unboxed_2777_0", label %switch-unboxed_default_0 [
    i32 1, label %switch-unboxed_branch_1_i32_0
  ]

switch-unboxed_branch_1_i32_0:                    ; preds = %1
  %2 = call ptr @Builtin.True(ptr null)
  ret ptr %2

switch-unboxed_default_0:                         ; preds = %1
  %3 = call ptr @Builtin.False(ptr null)
  ret ptr %3
}

define internal ptr @"Builtin.$divInt64_curry_3790"(ptr %0, ptr %"Builtin.$int64#_3791_0", ptr %"Builtin.$int64#_3792_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3791_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3791_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3792_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3792_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.divInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.Int64#"(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_ge_double_curry_2046"(ptr %0, double %"Builtin.$p_2047_0", double %"Builtin.$p_2048_0") {
  %2 = call i32 @malgo_ge_double(double %"Builtin.$p_2047_0", double %"Builtin.$p_2048_0")
  ret i32 %2
}

define internal ptr @Builtin.malgo_int64_t_to_string(ptr %0, i64 %"Builtin.$p_2156_0") {
  %2 = call ptr @malgo_int64_t_to_string(i64 %"Builtin.$p_2156_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_107"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_lt_string_curry_2100"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_string(ptr %0, ptr %"Builtin.$p_2098_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2098_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_107", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Builtin.$malgo_add_float_curry_1858"(ptr %0, float %"Builtin.$p_1859_0", float %"Builtin.$p_1860_0") {
  %2 = call float @malgo_add_float(float %"Builtin.$p_1859_0", float %"Builtin.$p_1860_0")
  ret float %2
}

define internal ptr @"Primitive.#let_closure_108"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$mulInt32_curry_2549"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.mulInt32(ptr %0, ptr %"Builtin.$int32#_2540_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_2540_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_108", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_eq_string_curry_2088"(ptr %0, ptr %"Builtin.$p_2089_0", ptr %"Builtin.$p_2090_0") {
  %2 = call i32 @malgo_eq_string(ptr %"Builtin.$p_2089_0", ptr %"Builtin.$p_2090_0")
  ret i32 %2
}

define internal i64 @"Primitive.#let_closure_109"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"Builtin.$malgo_sub_int64_t_curry_1840"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_sub_int64_t(ptr %0, i64 %"Builtin.$p_1838_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1838_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_109", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$addDouble#_curry_4096"(ptr %0, double %"Builtin.$x_4097_0", double %"Builtin.$y_4098_0") {
  %2 = call ptr @Builtin.malgo_add_double(ptr null, double %"Builtin.$x_4097_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"Builtin.$y_4098_0")
  ret double %7
}

define internal i32 @"Primitive.#let_closure_110"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$neInt32#_curry_2453"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.neInt32#"(ptr %0, i32 %"Builtin.$x_2448_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_2448_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_110", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$subFloat_curry_2340"(ptr %0, ptr %"Builtin.$float#_2341_0", ptr %"Builtin.$float#_2342_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2341_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2341_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_2342_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_2342_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.subFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call float %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.Float#"(ptr null, float %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_111"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_ge_double_curry_2046"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_double(ptr %0, double %"Builtin.$p_2044_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2044_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_111", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$eqInt64#_curry_3612"(ptr %0, i64 %"Builtin.$x_3613_0", i64 %"Builtin.$y_3614_0") {
  %2 = call ptr @Builtin.malgo_eq_int64_t(ptr null, i64 %"Builtin.$x_3613_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_3614_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_112"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$leString_curry_2893"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leString(ptr %0, ptr %"Builtin.$string#_2884_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_2884_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_112", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_113"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$leFloat#_curry_2744"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.leFloat#"(ptr %0, float %"Builtin.$x_2739_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_2739_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_113", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_114"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_eq_int64_t_curry_1944"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_int64_t(ptr %0, i64 %"Builtin.$p_1942_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1942_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_114", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_float_to_string(ptr %0, float %"Builtin.$p_2157_0") {
  %2 = call ptr @malgo_float_to_string(float %"Builtin.$p_2157_0")
  ret ptr %2
}

define internal i32 @"Builtin.$leChar#_curry_2768"(ptr %0, i8 %"Builtin.$x_2769_0", i8 %"Builtin.$y_2770_0") {
  %2 = call ptr @Builtin.malgo_le_char(ptr null, i8 %"Builtin.$x_2769_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_2770_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_115"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_le_double_curry_2040"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_double(ptr %0, double %"Builtin.$p_2038_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2038_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_115", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_116"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_le_float_curry_2004"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_float(ptr %0, float %"Builtin.$p_2002_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_2002_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_116", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Builtin.$divInt64#_curry_3774"(ptr %0, i64 %"Builtin.$x_3775_0", i64 %"Builtin.$y_3776_0") {
  %2 = call ptr @Builtin.malgo_div_int64_t(ptr null, i64 %"Builtin.$x_3775_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %"Builtin.$y_3776_0")
  ret i64 %7
}

define internal float @Builtin.sqrtf(ptr %0, float %"Builtin.$p_1904_0") {
  %2 = call float @sqrtf(float %"Builtin.$p_1904_0")
  ret float %2
}

define internal ptr @"Builtin.$malgo_string_append_curry_2142"(ptr %0, ptr %"Builtin.$p_2143_0", ptr %"Builtin.$p_2144_0") {
  %2 = call ptr @malgo_string_append(ptr %"Builtin.$p_2143_0", ptr %"Builtin.$p_2144_0")
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_gt_string_curry_2106"(ptr %0, ptr %"Builtin.$p_2107_0", ptr %"Builtin.$p_2108_0") {
  %2 = call i32 @malgo_gt_string(ptr %"Builtin.$p_2107_0", ptr %"Builtin.$p_2108_0")
  ret i32 %2
}

define internal double @"Primitive.#let_closure_117"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$mulDouble#_curry_2597"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"Builtin.mulDouble#"(ptr %0, double %"Builtin.$x_2592_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_2592_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_117", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_118"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_gt_int64_t_curry_1962"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_int64_t(ptr %0, i64 %"Builtin.$p_1960_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1960_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_118", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_119"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$addDouble_curry_4112"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.addDouble(ptr %0, ptr %"Builtin.$double#_4103_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_4103_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_119", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_ge_int32_t_curry_1938"(ptr %0, i32 %"Builtin.$p_1939_0", i32 %"Builtin.$p_1940_0") {
  %2 = call i32 @malgo_ge_int32_t(i32 %"Builtin.$p_1939_0", i32 %"Builtin.$p_1940_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_120"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$geInt64_curry_3426"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geInt64(ptr %0, ptr %"Builtin.$int64#_3417_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3417_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_120", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @Builtin.sqrt(ptr %0, double %"Builtin.$p_1905_0") {
  %2 = call double @sqrt(double %"Builtin.$p_1905_0")
  ret double %2
}

define internal ptr @"Primitive.#let_closure_121"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Primitive.$raw_let_21"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.substring(ptr %0, ptr %"Builtin.$string#_2226_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_2226_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_121", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_122"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$leInt32#_curry_2732"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.leInt32#"(ptr %0, i32 %"Builtin.$x_2727_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_2727_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_122", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_eq_int64_t_curry_1944"(ptr %0, i64 %"Builtin.$p_1945_0", i64 %"Builtin.$p_1946_0") {
  %2 = call i32 @malgo_eq_int64_t(i64 %"Builtin.$p_1945_0", i64 %"Builtin.$p_1946_0")
  ret i32 %2
}

define internal i64 @Builtin.malgo_string_length(ptr %0, ptr %"Builtin.$p_2127_0") {
  %2 = call i64 @malgo_string_length(ptr %"Builtin.$p_2127_0")
  ret i64 %2
}

define internal ptr @"Builtin.Double#"(ptr %0, double %"Builtin.$p_1798_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { double } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { double } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { double } }, ptr %2, i32 0, i32 1, i32 0
  store double %"Builtin.$p_1798_0", ptr %4, align 8
  ret ptr %2
}

define internal double @"Builtin.$divDouble#_curry_3870"(ptr %0, double %"Builtin.$x_3871_0", double %"Builtin.$y_3872_0") {
  %2 = call ptr @Builtin.malgo_div_double(ptr null, double %"Builtin.$x_3871_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"Builtin.$y_3872_0")
  ret double %7
}

define internal ptr @"Builtin.double#"(ptr %0, double %"Builtin.$x_3767_0") {
  %2 = call ptr @"Builtin.Double#"(ptr null, double %"Builtin.$x_3767_0")
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_gt_char_curry_2070"(ptr %0, i8 %"Builtin.$p_2071_0", i8 %"Builtin.$p_2072_0") {
  %2 = call i32 @malgo_gt_char(i8 %"Builtin.$p_2071_0", i8 %"Builtin.$p_2072_0")
  ret i32 %2
}

define internal i32 @"Primitive.#let_closure_123"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_ne_string_curry_2094"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_string(ptr %0, ptr %"Builtin.$p_2092_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2092_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_123", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_124"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$addInt32#_curry_4032"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.addInt32#"(ptr %0, i32 %"Builtin.$x_4027_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_4027_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_124", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_125"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltFloat_curry_2953"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltFloat(ptr %0, ptr %"Builtin.$float#_2944_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_2944_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_125", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_126"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ne_int64_t_curry_1950"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_int64_t(ptr %0, i64 %"Builtin.$p_1948_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1948_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_126", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Primitive.#let_closure_127"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"Builtin.$addFloat#_curry_4064"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"Builtin.addFloat#"(ptr %0, float %"Builtin.$x_4059_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_4059_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_127", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_le_float_curry_2004"(ptr %0, float %"Builtin.$p_2005_0", float %"Builtin.$p_2006_0") {
  %2 = call i32 @malgo_le_float(float %"Builtin.$p_2005_0", float %"Builtin.$p_2006_0")
  ret i32 %2
}

define internal ptr @"Builtin.$geString_curry_3394"(ptr %0, ptr %"Builtin.$string#_3395_0", ptr %"Builtin.$string#_3396_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3395_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3395_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3396_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3396_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.geString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_128"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$divInt32#_curry_3806"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.divInt32#"(ptr %0, i32 %"Builtin.$x_3801_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_3801_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_128", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i8 @"Primitive.#let_closure_129"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i8 @"Builtin.$malgo_string_at_curry_2130"(ptr null, i64 %p_0, ptr %1)
  ret i8 %3
}

define internal ptr @Builtin.malgo_string_at(ptr %0, i64 %"Builtin.$p_2128_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_2128_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_129", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$addFloat_curry_4080"(ptr %0, ptr %"Builtin.$float#_4081_0", ptr %"Builtin.$float#_4082_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_4081_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_4081_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_4082_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_4082_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.addFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call float %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.Float#"(ptr null, float %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_ne_float_curry_1986"(ptr %0, float %"Builtin.$p_1987_0", float %"Builtin.$p_1988_0") {
  %2 = call i32 @malgo_ne_float(float %"Builtin.$p_1987_0", float %"Builtin.$p_1988_0")
  ret i32 %2
}

define internal i32 @"Builtin.$leInt32#_curry_2732"(ptr %0, i32 %"Builtin.$x_2733_0", i32 %"Builtin.$y_2734_0") {
  %2 = call ptr @Builtin.malgo_le_int32_t(ptr null, i32 %"Builtin.$x_2733_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_2734_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_130"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_ne_char_curry_2058"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_char(ptr %0, i8 %"Builtin.$p_2056_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2056_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_130", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_131"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtString_curry_3190"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtString(ptr %0, ptr %"Builtin.$string#_3181_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3181_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_131", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_le_char_curry_2076"(ptr %0, i8 %"Builtin.$p_2077_0", i8 %"Builtin.$p_2078_0") {
  %2 = call i32 @malgo_le_char(i8 %"Builtin.$p_2077_0", i8 %"Builtin.$p_2078_0")
  ret i32 %2
}

define internal float @"Primitive.#let_closure_132"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"Builtin.$malgo_add_float_curry_1858"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @Builtin.malgo_add_float(ptr %0, float %"Builtin.$p_1856_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1856_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_132", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_133"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_sub_int32_t_curry_1816"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_sub_int32_t(ptr %0, i32 %"Builtin.$p_1814_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1814_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_133", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_double_to_string(ptr %0, double %"Builtin.$p_2158_0") {
  %2 = call ptr @malgo_double_to_string(double %"Builtin.$p_2158_0")
  ret ptr %2
}

define internal double @"Primitive.#let_closure_134"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"Builtin.$malgo_sub_double_curry_1888"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_sub_double(ptr %0, double %"Builtin.$p_1886_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1886_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_134", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_135"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$mulDouble_curry_2613"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.mulDouble(ptr %0, ptr %"Builtin.$double#_2604_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_2604_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_135", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_int32_t_to_string(ptr %0, i32 %"Builtin.$p_2155_0") {
  %2 = call ptr @malgo_int32_t_to_string(i32 %"Builtin.$p_2155_0")
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_136"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$consString_curry_3918"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.consString(ptr %0, ptr %"Builtin.$char#_3909_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_3909_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_136", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_137"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$leInt32_curry_2853"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leInt32(ptr %0, ptr %"Builtin.$int32#_2844_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_2844_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_137", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_138"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtDouble_curry_3318"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtDouble(ptr %0, ptr %"Builtin.$double#_3309_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_3309_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_138", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_ne_int32_t_curry_1914"(ptr %0, i32 %"Builtin.$p_1915_0", i32 %"Builtin.$p_1916_0") {
  %2 = call i32 @malgo_ne_int32_t(i32 %"Builtin.$p_1915_0", i32 %"Builtin.$p_1916_0")
  ret i32 %2
}

define internal ptr @"Builtin.$gtInt32_curry_3254"(ptr %0, ptr %"Builtin.$int32#_3255_0", ptr %"Builtin.$int32#_3256_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3255_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3255_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3256_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3256_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.gtInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$atString_curry_3952"(ptr %0, ptr %"Builtin.$int64#_3953_0", ptr %"Builtin.$string#_3954_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3953_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3953_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3954_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3954_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.atString#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i8 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.Char#"(ptr null, i8 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_139"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$divFloat_curry_3854"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.divFloat(ptr %0, ptr %"Builtin.$float#_3845_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_3845_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_139", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_140"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_le_int64_t_curry_1968"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_int64_t(ptr %0, i64 %"Builtin.$p_1966_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1966_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_140", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Primitive.#let_closure_141"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"Builtin.$malgo_div_int64_t_curry_1852"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_div_int64_t(ptr %0, i64 %"Builtin.$p_1850_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1850_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_141", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Builtin.$mulFloat#_curry_2565"(ptr %0, float %"Builtin.$x_2566_0", float %"Builtin.$y_2567_0") {
  %2 = call ptr @Builtin.malgo_mul_float(ptr null, float %"Builtin.$x_2566_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call float %6(ptr %4, float %"Builtin.$y_2567_0")
  ret float %7
}

define internal ptr @"Builtin.Int32#"(ptr %0, i32 %"Builtin.$p_1792_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i32 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i32 } }, ptr %2, i32 0, i32 1, i32 0
  store i32 %"Builtin.$p_1792_0", ptr %4, align 4
  ret ptr %2
}

define internal i64 @"Primitive.#let_closure_142"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i64 @"Builtin.$malgo_add_int64_t_curry_1834"(ptr null, i64 %p_0, i64 %1)
  ret i64 %3
}

define internal ptr @Builtin.malgo_add_int64_t(ptr %0, i64 %"Builtin.$p_1832_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1832_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_142", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.int32#"(ptr %0, i32 %"Builtin.$x_3167_0") {
  %2 = call ptr @"Builtin.Int32#"(ptr null, i32 %"Builtin.$x_3167_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_143"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_gt_string_curry_2106"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_string(ptr %0, ptr %"Builtin.$p_2104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2104_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_143", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$neInt32_curry_3093"(ptr %0, ptr %"Builtin.$int32#_3094_0", ptr %"Builtin.$int32#_3095_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3094_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3094_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3095_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3095_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.neInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"Primitive.#let_closure_144"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call float @"Builtin.$malgo_mul_float_curry_1870"(ptr null, float %p_0, float %1)
  ret float %3
}

define internal ptr @Builtin.malgo_mul_float(ptr %0, float %"Builtin.$p_1868_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1868_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_144", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$ltChar#_curry_2689"(ptr %0, i8 %"Builtin.$x_2690_0", i8 %"Builtin.$y_2691_0") {
  %2 = call ptr @Builtin.malgo_lt_char(ptr null, i8 %"Builtin.$x_2690_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_2691_0")
  ret i32 %7
}

define internal ptr @"Builtin.$neString_curry_3133"(ptr %0, ptr %"Builtin.$string#_3134_0", ptr %"Builtin.$string#_3135_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3134_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3134_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3135_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3135_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.neString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.lengthString(ptr %0, ptr %"Builtin.$string#_2698_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2698_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2698_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 @"Builtin.lengthString#"(ptr null, ptr %6)
  %8 = call ptr @"Builtin.Int64#"(ptr null, i64 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.isDigit(ptr %0, ptr %"Builtin.$char#_3153_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3153_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3153_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call i32 @"Builtin.isDigit#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.isTrue#"(ptr null, i32 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_145"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$leString#_curry_2708"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.leString#"(ptr %0, ptr %"Builtin.$x_2703_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_2703_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_145", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_146"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$gtString#_curry_3174"(ptr null, ptr %x_0, ptr %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtString#"(ptr %0, ptr %"Builtin.$x_3169_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_3169_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_146", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$geInt64_curry_3426"(ptr %0, ptr %"Builtin.$int64#_3427_0", ptr %"Builtin.$int64#_3428_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3427_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3427_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3428_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3428_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.geInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.malgo_flush(ptr %0, ptr %"Builtin.$p_2164_0") {
  %2 = call ptr @malgo_flush(ptr %"Builtin.$p_2164_0")
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_147"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$subFloat_curry_2340"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.subFloat(ptr %0, ptr %"Builtin.$float#_2331_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_2331_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_147", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_148"(ptr %0, i64 %1) {
  %p_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %p_0 = load i64, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_lt_int64_t_curry_1956"(ptr null, i64 %p_0, i64 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_int64_t(ptr %0, i64 %"Builtin.$p_1954_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$p_1954_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_148", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Primitive.#let_closure_149"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call float @"Builtin.$subFloat#_curry_2324"(ptr null, float %x_0, float %1)
  ret float %3
}

define internal ptr @"Builtin.subFloat#"(ptr %0, float %"Builtin.$x_2319_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_2319_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_149", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_150"(ptr %0, ptr %1) {
  %p_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %p_0 = load ptr, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_eq_string_curry_2088"(ptr null, ptr %p_0, ptr %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_string(ptr %0, ptr %"Builtin.$p_2086_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$p_2086_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_150", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$gtDouble#_curry_3302"(ptr %0, double %"Builtin.$x_3303_0", double %"Builtin.$y_3304_0") {
  %2 = call ptr @Builtin.malgo_gt_double(ptr null, double %"Builtin.$x_3303_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_3304_0")
  ret i32 %7
}

define internal ptr @Builtin.not(ptr %0, ptr %"Builtin.$true_2419_0") {
  %2 = getelementptr { i8, {} }, ptr %"Builtin.$true_2419_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %switch_branch_Builtin.False_0
    i8 1, label %switch_branch_Builtin.True_0
  ]

switch_branch_Builtin.False_0:                    ; preds = %1
  %4 = getelementptr { i8, {} }, ptr %"Builtin.$true_2419_0", i32 0, i32 1
  %5 = call ptr @Builtin.True(ptr null)
  ret ptr %5

switch_branch_Builtin.True_0:                     ; preds = %1
  %6 = getelementptr { i8, {} }, ptr %"Builtin.$true_2419_0", i32 0, i32 1
  %7 = call ptr @Builtin.False(ptr null)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_151"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %"int64#_addr_0" = getelementptr { ptr, ptr }, ptr %0, i32 0, i32 1
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$substring_curry_2239"(ptr null, ptr %"string#_0", ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @"Primitive.$raw_let_21"(ptr %0, ptr %"Builtin.$string#_2226_0", ptr %"Builtin.$int64#_2227_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_2226_0", ptr %"string#_0", align 8
  %"int64#_0" = getelementptr { ptr, ptr }, ptr %let_capture_0, i32 0, i32 1
  store ptr %"Builtin.$int64#_2227_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_151", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_152"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_gt_int32_t_curry_1926"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_gt_int32_t(ptr %0, i32 %"Builtin.$p_1924_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1924_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_152", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_153"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$gtFloat_curry_3286"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.gtFloat(ptr %0, ptr %"Builtin.$float#_3277_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_3277_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_153", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.malgo_panic(ptr %0, ptr %"Builtin.$p_1806_0") {
  %2 = call ptr @malgo_panic(ptr %"Builtin.$p_1806_0")
  ret ptr %2
}

define internal i32 @"Builtin.$geDouble#_curry_3506"(ptr %0, double %"Builtin.$x_3507_0", double %"Builtin.$y_3508_0") {
  %2 = call ptr @Builtin.malgo_ge_double(ptr null, double %"Builtin.$x_3507_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, double %"Builtin.$y_3508_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_154"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$leInt64_curry_2873"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.leInt64(ptr %0, ptr %"Builtin.$int64#_2864_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_2864_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_154", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$leString_curry_2893"(ptr %0, ptr %"Builtin.$string#_2894_0", ptr %"Builtin.$string#_2895_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2894_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2894_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2895_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2895_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.leString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_155"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_lt_char_curry_2064"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_lt_char(ptr %0, i8 %"Builtin.$p_2062_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2062_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_155", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_156"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltString_curry_3013"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltString(ptr %0, ptr %"Builtin.$string#_3004_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3004_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_156", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_lt_int32_t_curry_1920"(ptr %0, i32 %"Builtin.$p_1921_0", i32 %"Builtin.$p_1922_0") {
  %2 = call i32 @malgo_lt_int32_t(i32 %"Builtin.$p_1921_0", i32 %"Builtin.$p_1922_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_157"(ptr %0, ptr %1) {
  %"double#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"double#_0" = load ptr, ptr %"double#_addr_0", align 8
  %3 = call ptr @"Builtin.$geDouble_curry_3522"(ptr null, ptr %"double#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geDouble(ptr %0, ptr %"Builtin.$double#_3513_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"double#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$double#_3513_0", ptr %"double#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_157", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$subDouble#_curry_2356"(ptr %0, double %"Builtin.$x_2357_0", double %"Builtin.$y_2358_0") {
  %2 = call ptr @Builtin.malgo_sub_double(ptr null, double %"Builtin.$x_2357_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call double %6(ptr %4, double %"Builtin.$y_2358_0")
  ret double %7
}

define internal ptr @"Builtin.$gtChar_curry_3350"(ptr %0, ptr %"Builtin.$char#_3351_0", ptr %"Builtin.$char#_3352_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3351_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3351_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3352_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3352_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.gtChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_158"(ptr %0, i8 %1) {
  %x_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %x_0 = load i8, ptr %x_addr_0, align 1
  %3 = call i32 @"Builtin.$leChar#_curry_2768"(ptr null, i8 %x_0, i8 %1)
  ret i32 %3
}

define internal ptr @"Builtin.leChar#"(ptr %0, i8 %"Builtin.$x_2763_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$x_2763_0", ptr %x_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_158", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$gtInt64_curry_3222"(ptr %0, ptr %"Builtin.$int64#_3223_0", ptr %"Builtin.$int64#_3224_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3223_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3223_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_3224_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_3224_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.gtInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal double @"Primitive.#let_closure_159"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call double @"Builtin.$malgo_add_double_curry_1882"(ptr null, double %p_0, double %1)
  ret double %3
}

define internal ptr @Builtin.malgo_add_double(ptr %0, double %"Builtin.$p_1880_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_1880_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_159", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Builtin.$malgo_sub_float_curry_1864"(ptr %0, float %"Builtin.$p_1865_0", float %"Builtin.$p_1866_0") {
  %2 = call float @malgo_sub_float(float %"Builtin.$p_1865_0", float %"Builtin.$p_1866_0")
  ret float %2
}

define internal i64 @"Builtin.lengthString#"(ptr %0, ptr %"Builtin.$x_2696_0") {
  %2 = call i64 @Builtin.malgo_string_length(ptr null, ptr %"Builtin.$x_2696_0")
  ret i64 %2
}

define internal i32 @"Primitive.#let_closure_160"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ne_float_curry_1986"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ne_float(ptr %0, float %"Builtin.$p_1984_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_1984_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_160", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_161"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqInt32_curry_3660"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqInt32(ptr %0, ptr %"Builtin.$int32#_3651_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_3651_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_161", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$geChar_curry_3554"(ptr %0, ptr %"Builtin.$char#_3555_0", ptr %"Builtin.$char#_3556_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3555_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3555_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3556_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3556_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.geChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$leChar_curry_2793"(ptr %0, ptr %"Builtin.$char#_2794_0", ptr %"Builtin.$char#_2795_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2794_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2794_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2795_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_1"
  ]

"switch_branch_Builtin.Char#_1":                  ; preds = %"switch_branch_Builtin.Char#_0"
  %9 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2795_0", i32 0, i32 1
  %10 = getelementptr { i8 }, ptr %9, i32 0, i32 0
  %11 = load i8, ptr %10, align 1
  %12 = call ptr @"Builtin.leChar#"(ptr null, i8 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i8 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Char#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.$gtString_curry_3190"(ptr %0, ptr %"Builtin.$string#_3191_0", ptr %"Builtin.$string#_3192_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3191_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3191_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_3192_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_1"
  ]

"switch_branch_Builtin.String#_1":                ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_3192_0", i32 0, i32 1
  %10 = getelementptr { ptr }, ptr %9, i32 0, i32 0
  %11 = load ptr, ptr %10, align 8
  %12 = call ptr @"Builtin.gtString#"(ptr null, ptr %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, ptr %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i8 @"Primitive.#let_closure_162"(ptr %0, ptr %1) {
  %i_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %i_0 = load i64, ptr %i_addr_0, align 4
  %3 = call i8 @"Builtin.$atString#_curry_3936"(ptr null, i64 %i_0, ptr %1)
  ret i8 %3
}

define internal ptr @"Builtin.atString#"(ptr %0, i64 %"Builtin.$i_3931_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %i_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$i_3931_0", ptr %i_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_162", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.flush(ptr %0, ptr %"Builtin.$__3565_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Builtin.malgo_flush(ptr null, ptr %2)
  ret ptr %4
}

define internal i32 @"Builtin.$malgo_lt_float_curry_1992"(ptr %0, float %"Builtin.$p_1993_0", float %"Builtin.$p_1994_0") {
  %2 = call i32 @malgo_lt_float(float %"Builtin.$p_1993_0", float %"Builtin.$p_1994_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_ge_string_curry_2118"(ptr %0, ptr %"Builtin.$p_2119_0", ptr %"Builtin.$p_2120_0") {
  %2 = call i32 @malgo_ge_string(ptr %"Builtin.$p_2119_0", ptr %"Builtin.$p_2120_0")
  ret i32 %2
}

define internal ptr @Builtin.toStringChar(ptr %0, ptr %"Builtin.$char#_2202_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_2202_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_2202_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call ptr @"Builtin.toStringChar#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$ltString#_curry_2629"(ptr %0, ptr %"Builtin.$x_2630_0", ptr %"Builtin.$y_2631_0") {
  %2 = call ptr @Builtin.malgo_lt_string(ptr null, ptr %"Builtin.$x_2630_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_2631_0")
  ret i32 %7
}

define internal i32 @"Builtin.ordChar#"(ptr %0, i8 %"Builtin.$x_2412_0") {
  %2 = call i32 @Builtin.malgo_char_ord(ptr null, i8 %"Builtin.$x_2412_0")
  ret i32 %2
}

define internal ptr @"Builtin.$leInt64_curry_2873"(ptr %0, ptr %"Builtin.$int64#_2874_0", ptr %"Builtin.$int64#_2875_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2874_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2874_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2875_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2875_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.leInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_163"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$ltInt32#_curry_2653"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltInt32#"(ptr %0, i32 %"Builtin.$x_2648_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_2648_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_163", ptr %let_func_0, align 8
  ret ptr %2
}

define internal float @"Builtin.$addFloat#_curry_4064"(ptr %0, float %"Builtin.$x_4065_0", float %"Builtin.$y_4066_0") {
  %2 = call ptr @Builtin.malgo_add_float(ptr null, float %"Builtin.$x_4065_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call float %6(ptr %4, float %"Builtin.$y_4066_0")
  ret float %7
}

define internal i32 @"Builtin.$addInt32#_curry_4032"(ptr %0, i32 %"Builtin.$x_4033_0", i32 %"Builtin.$y_4034_0") {
  %2 = call ptr @Builtin.malgo_add_int32_t(ptr null, i32 %"Builtin.$x_4033_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_4034_0")
  ret i32 %7
}

define internal i32 @"Builtin.$eqInt32#_curry_3644"(ptr %0, i32 %"Builtin.$x_3645_0", i32 %"Builtin.$y_3646_0") {
  %2 = call ptr @Builtin.malgo_eq_int32_t(ptr null, i32 %"Builtin.$x_3645_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_3646_0")
  ret i32 %7
}

define internal ptr @"Primitive.#let_closure_164"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$addFloat_curry_4080"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.addFloat(ptr %0, ptr %"Builtin.$float#_4071_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_4071_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_164", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_165"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ge_int32_t_curry_1938"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_int32_t(ptr %0, i32 %"Builtin.$p_1936_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1936_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_165", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$subInt64_curry_2276"(ptr %0, ptr %"Builtin.$int64#_2277_0", ptr %"Builtin.$int64#_2278_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2277_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2277_0", i32 0, i32 1
  %5 = getelementptr { i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5, align 4
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2278_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2278_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = call ptr @"Builtin.subInt64#"(ptr null, i64 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i64 %16(ptr %14, i64 %11)
  %18 = call ptr @"Builtin.Int64#"(ptr null, i64 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_166"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$eqDouble#_curry_3708"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqDouble#"(ptr %0, double %"Builtin.$x_3703_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_3703_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_166", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Primitive.#let_closure_167"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"Builtin.$addInt64#_curry_4000"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"Builtin.addInt64#"(ptr %0, i64 %"Builtin.$x_3995_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_3995_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_167", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @Builtin.panic(ptr %0, ptr %"Builtin.$string#_2409_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2409_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2409_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr @Builtin.malgo_panic(ptr null, ptr %6)
  ret ptr %7

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_168"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$leDouble#_curry_2756"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.leDouble#"(ptr %0, double %"Builtin.$x_2751_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_2751_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_168", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_169"(ptr %0, i8 %1) {
  %p_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %p_0 = load i8, ptr %p_addr_0, align 1
  %3 = call i32 @"Builtin.$malgo_le_char_curry_2076"(ptr null, i8 %p_0, i8 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_le_char(ptr %0, i8 %"Builtin.$p_2074_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$p_2074_0", ptr %p_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_169", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$leFloat#_curry_2744"(ptr %0, float %"Builtin.$x_2745_0", float %"Builtin.$y_2746_0") {
  %2 = call ptr @Builtin.malgo_le_float(ptr null, float %"Builtin.$x_2745_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_2746_0")
  ret i32 %7
}

define internal double @"Primitive.#let_closure_170"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call double @"Builtin.$divDouble#_curry_3870"(ptr null, double %x_0, double %1)
  ret double %3
}

define internal ptr @"Builtin.divDouble#"(ptr %0, double %"Builtin.$x_3865_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_3865_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_170", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$subInt32_curry_2308"(ptr %0, ptr %"Builtin.$int32#_2309_0", ptr %"Builtin.$int32#_2310_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2309_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2309_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2310_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2310_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.subInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal float @"Builtin.$malgo_div_float_curry_1876"(ptr %0, float %"Builtin.$p_1877_0", float %"Builtin.$p_1878_0") {
  %2 = call float @malgo_div_float(float %"Builtin.$p_1877_0", float %"Builtin.$p_1878_0")
  ret float %2
}

define internal ptr @Builtin.isLower(ptr %0, ptr %"Builtin.$char#_3146_0") {
  %2 = getelementptr { i8, <1 x i8> }, ptr %"Builtin.$char#_3146_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Char#_0"
  ]

"switch_branch_Builtin.Char#_0":                  ; preds = %1
  %4 = getelementptr { i8, { i8 } }, ptr %"Builtin.$char#_3146_0", i32 0, i32 1
  %5 = getelementptr { i8 }, ptr %4, i32 0, i32 0
  %6 = load i8, ptr %5, align 1
  %7 = call i32 @"Builtin.isLower#"(ptr null, i8 %6)
  %8 = call ptr @"Builtin.isTrue#"(ptr null, i32 %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_171"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$neString_curry_3133"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neString(ptr %0, ptr %"Builtin.$string#_3124_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3124_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_171", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_172"(ptr %0, double %1) {
  %x_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %x_0 = load double, ptr %x_addr_0, align 8
  %3 = call i32 @"Builtin.$gtDouble#_curry_3302"(ptr null, double %x_0, double %1)
  ret i32 %3
}

define internal ptr @"Builtin.gtDouble#"(ptr %0, double %"Builtin.$x_3297_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$x_3297_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_172", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$neInt64#_curry_2441"(ptr %0, i64 %"Builtin.$x_2442_0", i64 %"Builtin.$y_2443_0") {
  %2 = call ptr @Builtin.malgo_ne_int64_t(ptr null, i64 %"Builtin.$x_2442_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_2443_0")
  ret i32 %7
}

define internal ptr @Builtin.malgo_exit_failure(ptr %0, ptr %"Builtin.$p_2160_0") {
  %2 = call ptr @malgo_exit_failure(ptr %"Builtin.$p_2160_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_173"(ptr %0, float %1) {
  %x_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %x_0 = load float, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$ltFloat#_curry_2665"(ptr null, float %x_0, float %1)
  ret i32 %3
}

define internal ptr @"Builtin.ltFloat#"(ptr %0, float %"Builtin.$x_2660_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$x_2660_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_173", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_eq_char_curry_2052"(ptr %0, i8 %"Builtin.$p_2053_0", i8 %"Builtin.$p_2054_0") {
  %2 = call i32 @malgo_eq_char(i8 %"Builtin.$p_2053_0", i8 %"Builtin.$p_2054_0")
  ret i32 %2
}

define internal i64 @"Builtin.$mulInt64#_curry_2501"(ptr %0, i64 %"Builtin.$x_2502_0", i64 %"Builtin.$y_2503_0") {
  %2 = call ptr @Builtin.malgo_mul_int64_t(ptr null, i64 %"Builtin.$x_2502_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i64 %6(ptr %4, i64 %"Builtin.$y_2503_0")
  ret i64 %7
}

define internal i32 @"Builtin.$geFloat#_curry_3474"(ptr %0, float %"Builtin.$x_3475_0", float %"Builtin.$y_3476_0") {
  %2 = call ptr @Builtin.malgo_ge_float(ptr null, float %"Builtin.$x_3475_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, float %"Builtin.$y_3476_0")
  ret i32 %7
}

define internal i32 @Builtin.malgo_char_ord(ptr %0, i8 %"Builtin.$p_2122_0") {
  %2 = call i32 @malgo_char_ord(i8 %"Builtin.$p_2122_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_174"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$geString_curry_3394"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geString(ptr %0, ptr %"Builtin.$string#_3385_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3385_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_174", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$eqFloat_curry_3692"(ptr %0, ptr %"Builtin.$float#_3693_0", ptr %"Builtin.$float#_3694_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3693_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Float#_0"
  ]

"switch_branch_Builtin.Float#_0":                 ; preds = %1
  %4 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3693_0", i32 0, i32 1
  %5 = getelementptr { float }, ptr %4, i32 0, i32 0
  %6 = load float, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$float#_3694_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Float#_1"
  ]

"switch_branch_Builtin.Float#_1":                 ; preds = %"switch_branch_Builtin.Float#_0"
  %9 = getelementptr { i8, { float } }, ptr %"Builtin.$float#_3694_0", i32 0, i32 1
  %10 = getelementptr { float }, ptr %9, i32 0, i32 0
  %11 = load float, ptr %10, align 4
  %12 = call ptr @"Builtin.eqFloat#"(ptr null, float %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, float %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Float#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_ge_char_curry_2082"(ptr %0, i8 %"Builtin.$p_2083_0", i8 %"Builtin.$p_2084_0") {
  %2 = call i32 @malgo_ge_char(i8 %"Builtin.$p_2083_0", i8 %"Builtin.$p_2084_0")
  ret i32 %2
}

define internal ptr @"Builtin.$malgo_substring_curry_2149"(ptr %0, ptr %"Builtin.$p_2150_0", i64 %"Builtin.$p_2151_0", i64 %"Builtin.$p_2152_0") {
  %2 = call ptr @malgo_substring(ptr %"Builtin.$p_2150_0", i64 %"Builtin.$p_2151_0", i64 %"Builtin.$p_2152_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_175"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$geInt64#_curry_3410"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.geInt64#"(ptr %0, i64 %"Builtin.$x_3405_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_3405_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_175", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_176"(ptr %0, ptr %1) {
  %x_addr_0 = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %x_0 = load ptr, ptr %x_addr_0, align 8
  %3 = call ptr @"Builtin.$appendString#_curry_3968"(ptr null, ptr %x_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Builtin.appendString#"(ptr %0, ptr %"Builtin.$x_3963_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$x_3963_0", ptr %x_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_176", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$malgo_mul_int32_t_curry_1822"(ptr %0, i32 %"Builtin.$p_1823_0", i32 %"Builtin.$p_1824_0") {
  %2 = call i32 @malgo_mul_int32_t(i32 %"Builtin.$p_1823_0", i32 %"Builtin.$p_1824_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_177"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltChar_curry_2913"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltChar(ptr %0, ptr %"Builtin.$char#_2904_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_2904_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_177", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_178"(ptr %0, ptr %1) {
  %"char#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"char#_0" = load ptr, ptr %"char#_addr_0", align 8
  %3 = call ptr @"Builtin.$geChar_curry_3554"(ptr null, ptr %"char#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.geChar(ptr %0, ptr %"Builtin.$char#_3545_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"char#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$char#_3545_0", ptr %"char#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_178", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_179"(ptr %0, ptr %1) {
  %"float#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"float#_0" = load ptr, ptr %"float#_addr_0", align 8
  %3 = call ptr @"Builtin.$neFloat_curry_3073"(ptr null, ptr %"float#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neFloat(ptr %0, ptr %"Builtin.$float#_3064_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"float#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$float#_3064_0", ptr %"float#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_179", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_180"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$divInt64_curry_3790"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.divInt64(ptr %0, ptr %"Builtin.$int64#_3781_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3781_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_180", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Builtin.$geInt64#_curry_3410"(ptr %0, i64 %"Builtin.$x_3411_0", i64 %"Builtin.$y_3412_0") {
  %2 = call ptr @Builtin.malgo_ge_int64_t(ptr null, i64 %"Builtin.$x_3411_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_3412_0")
  ret i32 %7
}

define internal i64 @"Primitive.#let_closure_181"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i64 @"Builtin.$mulInt64#_curry_2501"(ptr null, i64 %x_0, i64 %1)
  ret i64 %3
}

define internal ptr @"Builtin.mulInt64#"(ptr %0, i64 %"Builtin.$x_2496_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_2496_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_181", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_182"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$eqString_curry_3596"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.eqString(ptr %0, ptr %"Builtin.$string#_3587_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3587_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_182", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i64 @"Builtin.$malgo_add_int64_t_curry_1834"(ptr %0, i64 %"Builtin.$p_1835_0", i64 %"Builtin.$p_1836_0") {
  %2 = call i64 @malgo_add_int64_t(i64 %"Builtin.$p_1835_0", i64 %"Builtin.$p_1836_0")
  ret i64 %2
}

define internal i8 @Builtin.malgo_get_char(ptr %0, ptr %"Builtin.$p_2165_0") {
  %2 = call i8 @malgo_get_char(ptr %"Builtin.$p_2165_0")
  ret i8 %2
}

define internal ptr @"Builtin.$neDouble_curry_3053"(ptr %0, ptr %"Builtin.$double#_3054_0", ptr %"Builtin.$double#_3055_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3054_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3054_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3055_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3055_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.neDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_lt_char_curry_2064"(ptr %0, i8 %"Builtin.$p_2065_0", i8 %"Builtin.$p_2066_0") {
  %2 = call i32 @malgo_lt_char(i8 %"Builtin.$p_2065_0", i8 %"Builtin.$p_2066_0")
  ret i32 %2
}

define internal ptr @"Primitive.#let_closure_183"(ptr %0, ptr %1) {
  %"int32#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int32#_0" = load ptr, ptr %"int32#_addr_0", align 8
  %3 = call ptr @"Builtin.$ltInt32_curry_2973"(ptr null, ptr %"int32#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.ltInt32(ptr %0, ptr %"Builtin.$int32#_2964_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int32#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int32#_2964_0", ptr %"int32#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_183", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_184"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$neInt64_curry_3113"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.neInt64(ptr %0, ptr %"Builtin.$int64#_3104_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3104_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_184", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$eqInt32_curry_3660"(ptr %0, ptr %"Builtin.$int32#_3661_0", ptr %"Builtin.$int32#_3662_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3661_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3661_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_3662_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_3662_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.eqInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Primitive.#let_closure_185"(ptr %0, i32 %1) {
  %p_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %p_0 = load i32, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_add_int32_t_curry_1810"(ptr null, i32 %p_0, i32 %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_add_int32_t(ptr %0, i32 %"Builtin.$p_1808_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$p_1808_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_185", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$addInt32_curry_4048"(ptr %0, ptr %"Builtin.$int32#_4049_0", ptr %"Builtin.$int32#_4050_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_4049_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_4049_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_4050_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_4050_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.addInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.Int32#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @Builtin.malgo_get_contents(ptr %0, ptr %"Builtin.$p_2166_0") {
  %2 = call ptr @malgo_get_contents(ptr %"Builtin.$p_2166_0")
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_186"(ptr %0, double %1) {
  %p_addr_0 = getelementptr { double }, ptr %0, i32 0, i32 0
  %p_0 = load double, ptr %p_addr_0, align 8
  %3 = call i32 @"Builtin.$malgo_eq_double_curry_2016"(ptr null, double %p_0, double %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_eq_double(ptr %0, double %"Builtin.$p_2014_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ double }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { double }, ptr %let_capture_0, i32 0, i32 0
  store double %"Builtin.$p_2014_0", ptr %p_0, align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_186", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.Char#"(ptr %0, i8 %"Builtin.$p_1800_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, { i8 } }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, { i8 } }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = getelementptr { i8, { i8 } }, ptr %2, i32 0, i32 1, i32 0
  store i8 %"Builtin.$p_1800_0", ptr %4, align 1
  ret ptr %2
}

define internal ptr @"Builtin.$ltInt32_curry_2973"(ptr %0, ptr %"Builtin.$int32#_2974_0", ptr %"Builtin.$int32#_2975_0") {
  %2 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2974_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int32#_0"
  ]

"switch_branch_Builtin.Int32#_0":                 ; preds = %1
  %4 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2974_0", i32 0, i32 1
  %5 = getelementptr { i32 }, ptr %4, i32 0, i32 0
  %6 = load i32, ptr %5, align 4
  %7 = getelementptr { i8, <4 x i8> }, ptr %"Builtin.$int32#_2975_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int32#_1"
  ]

"switch_branch_Builtin.Int32#_1":                 ; preds = %"switch_branch_Builtin.Int32#_0"
  %9 = getelementptr { i8, { i32 } }, ptr %"Builtin.$int32#_2975_0", i32 0, i32 1
  %10 = getelementptr { i32 }, ptr %9, i32 0, i32 0
  %11 = load i32, ptr %10, align 4
  %12 = call ptr @"Builtin.ltInt32#"(ptr null, i32 %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, i32 %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int32#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal ptr @"Builtin.char#"(ptr %0, i8 %"Builtin.$x_3929_0") {
  %2 = call ptr @"Builtin.Char#"(ptr null, i8 %"Builtin.$x_3929_0")
  ret ptr %2
}

define internal ptr @Builtin.True(ptr %0) {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 1, ptr %3, align 1
  ret ptr %2
}

define internal i32 @"Builtin.$gtInt32#_curry_3238"(ptr %0, i32 %"Builtin.$x_3239_0", i32 %"Builtin.$y_3240_0") {
  %2 = call ptr @Builtin.malgo_gt_int32_t(ptr null, i32 %"Builtin.$x_3239_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i32 %"Builtin.$y_3240_0")
  ret i32 %7
}

define internal i32 @"Builtin.$eqChar#_curry_3740"(ptr %0, i8 %"Builtin.$x_3741_0", i8 %"Builtin.$y_3742_0") {
  %2 = call ptr @Builtin.malgo_eq_char(ptr null, i8 %"Builtin.$x_3741_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i8 %"Builtin.$y_3742_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_187"(ptr %0, float %1) {
  %p_addr_0 = getelementptr { float }, ptr %0, i32 0, i32 0
  %p_0 = load float, ptr %p_addr_0, align 4
  %3 = call i32 @"Builtin.$malgo_ge_float_curry_2010"(ptr null, float %p_0, float %1)
  ret i32 %3
}

define internal ptr @Builtin.malgo_ge_float(ptr %0, float %"Builtin.$p_2008_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ float }, ptr null, i32 1) to i64))
  %p_0 = getelementptr { float }, ptr %let_capture_0, i32 0, i32 0
  store float %"Builtin.$p_2008_0", ptr %p_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_187", ptr %let_func_0, align 8
  ret ptr %2
}

define internal double @"Builtin.$malgo_sub_double_curry_1888"(ptr %0, double %"Builtin.$p_1889_0", double %"Builtin.$p_1890_0") {
  %2 = call double @malgo_sub_double(double %"Builtin.$p_1889_0", double %"Builtin.$p_1890_0")
  ret double %2
}

define internal ptr @"Primitive.#let_closure_188"(ptr %0, ptr %1) {
  %"int64#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"int64#_0" = load ptr, ptr %"int64#_addr_0", align 8
  %3 = call ptr @"Builtin.$atString_curry_3952"(ptr null, ptr %"int64#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.atString(ptr %0, ptr %"Builtin.$int64#_3943_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"int64#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$int64#_3943_0", ptr %"int64#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_188", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Primitive.#let_closure_189"(ptr %0, ptr %1) {
  %c_addr_0 = getelementptr { i8 }, ptr %0, i32 0, i32 0
  %c_0 = load i8, ptr %c_addr_0, align 1
  %3 = call ptr @"Builtin.$consString#_curry_3902"(ptr null, i8 %c_0, ptr %1)
  ret ptr %3
}

define internal ptr @"Builtin.consString#"(ptr %0, i8 %"Builtin.$c_3897_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8 }, ptr null, i32 1) to i64))
  %c_0 = getelementptr { i8 }, ptr %let_capture_0, i32 0, i32 0
  store i8 %"Builtin.$c_3897_0", ptr %c_0, align 1
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_189", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$appendString#_curry_3968"(ptr %0, ptr %"Builtin.$x_3969_0", ptr %"Builtin.$y_3970_0") {
  %2 = call ptr @Builtin.malgo_string_append(ptr null, ptr %"Builtin.$x_3969_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call ptr %6(ptr %4, ptr %"Builtin.$y_3970_0")
  ret ptr %7
}

define internal i32 @"Builtin.$malgo_ne_double_curry_2022"(ptr %0, double %"Builtin.$p_2023_0", double %"Builtin.$p_2024_0") {
  %2 = call i32 @malgo_ne_double(double %"Builtin.$p_2023_0", double %"Builtin.$p_2024_0")
  ret i32 %2
}

define internal i32 @"Builtin.$malgo_le_int64_t_curry_1968"(ptr %0, i64 %"Builtin.$p_1969_0", i64 %"Builtin.$p_1970_0") {
  %2 = call i32 @malgo_le_int64_t(i64 %"Builtin.$p_1969_0", i64 %"Builtin.$p_1970_0")
  ret i32 %2
}

define internal i32 @"Builtin.$ltInt64#_curry_2641"(ptr %0, i64 %"Builtin.$x_2642_0", i64 %"Builtin.$y_2643_0") {
  %2 = call ptr @Builtin.malgo_lt_int64_t(ptr null, i64 %"Builtin.$x_2642_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, i64 %"Builtin.$y_2643_0")
  ret i32 %7
}

define internal i32 @"Primitive.#let_closure_190"(ptr %0, i64 %1) {
  %x_addr_0 = getelementptr { i64 }, ptr %0, i32 0, i32 0
  %x_0 = load i64, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$leInt64#_curry_2720"(ptr null, i64 %x_0, i64 %1)
  ret i32 %3
}

define internal ptr @"Builtin.leInt64#"(ptr %0, i64 %"Builtin.$x_2715_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i64 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i64 }, ptr %let_capture_0, i32 0, i32 0
  store i64 %"Builtin.$x_2715_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_190", ptr %let_func_0, align 8
  ret ptr %2
}

define internal i32 @"Primitive.#let_closure_191"(ptr %0, i32 %1) {
  %x_addr_0 = getelementptr { i32 }, ptr %0, i32 0, i32 0
  %x_0 = load i32, ptr %x_addr_0, align 4
  %3 = call i32 @"Builtin.$eqInt32#_curry_3644"(ptr null, i32 %x_0, i32 %1)
  ret i32 %3
}

define internal ptr @"Builtin.eqInt32#"(ptr %0, i32 %"Builtin.$x_3639_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i32 }, ptr null, i32 1) to i64))
  %x_0 = getelementptr { i32 }, ptr %let_capture_0, i32 0, i32 0
  store i32 %"Builtin.$x_3639_0", ptr %x_0, align 4
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_191", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.$eqDouble_curry_3724"(ptr %0, ptr %"Builtin.$double#_3725_0", ptr %"Builtin.$double#_3726_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3725_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3725_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_3726_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_1"
  ]

"switch_branch_Builtin.Double#_1":                ; preds = %"switch_branch_Builtin.Double#_0"
  %9 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_3726_0", i32 0, i32 1
  %10 = getelementptr { double }, ptr %9, i32 0, i32 0
  %11 = load double, ptr %10, align 8
  %12 = call ptr @"Builtin.eqDouble#"(ptr null, double %6)
  %13 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 0
  %14 = load ptr, ptr %13, align 8
  %15 = getelementptr { ptr, ptr }, ptr %12, i32 0, i32 1
  %16 = load ptr, ptr %15, align 8
  %17 = call i32 %16(ptr %14, double %11)
  %18 = call ptr @"Builtin.isTrue#"(ptr null, i32 %17)
  ret ptr %18

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Double#_0"
  unreachable

switch_default_1:                                 ; preds = %1
  unreachable
}

define internal i32 @"Builtin.$malgo_div_int32_t_curry_1828"(ptr %0, i32 %"Builtin.$p_1829_0", i32 %"Builtin.$p_1830_0") {
  %2 = call i32 @malgo_div_int32_t(i32 %"Builtin.$p_1829_0", i32 %"Builtin.$p_1830_0")
  ret i32 %2
}

define internal ptr @Builtin.toStringDouble(ptr %0, ptr %"Builtin.$double#_2195_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$double#_2195_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Double#_0"
  ]

"switch_branch_Builtin.Double#_0":                ; preds = %1
  %4 = getelementptr { i8, { double } }, ptr %"Builtin.$double#_2195_0", i32 0, i32 1
  %5 = getelementptr { double }, ptr %4, i32 0, i32 0
  %6 = load double, ptr %5, align 8
  %7 = call ptr @"Builtin.toStringDouble#"(ptr null, double %6)
  %8 = call ptr @"Builtin.String#"(ptr null, ptr %7)
  ret ptr %8

switch_default_0:                                 ; preds = %1
  unreachable
}

define internal ptr @"Primitive.#let_closure_192"(ptr %0, ptr %1) {
  %"string#_addr_0" = getelementptr { ptr }, ptr %0, i32 0, i32 0
  %"string#_0" = load ptr, ptr %"string#_addr_0", align 8
  %3 = call ptr @"Builtin.$appendString_curry_3984"(ptr null, ptr %"string#_0", ptr %1)
  ret ptr %3
}

define internal ptr @Builtin.appendString(ptr %0, ptr %"Builtin.$string#_3975_0") {
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr, ptr }, ptr null, i32 1) to i64))
  %let_capture_0 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ ptr }, ptr null, i32 1) to i64))
  %"string#_0" = getelementptr { ptr }, ptr %let_capture_0, i32 0, i32 0
  store ptr %"Builtin.$string#_3975_0", ptr %"string#_0", align 8
  %let_capture_1 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  store ptr %let_capture_0, ptr %let_capture_1, align 8
  %let_func_0 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  store ptr @"Primitive.#let_closure_192", ptr %let_func_0, align 8
  ret ptr %2
}

define internal ptr @"Builtin.toStringChar#"(ptr %0, i8 %"Builtin.$x_2200_0") {
  %2 = call ptr @Builtin.malgo_char_to_string(ptr null, i8 %"Builtin.$x_2200_0")
  ret ptr %2
}

define internal float @"Builtin.sqrtFloat#"(ptr %0, float %"Builtin.$x_2385_0") {
  %2 = call float @Builtin.sqrtf(ptr null, float %"Builtin.$x_2385_0")
  ret float %2
}

define internal i32 @"Builtin.$geString#_curry_3378"(ptr %0, ptr %"Builtin.$x_3379_0", ptr %"Builtin.$y_3380_0") {
  %2 = call ptr @Builtin.malgo_ge_string(ptr null, ptr %"Builtin.$x_3379_0")
  %3 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 0
  %4 = load ptr, ptr %3, align 8
  %5 = getelementptr { ptr, ptr }, ptr %2, i32 0, i32 1
  %6 = load ptr, ptr %5, align 8
  %7 = call i32 %6(ptr %4, ptr %"Builtin.$y_3380_0")
  ret i32 %7
}

define internal ptr @"Builtin.$substring_curry_2239"(ptr %0, ptr %"Builtin.$string#_2240_0", ptr %"Builtin.$int64#_2241_0", ptr %"Builtin.$int64#_2242_0") {
  %2 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$string#_2240_0", i32 0, i32 0
  %3 = load i8, ptr %2, align 1
  switch i8 %3, label %switch_default_2 [
    i8 0, label %"switch_branch_Builtin.String#_0"
  ]

"switch_branch_Builtin.String#_0":                ; preds = %1
  %4 = getelementptr { i8, { ptr } }, ptr %"Builtin.$string#_2240_0", i32 0, i32 1
  %5 = getelementptr { ptr }, ptr %4, i32 0, i32 0
  %6 = load ptr, ptr %5, align 8
  %7 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2241_0", i32 0, i32 0
  %8 = load i8, ptr %7, align 1
  switch i8 %8, label %switch_default_1 [
    i8 0, label %"switch_branch_Builtin.Int64#_0"
  ]

"switch_branch_Builtin.Int64#_0":                 ; preds = %"switch_branch_Builtin.String#_0"
  %9 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2241_0", i32 0, i32 1
  %10 = getelementptr { i64 }, ptr %9, i32 0, i32 0
  %11 = load i64, ptr %10, align 4
  %12 = getelementptr { i8, <8 x i8> }, ptr %"Builtin.$int64#_2242_0", i32 0, i32 0
  %13 = load i8, ptr %12, align 1
  switch i8 %13, label %switch_default_0 [
    i8 0, label %"switch_branch_Builtin.Int64#_1"
  ]

"switch_branch_Builtin.Int64#_1":                 ; preds = %"switch_branch_Builtin.Int64#_0"
  %14 = getelementptr { i8, { i64 } }, ptr %"Builtin.$int64#_2242_0", i32 0, i32 1
  %15 = getelementptr { i64 }, ptr %14, i32 0, i32 0
  %16 = load i64, ptr %15, align 4
  %17 = call ptr @"Builtin.substring#"(ptr null, ptr %6)
  %18 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 0
  %19 = load ptr, ptr %18, align 8
  %20 = getelementptr { ptr, ptr }, ptr %17, i32 0, i32 1
  %21 = load ptr, ptr %20, align 8
  %22 = call ptr %21(ptr %19, i64 %11)
  %23 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 0
  %24 = load ptr, ptr %23, align 8
  %25 = getelementptr { ptr, ptr }, ptr %22, i32 0, i32 1
  %26 = load ptr, ptr %25, align 8
  %27 = call ptr %26(ptr %24, i64 %16)
  %28 = call ptr @"Builtin.String#"(ptr null, ptr %27)
  ret ptr %28

switch_default_0:                                 ; preds = %"switch_branch_Builtin.Int64#_0"
  unreachable

switch_default_1:                                 ; preds = %"switch_branch_Builtin.String#_0"
  unreachable

switch_default_2:                                 ; preds = %1
  unreachable
}

define internal double @"Builtin.sqrtDouble#"(ptr %0, double %"Builtin.$x_2392_0") {
  %2 = call double @Builtin.sqrt(ptr null, double %"Builtin.$x_2392_0")
  ret double %2
}

define i32 @main(ptr %0) {
  call void @GC_init()
  call void @malgo_load_Primitive()
  %2 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %3 = getelementptr { i8, {} }, ptr %2, i32 0, i32 0
  store i8 0, ptr %3, align 1
  %4 = call ptr @Primitive.main(ptr null, ptr %2)
  ret i32 0
}

define internal void @malgo_load_Primitive() {
  %1 = call ptr @malgo_malloc(i64 ptrtoint (ptr getelementptr inbounds ({ i8, {} }, ptr null, i32 1) to i64))
  %2 = getelementptr { i8, {} }, ptr %1, i32 0, i32 0
  store i8 0, ptr %2, align 1
  %3 = call ptr @Builtin.malgo_unsafe_cast(ptr null, ptr %1)
  store ptr %3, ptr @Builtin.undefined, align 8
  ret void
}
