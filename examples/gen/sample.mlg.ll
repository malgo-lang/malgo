; ModuleID = './examples/sample.mlg'
source_filename = "./examples/sample.mlg"


 


declare external ccc  i8* @substring(i8*, i64, i64)    


define external ccc  i8* @substring10(i8* , i64 , i64 )    {
  %4 =  call ccc  i8*  @substring(i8*  %0, i64  %1, i64  %2)  
  ret i8* %4 
}


declare external ccc  i64 @size(i8*)    


define external ccc  i64 @size8(i8* )    {
  %2 =  call ccc  i64  @size(i8*  %0)  
  ret i64 %2 
}


define external ccc  {}* @println_int13(i64 )    {
  %2 =  call ccc  {}*  @print_int2(i64  %0)  
  %3 =  call ccc  {}*  @newline9()  
  ret {}* %3 
}


define external ccc  {}* @println_float14(double )    {
  %2 =  call ccc  {}*  @print_float3(double  %0)  
  %3 =  call ccc  {}*  @newline9()  
  ret {}* %3 
}


declare external ccc  {}* @println(i8*)    


define external ccc  {}* @println1(i8* )    {
  %2 =  call ccc  {}*  @println(i8*  %0)  
  ret {}* %2 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int2(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  {}* @print_float(double)    


define external ccc  {}* @print_float3(double )    {
  %2 =  call ccc  {}*  @print_float(double  %0)  
  ret {}* %2 
}


define external ccc  {}* @print_fib29(i64 )    {
  %2 =  call ccc  i64  @fib18(i64  %0)  
  %3 =  call ccc  {}*  @print_int2(i64  %2)  
  ret {}* %3 
}


declare external ccc  {}* @print(i8*)    


define external ccc  {}* @print0(i8* )    {
  %2 =  call ccc  {}*  @print(i8*  %0)  
  ret {}* %2 
}


declare external ccc  i64 @ord(i8)    


define external ccc  i64 @ord6(i8 )    {
  %2 =  call ccc  i64  @ord(i8  %0)  
  ret i64 %2 
}


declare external ccc  i1 @not(i1)    


define external ccc  i1 @not12(i1 )    {
  %2 =  call ccc  i1  @not(i1  %0)  
  ret i1 %2 
}


declare external ccc  {}* @newline()    


define external ccc  {}* @newline9()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


declare external ccc  i8 @getchar()    


define external ccc  i8 @getChar5()    {
  %1 =  call ccc  i8  @getchar()  
  ret i8 %1 
}


declare external ccc  {}* @flush()    


define external ccc  {}* @flush4()    {
  %1 =  call ccc  {}*  @flush()  
  ret {}* %1 
}


define external ccc  i64 @fib18(i64 )    {
; <label>:1:
  %2 = icmp sle i64 %0, 1 
  %3 = alloca i64 
  br i1 %2, label %then_0, label %else_0 
then_0:
  store  i64 1, i64* %3 
  br label %end_0 
else_0:
  %4 = sub   i64 %0, 1 
  %5 =  call ccc  i64  @fib18(i64  %4)  
  %6 = sub   i64 %0, 2 
  %7 =  call ccc  i64  @fib18(i64  %6)  
  %8 = add   i64 %5, %7 
  store  i64 %8, i64* %3 
  br label %end_0 
end_0:
  %9 = load  i64, i64* %3 
  ret i64 %9 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @do_nothing21()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  ret {}* %2 
}


declare external ccc  i8* @concat(i8*, i8*)    


define external ccc  i8* @concat11(i8* , i8* )    {
  %3 =  call ccc  i8*  @concat(i8*  %0, i8*  %1)  
  ret i8* %3 
}


declare external ccc  i8 @chr(i64)    


define external ccc  i8 @chr7(i64 )    {
  %2 =  call ccc  i8  @chr(i64  %0)  
  ret i8 %2 
}


define external ccc  double @area23(double )    {
  %2 = fmul double %0, %0 
  %3 = fmul double %2, 3.140000e0 
  ret double %3 
}


define external ccc  i64 @add225(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @add225, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %5, %1 
  ret i64 %10 
}


define external ccc  i64 @add22(i64 )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  store  i64 %0, i64* %4 
  %5 = bitcast {i64}* %3 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @add225, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %5, i8** %9 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  %11 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %10 
  %12 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  %13 = load  i8*, i8** %12 
  %14 =  call ccc  i64  %11(i8*  %13, i64  2)  
  ret i64 %14 
}


define external ccc  i64 @$lambda117(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda117, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 = mul   i64 %1, 4 
  ret i64 %8 
}


declare external ccc  void @GC_init()    


@$globle_str_223 =  unnamed_addr  constant [6 x i8] c"malgo\00"


@$globle_str_224 =  unnamed_addr  constant [14 x i8] c"Hello, world!\00"


@$globle_str_225 =  unnamed_addr  constant [11 x i8] c"fib(10) = \00"


@$globle_str_226 =  unnamed_addr  constant [4 x i8] c"foo\00"


@$globle_str_227 =  unnamed_addr  constant [4 x i8] c"bar\00"


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 = add   i64 42, 1 
  %2 =  call ccc  {}*  @println1(i8*  getelementptr inbounds ([14 x i8], [14 x i8]* @$globle_str_224, i32 0, i32 0))  
  %3 =  call ccc  {}*  @print0(i8*  getelementptr inbounds ([11 x i8], [11 x i8]* @$globle_str_225, i32 0, i32 0))  
  %4 =  call ccc  i64  @fib18(i64  10)  
  %5 =  call ccc  {}*  @println_int13(i64  %4)  
  %6 =  call ccc  {}*  @do_nothing21()  
  %7 =  call ccc  i64  @add22(i64  2)  
  %8 =  call ccc  {}*  @println_int13(i64  %7)  
  %9 =  call ccc  {}*  @println_int13(i64  %1)  
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {}* 
  %12 = bitcast {}* %11 to i8* 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i64 (i8*, i64)*, i8*}* 
  %15 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %14, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda117, i64 (i8*, i64)** %15 
  %16 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %14, i32 0, i32 1 
  store  i8* %12, i8** %16 
  %17 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %14, i32 0, i32 0 
  %18 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %17 
  %19 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %14, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i64  %18(i8*  %20, i64  3)  
  %22 =  call ccc  {}*  @println_int13(i64  %21)  
  %23 =  call ccc  i8*  @substring10(i8*  getelementptr inbounds ([6 x i8], [6 x i8]* @$globle_str_223, i32 0, i32 0), i64  1, i64  3)  
  %24 =  call ccc  {}*  @println1(i8*  %23)  
  %25 =  call ccc  i8*  @concat11(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @$globle_str_226, i32 0, i32 0), i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @$globle_str_227, i32 0, i32 0))  
  %26 =  call ccc  {}*  @println1(i8*  %25)  
  %27 =  call ccc  {}*  @println_float14(double  3.140000e0)  
  %28 =  call ccc  double  @area23(double  1.000000e1)  
  %29 =  call ccc  {}*  @println_float14(double  %28)  
  ret i32 0 
}
