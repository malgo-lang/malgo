source_filename = "./examples/array.mlg"
; ModuleID = './examples/array.mlg'


 


declare external ccc  {}* @newline()    


define external ccc  {}* @newline1()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int0(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 10 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i64* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64*, i64}* 
  %6 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  store  i64* %3, i64** %6 
  %7 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  store  i64 10, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 10 
  br i1 %10, label %body_0, label %end_0 
body_0:
  %11 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %12 = load  i64*, i64** %11 
  %13 = getelementptr  i64, i64* %12, i64 %9 
  store  i64 0, i64* %13 
  %14 = add   i64 %9, 1 
  store  i64 %14, i64* %8 
  br label %cond_0 
end_0:
  %15 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %16 = load  i64*, i64** %15 
  %17 = getelementptr  i64, i64* %16, i64 1 
  %18 = load  i64, i64* %17 
  %19 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %20 = load  i64*, i64** %19 
  %21 = getelementptr  i64, i64* %20, i64 2 
  store  i64 42, i64* %21 
  %22 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %23 = load  i64*, i64** %22 
  %24 = getelementptr  i64, i64* %23, i64 2 
  %25 = load  i64, i64* %24 
  %26 =  call ccc  {}*  @print_int0(i64  %18)  
  %27 =  call ccc  {}*  @newline1()  
  %28 =  call ccc  {}*  @print_int0(i64  %25)  
  %29 =  call ccc  {}*  @newline1()  
  ret i32 0 
}
