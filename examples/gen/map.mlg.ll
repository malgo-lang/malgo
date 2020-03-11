; ModuleID = './examples/map.mlg'
source_filename = "./examples/map.mlg"


 


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int0(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {i64, i64}* @map1({i64 (i8*, i64)*, i8*}* , {i64, i64}* )    {
  %3 = getelementptr  {i64, i64}, {i64, i64}* %1, i32 0, i32 0 
  %4 = load  i64, i64* %3 
  %5 = getelementptr  {i64, i64}, {i64, i64}* %1, i32 0, i32 1 
  %6 = load  i64, i64* %5 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %0, i32 0, i32 0 
  %8 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %7 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %0, i32 0, i32 1 
  %10 = load  i8*, i8** %9 
  %11 =  call ccc  i64  %8(i8*  %10, i64  %4)  
  %12 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %0, i32 0, i32 0 
  %13 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %12 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %0, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 =  call ccc  i64  %13(i8*  %15, i64  %6)  
  %17 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %18 = bitcast i8* %17 to {i64, i64}* 
  %19 = getelementptr  {i64, i64}, {i64, i64}* %18, i32 0, i32 0 
  store  i64 %11, i64* %19 
  %20 = getelementptr  {i64, i64}, {i64, i64}* %18, i32 0, i32 1 
  store  i64 %16, i64* %20 
  ret {i64, i64}* %18 
}


define external ccc  i64 @add426(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @add426, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 = add   i64 %1, 42 
  ret i64 %8 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @add426, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64, i64}* 
  %10 = getelementptr  {i64, i64}, {i64, i64}* %9, i32 0, i32 0 
  store  i64 1, i64* %10 
  %11 = getelementptr  {i64, i64}, {i64, i64}* %9, i32 0, i32 1 
  store  i64 2, i64* %11 
  %12 =  call ccc  {i64, i64}*  @map1({i64 (i8*, i64)*, i8*}*  %5, {i64, i64}*  %9)  
  %13 = getelementptr  {i64, i64}, {i64, i64}* %12, i32 0, i32 0 
  %14 = load  i64, i64* %13 
  %15 = getelementptr  {i64, i64}, {i64, i64}* %12, i32 0, i32 1 
  %16 = load  i64, i64* %15 
  %17 =  call ccc  {}*  @print_int0(i64  %14)  
  ret i32 0 
}
