; ModuleID = './examples/curry.mlg'
source_filename = "./examples/curry.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {i64 (i8*, i64)*, i8*}* @$lambda7(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @$lambda7, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %6 
  %7 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64}* 
  %10 = getelementptr  {i64}, {i64}* %9, i32 0, i32 0 
  store  i64 %1, i64* %10 
  %11 = bitcast {i64}* %9 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {i64 (i8*, i64)*, i8*}* 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %13, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda6, i64 (i8*, i64)** %14 
  %15 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %13, i32 0, i32 1 
  store  i8* %11, i8** %15 
  ret {i64 (i8*, i64)*, i8*}* %13 
}


define external ccc  i64 @$lambda6(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda6, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %5, %1 
  ret i64 %10 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @$lambda7, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %6 
  %7 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  %9 = load  {i64 (i8*, i64)*, i8*}* (i8*, i64)*, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %8 
  %10 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  %11 = load  i8*, i8** %10 
  %12 =  call ccc  {i64 (i8*, i64)*, i8*}*  %9(i8*  %11, i64  1)  
  %13 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  %14 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %13 
  %15 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  %16 = load  i8*, i8** %15 
  %17 =  call ccc  i64  %14(i8*  %16, i64  2)  
  ret i32 0 
}
