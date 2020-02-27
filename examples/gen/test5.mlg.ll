; ModuleID = './examples/test5.mlg'
source_filename = "./examples/test5.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {i64 (i8*, i64)*, i8*}* @k1(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %8 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @k1, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %8 
  %9 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i64 (i8*, i64, i64)*, i8*}* 
  %12 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %11, i32 0, i32 0 
  store  i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %12 
  %13 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i64}* 
  %16 = getelementptr  {i64}, {i64}* %15, i32 0, i32 0 
  store  i64 %5, i64* %16 
  %17 = bitcast {i64}* %15 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i64 (i8*, i64)*, i8*}* 
  %20 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %19, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda13, i64 (i8*, i64)** %20 
  %21 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %19, i32 0, i32 1 
  store  i8* %17, i8** %21 
  ret {i64 (i8*, i64)*, i8*}* %19 
}


define external ccc  i64 @f2(i8* , i64 , i64 )    {
  %4 = bitcast i8* %0 to {i64}* 
  %5 = getelementptr  {i64}, {i64}* %4, i32 0, i32 0 
  %6 = load  i64, i64* %5 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %9 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %8, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @k1, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %9 
  %10 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %8, i32 0, i32 1 
  store  i8* %0, i8** %10 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64, i64)*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i64 (i8*, i64, i64)*, i8*}* 
  %13 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %12, i32 0, i32 0 
  store  i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %13 
  %14 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %12, i32 0, i32 1 
  store  i8* %0, i8** %14 
  %15 =  call ccc  {i64 (i8*, i64)*, i8*}*  @k1(i8*  %0, i64  %1)  
  %16 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 0 
  %17 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %16 
  %18 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 1 
  %19 = load  i8*, i8** %18 
  %20 =  call ccc  i64  %17(i8*  %19, i64  %2)  
  ret i64 %20 
}


define external ccc  i64 @$lambda13(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda13, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %1, %5 
  ret i64 %10 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64}* 
  %3 = getelementptr  {i64}, {i64}* %2, i32 0, i32 0 
  store  i64 42, i64* %3 
  %4 = bitcast {i64}* %2 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %7 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @k1, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %7 
  %8 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  store  i8* %4, i8** %8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i64}* 
  %11 = getelementptr  {i64}, {i64}* %10, i32 0, i32 0 
  store  i64 42, i64* %11 
  %12 = bitcast {i64}* %10 to i8* 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64, i64)*, i8*}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i64 (i8*, i64, i64)*, i8*}* 
  %15 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %14, i32 0, i32 0 
  store  i64 (i8*, i64, i64)* @f2, i64 (i8*, i64, i64)** %15 
  %16 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %14, i32 0, i32 1 
  store  i8* %12, i8** %16 
  %17 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %14, i32 0, i32 0 
  %18 = load  i64 (i8*, i64, i64)*, i64 (i8*, i64, i64)** %17 
  %19 = getelementptr  {i64 (i8*, i64, i64)*, i8*}, {i64 (i8*, i64, i64)*, i8*}* %14, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i64  %18(i8*  %20, i64  3, i64  4)  
  ret i32 0 
}
