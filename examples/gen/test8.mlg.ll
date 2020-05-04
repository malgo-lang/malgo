source_filename = "./examples/test8.mlg"
; ModuleID = './examples/test8.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i64 @g3(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64, {i64 (i8*, i64)*, i8*}*}* 
  %4 = getelementptr  {i64, {i64 (i8*, i64)*, i8*}*}, {i64, {i64 (i8*, i64)*, i8*}*}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 = getelementptr  {i64, {i64 (i8*, i64)*, i8*}*}, {i64, {i64 (i8*, i64)*, i8*}*}* %3, i32 0, i32 1 
  %7 = load  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %6 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64 (i8*, i64)*, i8*}* 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  i64 (i8*, i64)* @g3, i64 (i8*, i64)** %10 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  %13 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %12 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 =  call ccc  i64  %13(i8*  %15, i64  %5)  
  %17 = add   i64 %16, %1 
  ret i64 %17 
}


define external ccc  i64 @f1(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @f1, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %1, %5 
  ret i64 %10 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64}* 
  %3 = getelementptr  {i64}, {i64}* %2, i32 0, i32 0 
  store  i64 42, i64* %3 
  %4 = bitcast {i64}* %2 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i64 (i8*, i64)*, i8*}* 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  store  i64 (i8*, i64)* @f1, i64 (i8*, i64)** %7 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  store  i8* %4, i8** %8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, {i64 (i8*, i64)*, i8*}*}* getelementptr inbounds ({i64, {i64 (i8*, i64)*, i8*}*}, {i64, {i64 (i8*, i64)*, i8*}*}* inttoptr (i32 0 to {i64, {i64 (i8*, i64)*, i8*}*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i64, {i64 (i8*, i64)*, i8*}*}* 
  %11 = getelementptr  {i64, {i64 (i8*, i64)*, i8*}*}, {i64, {i64 (i8*, i64)*, i8*}*}* %10, i32 0, i32 0 
  store  i64 42, i64* %11 
  %12 = getelementptr  {i64, {i64 (i8*, i64)*, i8*}*}, {i64, {i64 (i8*, i64)*, i8*}*}* %10, i32 0, i32 1 
  store  {i64 (i8*, i64)*, i8*}* %6, {i64 (i8*, i64)*, i8*}** %12 
  %13 = bitcast {i64, {i64 (i8*, i64)*, i8*}*}* %10 to i8* 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i64 (i8*, i64)*, i8*}* 
  %16 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 0 
  store  i64 (i8*, i64)* @g3, i64 (i8*, i64)** %16 
  %17 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 1 
  store  i8* %13, i8** %17 
  %18 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 0 
  %19 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %18 
  %20 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %15, i32 0, i32 1 
  %21 = load  i8*, i8** %20 
  %22 =  call ccc  i64  %19(i8*  %21, i64  4)  
  %23 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  %24 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %23 
  %25 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  %26 = load  i8*, i8** %25 
  %27 =  call ccc  i64  %24(i8*  %26, i64  5)  
  %28 = add   i64 %22, %27 
  ret i32 0 
}
