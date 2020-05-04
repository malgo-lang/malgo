source_filename = "./examples/cls.mlg"
; ModuleID = './examples/cls.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i64 @add_inner2(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %5, %1 
  ret i64 %10 
}


define external ccc  {i64 (i8*, i64)*, i8*}* @add0(i64 )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  store  i64 %0, i64* %4 
  %5 = bitcast {i64}* %3 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @add_inner2, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %5, i8** %9 
  ret {i64 (i8*, i64)*, i8*}* %7 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  {i64 (i8*, i64)*, i8*}*  @add0(i64  3)  
  %2 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %1, i32 0, i32 0 
  %3 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %2 
  %4 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %1, i32 0, i32 1 
  %5 = load  i8*, i8** %4 
  %6 =  call ccc  i64  %3(i8*  %5, i64  4)  
  ret i32 0 
}
