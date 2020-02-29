; ModuleID = './examples/test3.mlg'
source_filename = "./examples/test3.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @f0(i8* , {}* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*, {}*)*, i8*}* 
  %6 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*, {}*)* @f0, {}* (i8*, {}*)** %6 
  %7 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  ret {}* %1 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*, {}*)*, i8*}* 
  %6 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*, {}*)* @f0, {}* (i8*, {}*)** %6 
  %7 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  %10 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 0 
  %11 = load  {}* (i8*, {}*)*, {}* (i8*, {}*)** %10 
  %12 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 1 
  %13 = load  i8*, i8** %12 
  %14 =  call ccc  {}*  %11(i8*  %13, {}*  %9)  
  %15 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 0 
  %16 = load  {}* (i8*, {}*)*, {}* (i8*, {}*)** %15 
  %17 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %5, i32 0, i32 1 
  %18 = load  i8*, i8** %17 
  %19 =  call ccc  {}*  %16(i8*  %18, {}*  %14)  
  ret i32 0 
}
