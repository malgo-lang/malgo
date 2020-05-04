source_filename = "./examples/test2.mlg"
; ModuleID = './examples/test2.mlg'


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @f0(i8* )    {
  %2 = bitcast i8* %0 to {}* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*)*, i8*}* getelementptr inbounds ({{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* inttoptr (i32 0 to {{}* (i8*)*, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {{}* (i8*)*, i8*}* 
  %5 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %4, i32 0, i32 0 
  store  {}* (i8*)* @f0, {}* (i8*)** %5 
  %6 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %4, i32 0, i32 1 
  store  i8* %0, i8** %6 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {}* 
  ret {}* %8 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*)*, i8*}* getelementptr inbounds ({{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* inttoptr (i32 0 to {{}* (i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*)*, i8*}* 
  %6 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*)* @f0, {}* (i8*)** %6 
  %7 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %5, i32 0, i32 0 
  %9 = load  {}* (i8*)*, {}* (i8*)** %8 
  %10 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %5, i32 0, i32 1 
  %11 = load  i8*, i8** %10 
  %12 =  call ccc  {}*  %9(i8*  %11)  
  ret i32 0 
}
