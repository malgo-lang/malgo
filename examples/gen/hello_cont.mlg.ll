; ModuleID = './examples/hello_cont.mlg'
source_filename = "./examples/hello_cont.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @$fo32(i8* , {}* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = bitcast {}* %1 to i8* 
  %5 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %5 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i8*  %6(i8*  %8, i8*  %4)  
  %10 = bitcast i8* %9 to {}* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {}* 
  ret {}* %12 
}


define external ccc  {}* @println_k1(i8* , {{}* (i8*, {}*)*, i8*}* )    {
  %3 =  call ccc  {}*  @println0(i8*  %0)  
  %4 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %1, i32 0, i32 0 
  %5 = load  {}* (i8*, {}*)*, {}* (i8*, {}*)** %4 
  %6 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %1, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  {}*  %5(i8*  %7, {}*  %3)  
  ret {}* %8 
}


declare external ccc  {}* @println(i8*)    


define external ccc  {}* @println0(i8* )    {
  %2 =  call ccc  {}*  @println(i8*  %0)  
  ret {}* %2 
}


define external ccc  i8* @id4(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  ret i8* %1 
}


declare external ccc  void @GC_init()    


@$globle_str_44 =  unnamed_addr  constant [13 x i8] c"hello, world\00"


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id4, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = bitcast {i8* (i8*, i8*)*, i8*}* %5 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, {}*)*, i8*}* getelementptr inbounds ({{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* inttoptr (i32 0 to {{}* (i8*, {}*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {{}* (i8*, {}*)*, i8*}* 
  %11 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %10, i32 0, i32 0 
  store  {}* (i8*, {}*)* @$fo32, {}* (i8*, {}*)** %11 
  %12 = getelementptr  {{}* (i8*, {}*)*, i8*}, {{}* (i8*, {}*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 =  call ccc  {}*  @println_k1(i8*  getelementptr inbounds ([13 x i8], [13 x i8]* @$globle_str_44, i32 0, i32 0), {{}* (i8*, {}*)*, i8*}*  %10)  
  ret i32 0 
}
