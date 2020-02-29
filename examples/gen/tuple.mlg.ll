; ModuleID = './examples/tuple.mlg'
source_filename = "./examples/tuple.mlg"


 


define external ccc  i8* @snd_str5({i64, i8*}* )    {
  %2 = getelementptr  {i64, i8*}, {i64, i8*}* %0, i32 0, i32 0 
  %3 = load  i64, i64* %2 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %0, i32 0, i32 1 
  %5 = load  i8*, i8** %4 
  ret i8* %5 
}


declare external ccc  {}* @println(i8*)    


define external ccc  {}* @println2(i8* )    {
  %2 =  call ccc  {}*  @println(i8*  %0)  
  ret {}* %2 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int1(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  {}* @newline()    


define external ccc  {}* @newline3()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


define external ccc  i64 @fst_int4({i64, i8*}* )    {
  %2 = getelementptr  {i64, i8*}, {i64, i8*}* %0, i32 0, i32 0 
  %3 = load  i64, i64* %2 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %0, i32 0, i32 1 
  %5 = load  i8*, i8** %4 
  ret i64 %3 
}


declare external ccc  void @GC_init()    


@$globle_str_48 =  unnamed_addr  constant [15 x i8] c" is the answer\00"


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64, i8*}* 
  %3 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 0 
  store  i64 42, i64* %3 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 1 
  store  i8* getelementptr inbounds ([15 x i8], [15 x i8]* @$globle_str_48, i32 0, i32 0), i8** %4 
  %5 =  call ccc  i64  @fst_int4({i64, i8*}*  %2)  
  %6 =  call ccc  {}*  @print_int1(i64  %5)  
  %7 =  call ccc  i8*  @snd_str5({i64, i8*}*  %2)  
  %8 =  call ccc  {}*  @println2(i8*  %7)  
  ret i32 0 
}
