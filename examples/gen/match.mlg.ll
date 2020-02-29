; ModuleID = './examples/match.mlg'
source_filename = "./examples/match.mlg"


 


define external ccc  i64 @add0({{i64, i64}*, i64}* )    {
  %2 = getelementptr  {{i64, i64}*, i64}, {{i64, i64}*, i64}* %0, i32 0, i32 0 
  %3 = load  {i64, i64}*, {i64, i64}** %2 
  %4 = getelementptr  {{i64, i64}*, i64}, {{i64, i64}*, i64}* %0, i32 0, i32 1 
  %5 = load  i64, i64* %4 
  %6 = getelementptr  {i64, i64}, {i64, i64}* %3, i32 0, i32 0 
  %7 = load  i64, i64* %6 
  %8 = getelementptr  {i64, i64}, {i64, i64}* %3, i32 0, i32 1 
  %9 = load  i64, i64* %8 
  %10 = add   i64 %7, %9 
  %11 = add   i64 %10, %5 
  ret i64 %11 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i64}* getelementptr inbounds ({i64, i64}, {i64, i64}* inttoptr (i32 0 to {i64, i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64, i64}* 
  %3 = getelementptr  {i64, i64}, {i64, i64}* %2, i32 0, i32 0 
  store  i64 1, i64* %3 
  %4 = getelementptr  {i64, i64}, {i64, i64}* %2, i32 0, i32 1 
  store  i64 2, i64* %4 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64, i64}*, i64}* getelementptr inbounds ({{i64, i64}*, i64}, {{i64, i64}*, i64}* inttoptr (i32 0 to {{i64, i64}*, i64}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {{i64, i64}*, i64}* 
  %7 = getelementptr  {{i64, i64}*, i64}, {{i64, i64}*, i64}* %6, i32 0, i32 0 
  store  {i64, i64}* %2, {i64, i64}** %7 
  %8 = getelementptr  {{i64, i64}*, i64}, {{i64, i64}*, i64}* %6, i32 0, i32 1 
  store  i64 3, i64* %8 
  %9 =  call ccc  i64  @add0({{i64, i64}*, i64}*  %6)  
  ret i32 0 
}
