; ModuleID = './examples/test12.mlg'
source_filename = "./examples/test12.mlg"


 


declare external ccc  {}* @print_bool(i1)    


define external ccc  {}* @print_bool5(i1 )    {
  %2 =  call ccc  {}*  @print_bool(i1  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i1 @odd2(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i1 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i1 (i8*, i64)* @even1, i1 (i8*, i64)** %8 
  %9 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i1 (i8*, i64)*, i8*}* 
  %12 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %11, i32 0, i32 0 
  store  i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %12 
  %13 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 = icmp eq i64 %1, %5 
  %15 = alloca i1 
  br i1 %14, label %then_0, label %else_0 
then_0:
  store  i1 0, i1* %15 
  br label %end_0 
else_0:
  %16 = sub   i64 %1, 1 
  %17 =  call ccc  i1  @even1(i8*  %0, i64  %16)  
  store  i1 %17, i1* %15 
  br label %end_0 
end_0:
  %18 = load  i1, i1* %15 
  ret i1 %18 
}


define external ccc  i1 @even1(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i1 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i1 (i8*, i64)* @even1, i1 (i8*, i64)** %8 
  %9 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i1 (i8*, i64)*, i8*}* 
  %12 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %11, i32 0, i32 0 
  store  i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %12 
  %13 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 = icmp eq i64 %1, %5 
  %15 = alloca i1 
  br i1 %14, label %then_0, label %else_0 
then_0:
  store  i1 1, i1* %15 
  br label %end_0 
else_0:
  %16 = sub   i64 %1, 1 
  %17 =  call ccc  i1  @odd2(i8*  %0, i64  %16)  
  store  i1 %17, i1* %15 
  br label %end_0 
end_0:
  %18 = load  i1, i1* %15 
  ret i1 %18 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64}* 
  %3 = getelementptr  {i64}, {i64}* %2, i32 0, i32 0 
  store  i64 0, i64* %3 
  %4 = bitcast {i64}* %2 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i1 (i8*, i64)*, i8*}* 
  %7 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %6, i32 0, i32 0 
  store  i1 (i8*, i64)* @even1, i1 (i8*, i64)** %7 
  %8 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %6, i32 0, i32 1 
  store  i8* %4, i8** %8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i64}* 
  %11 = getelementptr  {i64}, {i64}* %10, i32 0, i32 0 
  store  i64 0, i64* %11 
  %12 = bitcast {i64}* %10 to i8* 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i1 (i8*, i64)*, i8*}* getelementptr inbounds ({i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i1 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i1 (i8*, i64)*, i8*}* 
  %15 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %14, i32 0, i32 0 
  store  i1 (i8*, i64)* @odd2, i1 (i8*, i64)** %15 
  %16 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %14, i32 0, i32 1 
  store  i8* %12, i8** %16 
  %17 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %14, i32 0, i32 0 
  %18 = load  i1 (i8*, i64)*, i1 (i8*, i64)** %17 
  %19 = getelementptr  {i1 (i8*, i64)*, i8*}, {i1 (i8*, i64)*, i8*}* %14, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i1  %18(i8*  %20, i64  34)  
  %22 =  call ccc  {}*  @print_bool5(i1  %21)  
  ret i32 0 
}
