; ModuleID = './examples/test15.mlg'
source_filename = "./examples/test15.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @g1(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*, i64)* @f0, {}* (i8*, i64)** %6 
  %7 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {{}* (i8*, i64)*, i8*}* 
  %10 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  {}* (i8*, i64)* @g1, {}* (i8*, i64)** %10 
  %11 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = icmp slt i64 %1, 0 
  %13 = alloca {}* 
  br i1 %12, label %then_0, label %else_0 
then_0:
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {}* 
  store  {}* %15, {}** %13 
  br label %end_0 
else_0:
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {}* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{}* (i8*, i64)*, i8*}*, {}*}* getelementptr inbounds ({{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* inttoptr (i32 0 to {{{}* (i8*, i64)*, i8*}*, {}*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {{{}* (i8*, i64)*, i8*}*, {}*}* 
  %20 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 0 
  store  {{}* (i8*, i64)*, i8*}* %5, {{}* (i8*, i64)*, i8*}** %20 
  %21 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 1 
  store  {}* %17, {}** %21 
  %22 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 0 
  %23 = load  {{}* (i8*, i64)*, i8*}*, {{}* (i8*, i64)*, i8*}** %22 
  %24 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 1 
  %25 = load  {}*, {}** %24 
  %26 = sub   i64 %1, 1 
  %27 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %23, i32 0, i32 0 
  %28 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %27 
  %29 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %23, i32 0, i32 1 
  %30 = load  i8*, i8** %29 
  %31 =  call ccc  {}*  %28(i8*  %30, i64  %26)  
  store  {}* %31, {}** %13 
  br label %end_0 
end_0:
  %32 = load  {}*, {}** %13 
  ret {}* %32 
}


define external ccc  {}* @f0(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*, i64)* @f0, {}* (i8*, i64)** %6 
  %7 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {{}* (i8*, i64)*, i8*}* 
  %10 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  {}* (i8*, i64)* @g1, {}* (i8*, i64)** %10 
  %11 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = icmp slt i64 %1, 0 
  %13 = alloca {}* 
  br i1 %12, label %then_0, label %else_0 
then_0:
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {}* 
  store  {}* %15, {}** %13 
  br label %end_0 
else_0:
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {}* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{}* (i8*, i64)*, i8*}*, {}*}* getelementptr inbounds ({{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* inttoptr (i32 0 to {{{}* (i8*, i64)*, i8*}*, {}*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {{{}* (i8*, i64)*, i8*}*, {}*}* 
  %20 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 0 
  store  {{}* (i8*, i64)*, i8*}* %9, {{}* (i8*, i64)*, i8*}** %20 
  %21 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 1 
  store  {}* %17, {}** %21 
  %22 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 0 
  %23 = load  {{}* (i8*, i64)*, i8*}*, {{}* (i8*, i64)*, i8*}** %22 
  %24 = getelementptr  {{{}* (i8*, i64)*, i8*}*, {}*}, {{{}* (i8*, i64)*, i8*}*, {}*}* %19, i32 0, i32 1 
  %25 = load  {}*, {}** %24 
  %26 = sub   i64 %1, 1 
  %27 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %23, i32 0, i32 0 
  %28 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %27 
  %29 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %23, i32 0, i32 1 
  %30 = load  i8*, i8** %29 
  %31 =  call ccc  {}*  %28(i8*  %30, i64  %26)  
  store  {}* %31, {}** %13 
  br label %end_0 
end_0:
  %32 = load  {}*, {}** %13 
  ret {}* %32 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {}* (i8*, i64)* @f0, {}* (i8*, i64)** %6 
  %7 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  %10 = bitcast {}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {{}* (i8*, i64)*, i8*}* 
  %13 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  store  {}* (i8*, i64)* @g1, {}* (i8*, i64)** %13 
  %14 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14 
  %15 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  %16 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %15 
  %17 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  %18 = load  i8*, i8** %17 
  %19 =  call ccc  {}*  %16(i8*  %18, i64  5)  
  ret i32 0 
}
