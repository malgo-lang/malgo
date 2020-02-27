; ModuleID = './examples/test16.mlg'
source_filename = "./examples/test16.mlg"


 


define external ccc  i8* @$fo58(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i8*  %5(i8*  %7, i8*  %1)  
  ret i8* %8 
}


define external ccc  i64 @$fo47(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = inttoptr i64 %1 to i8* 
  %5 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %5 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i8*  %6(i8*  %8, i8*  %4)  
  %10 = ptrtoint i8* %9 to i64 
  ret i64 %10 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int8(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i8* @id0(i8* , i8* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  ret i8* %1 
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @id0, i8* (i8*, i8*)** %6 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* 
  %10 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %9, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %5, {i8* (i8*, i8*)*, i8*}** %10 
  %11 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %9, i32 0, i32 1 
  store  {i8* (i8*, i8*)*, i8*}* %5, {i8* (i8*, i8*)*, i8*}** %11 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  %13 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %12 
  %14 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %9, i32 0, i32 0 
  %19 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %18 
  %20 = bitcast {i8* (i8*, i8*)*, i8*}* %19 to i8* 
  %21 = getelementptr  {i8*, i8*}, {i8*, i8*}* %17, i32 0, i32 0 
  store  i8* %20, i8** %21 
  %22 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %9, i32 0, i32 1 
  %23 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %22 
  %24 = bitcast {i8* (i8*, i8*)*, i8*}* %23 to i8* 
  %25 = getelementptr  {i8*, i8*}, {i8*, i8*}* %17, i32 0, i32 1 
  store  i8* %24, i8** %25 
  %26 = bitcast {i8*, i8*}* %17 to i8* 
  %27 =  call ccc  i8*  %13(i8*  %15, i8*  %26)  
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}*), i32 1) to i64))  
  %30 = bitcast i8* %29 to {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* 
  %31 = getelementptr  {i8*, i8*}, {i8*, i8*}* %28, i32 0, i32 0 
  %32 = load  i8*, i8** %31 
  %33 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %34 = bitcast i8* %33 to {i64 (i8*, i64)*, i8*}* 
  %35 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %34, i32 0, i32 0 
  store  i64 (i8*, i64)* @$fo47, i64 (i8*, i64)** %35 
  %36 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %34, i32 0, i32 1 
  store  i8* %32, i8** %36 
  %37 = getelementptr  {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %30, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* %34, {i64 (i8*, i64)*, i8*}** %37 
  %38 = getelementptr  {i8*, i8*}, {i8*, i8*}* %28, i32 0, i32 1 
  %39 = load  i8*, i8** %38 
  %40 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %41 = bitcast i8* %40 to {i8* (i8*, i8*)*, i8*}* 
  %42 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %41, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo58, i8* (i8*, i8*)** %42 
  %43 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %41, i32 0, i32 1 
  store  i8* %39, i8** %43 
  %44 = getelementptr  {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %30, i32 0, i32 1 
  store  {i8* (i8*, i8*)*, i8*}* %41, {i8* (i8*, i8*)*, i8*}** %44 
  %45 = getelementptr  {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %30, i32 0, i32 0 
  %46 = load  {i64 (i8*, i64)*, i8*}*, {i64 (i8*, i64)*, i8*}** %45 
  %47 = getelementptr  {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}, {{i64 (i8*, i64)*, i8*}*, {i8* (i8*, i8*)*, i8*}*}* %30, i32 0, i32 1 
  %48 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %47 
  %49 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %46, i32 0, i32 0 
  %50 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %49 
  %51 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %46, i32 0, i32 1 
  %52 = load  i8*, i8** %51 
  %53 =  call ccc  i64  %50(i8*  %52, i64  1)  
  %54 =  call ccc  {}*  @print_int8(i64  %53)  
  ret i32 0 
}
