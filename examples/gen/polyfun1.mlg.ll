; ModuleID = './examples/polyfun1.mlg'
source_filename = "./examples/polyfun1.mlg"


 


define external ccc  i8* @$fo108(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i64 (i8*, i8*)*, i64 (i8*, i8*)** %4 
  %6 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i64  %5(i8*  %7, i8*  %1)  
  %9 = inttoptr i64 %8 to i8* 
  ret i8* %9 
}


define external ccc  i64 @$fo81(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i64)*, i8*}* 
  %4 = ptrtoint i8* %1 to i64 
  %5 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %5 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i64  %6(i8*  %8, i64  %4)  
  ret i64 %9 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i8* @$fo57(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %4 = bitcast i8* %1 to {i8*, i8*}* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo64, i8* (i8*, i8*)** %11 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %6, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %10, {i8* (i8*, i8*)*, i8*}** %13 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %6, i32 0, i32 1 
  store  i8* %15, i8** %16 
  %17 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %3, i32 0, i32 0 
  %18 = load  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %17 
  %19 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %3, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i8*  %18(i8*  %20, {{i8* (i8*, i8*)*, i8*}*, i8*}*  %6)  
  ret i8* %21 
}


define external ccc  i8* @$fo64(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i8* (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i8*  %5(i8*  %7, i8*  %1)  
  ret i8* %8 
}


define external ccc  i64 @succ1(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64 (i8*, i64)*, i8*}* 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %10 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = add   i64 %1, 1 
  ret i64 %12 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int7(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


define external ccc  i8* @f0(i8* , {{i8* (i8*, i8*)*, i8*}*, i8*}* )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64 (i8*, i64)*, i8*}* 
  %10 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %10 
  %11 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %1, i32 0, i32 0 
  %13 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %12 
  %14 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %1, i32 0, i32 1 
  %15 = load  i8*, i8** %14 
  %16 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 0 
  %17 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %16 
  %18 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 1 
  %19 = load  i8*, i8** %18 
  %20 =  call ccc  i8*  %17(i8*  %19, i8*  %15)  
  ret i8* %20 
}


define external ccc  i32 @main()    {
; <label>:0:
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* getelementptr inbounds ({i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 0 
  store  i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)* @f0, i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)** %6 
  %7 = getelementptr  {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}, {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 = bitcast {i8* (i8*, {{i8* (i8*, i8*)*, i8*}*, i8*}*)*, i8*}* %5 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo57, i8* (i8*, i8*)** %11 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  store  i8* %8, i8** %12 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {}* 
  %15 = bitcast {}* %14 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i64 (i8*, i64)*, i8*}* 
  %18 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  store  i64 (i8*, i64)* @succ1, i64 (i8*, i64)** %18 
  %19 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  store  i8* %15, i8** %19 
  %20 = bitcast {i64 (i8*, i64)*, i8*}* %17 to i8* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i8*)*, i8*}* getelementptr inbounds ({i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {i64 (i8*, i8*)*, i8*}* 
  %23 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %22, i32 0, i32 0 
  store  i64 (i8*, i8*)* @$fo81, i64 (i8*, i8*)** %23 
  %24 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %22, i32 0, i32 1 
  store  i8* %20, i8** %24 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %26 = bitcast i8* %25 to {{i64 (i8*, i8*)*, i8*}*, i64}* 
  %27 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %26, i32 0, i32 0 
  store  {i64 (i8*, i8*)*, i8*}* %22, {i64 (i8*, i8*)*, i8*}** %27 
  %28 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %26, i32 0, i32 1 
  store  i64 1, i64* %28 
  %29 = mul   i64 ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %30 =  call ccc  i8*  @GC_malloc(i64  %29)  
  %31 = bitcast i8* %30 to {{i64 (i8*, i8*)*, i8*}*, i64}** 
  %32 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %33 = bitcast i8* %32 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %34 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %33, i32 0, i32 0 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}** %31, {{i64 (i8*, i8*)*, i8*}*, i64}*** %34 
  %35 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %33, i32 0, i32 1 
  store  i64 1, i64* %35 
  %36 = alloca i64 
  store  i64 0, i64* %36 
  br label %cond_0 
cond_0:
  %37 = load  i64, i64* %36 
  %38 = icmp slt i64 %37, 1 
  br i1 %38, label %body_0, label %end_0 
body_0:
  %39 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %33, i32 0, i32 0 
  %40 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %39 
  %41 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %40, i64 %37 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}* %26, {{i64 (i8*, i8*)*, i8*}*, i64}** %41 
  %42 = add   i64 %37, 1 
  store  i64 %42, i64* %36 
  br label %cond_0 
end_0:
  %43 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %33, i32 0, i32 0 
  %44 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %43 
  %45 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %44, i64 0 
  %46 = load  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %45 
  %47 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  %48 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %47 
  %49 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  %50 = load  i8*, i8** %49 
  %51 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %52 = bitcast i8* %51 to {i8*, i8*}* 
  %53 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %46, i32 0, i32 0 
  %54 = load  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %53 
  %55 = bitcast {i64 (i8*, i8*)*, i8*}* %54 to i8* 
  %56 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %57 = bitcast i8* %56 to {i8* (i8*, i8*)*, i8*}* 
  %58 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %57, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo108, i8* (i8*, i8*)** %58 
  %59 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %57, i32 0, i32 1 
  store  i8* %55, i8** %59 
  %60 = bitcast {i8* (i8*, i8*)*, i8*}* %57 to i8* 
  %61 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 0 
  store  i8* %60, i8** %61 
  %62 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %46, i32 0, i32 1 
  %63 = load  i64, i64* %62 
  %64 = inttoptr i64 %63 to i8* 
  %65 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 1 
  store  i8* %64, i8** %65 
  %66 = bitcast {i8*, i8*}* %52 to i8* 
  %67 =  call ccc  i8*  %48(i8*  %50, i8*  %66)  
  %68 = ptrtoint i8* %67 to i64 
  %69 =  call ccc  {}*  @print_int7(i64  %68)  
  ret i32 0 
}
