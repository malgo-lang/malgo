; ModuleID = './examples/polyfun2.mlg'
source_filename = "./examples/polyfun2.mlg"


 


define external ccc  i8* @$fo128(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i8*)*, i8*}* 
  %4 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %5 = load  i64 (i8*, i8*)*, i64 (i8*, i8*)** %4 
  %6 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %7 = load  i8*, i8** %6 
  %8 =  call ccc  i64  %5(i8*  %7, i8*  %1)  
  %9 = inttoptr i64 %8 to i8* 
  ret i8* %9 
}


define external ccc  i64 @$fo83(i8* , i8* )    {
  %3 = bitcast i8* %0 to {i64 (i8*, i64)*, i8*}* 
  %4 = ptrtoint i8* %1 to i64 
  %5 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 0 
  %6 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %5 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %3, i32 0, i32 1 
  %8 = load  i8*, i8** %7 
  %9 =  call ccc  i64  %6(i8*  %8, i64  %4)  
  ret i64 %9 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int10(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  i8* @GC_malloc(i64)    


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


define external ccc  i8* @f0({{i8* (i8*, i8*)*, i8*}*, i8*}* )    {
  %2 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %0, i32 0, i32 0 
  %3 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %2 
  %4 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %0, i32 0, i32 1 
  %5 = load  i8*, i8** %4 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 0 
  %7 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %6 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8 
  %10 =  call ccc  i8*  %7(i8*  %9, i8*  %5)  
  ret i8* %10 
}


define external ccc  i64 @addOne6(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @addOne6, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 = add   i64 %1, 1 
  ret i64 %8 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
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
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  %10 = bitcast {}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i64 (i8*, i64)*, i8*}* 
  %13 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  store  i64 (i8*, i64)* @addOne6, i64 (i8*, i64)** %13 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14 
  %15 = bitcast {i64 (i8*, i64)*, i8*}* %12 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i8*)*, i8*}* getelementptr inbounds ({i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i64 (i8*, i8*)*, i8*}* 
  %18 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %17, i32 0, i32 0 
  store  i64 (i8*, i8*)* @$fo83, i64 (i8*, i8*)** %18 
  %19 = getelementptr  {i64 (i8*, i8*)*, i8*}, {i64 (i8*, i8*)*, i8*}* %17, i32 0, i32 1 
  store  i8* %15, i8** %19 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {{i8* (i8*, i8*)*, i8*}*, i64}* 
  %22 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %21, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %5, {i8* (i8*, i8*)*, i8*}** %22 
  %23 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %21, i32 0, i32 1 
  store  i64 1, i64* %23 
  %24 = mul   i64 ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %25 =  call ccc  i8*  @GC_malloc(i64  %24)  
  %26 = bitcast i8* %25 to {{i8* (i8*, i8*)*, i8*}*, i64}** 
  %27 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %28 = bitcast i8* %27 to {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %29 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}** %26, {{i8* (i8*, i8*)*, i8*}*, i64}*** %29 
  %30 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 1 
  store  i64 1, i64* %30 
  %31 = alloca i64 
  store  i64 0, i64* %31 
  br label %cond_0 
cond_0:
  %32 = load  i64, i64* %31 
  %33 = icmp slt i64 %32, 1 
  br i1 %33, label %body_0, label %end_0 
body_0:
  %34 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  %35 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %34 
  %36 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %35, i64 %32 
  store  {{i8* (i8*, i8*)*, i8*}*, i64}* %21, {{i8* (i8*, i8*)*, i8*}*, i64}** %36 
  %37 = add   i64 %32, 1 
  store  i64 %37, i64* %31 
  br label %cond_0 
end_0:
  %38 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}* getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}*), i32 1) to i64))  
  %39 = bitcast i8* %38 to {{i64 (i8*, i8*)*, i8*}*, i64}* 
  %40 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %39, i32 0, i32 0 
  store  {i64 (i8*, i8*)*, i8*}* %17, {i64 (i8*, i8*)*, i8*}** %40 
  %41 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %39, i32 0, i32 1 
  store  i64 1, i64* %41 
  %42 = mul   i64 ptrtoint ({{i64 (i8*, i8*)*, i8*}*, i64}** getelementptr inbounds ({{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** inttoptr (i32 0 to {{i64 (i8*, i8*)*, i8*}*, i64}**), i32 1) to i64), 1 
  %43 =  call ccc  i8*  @GC_malloc(i64  %42)  
  %44 = bitcast i8* %43 to {{i64 (i8*, i8*)*, i8*}*, i64}** 
  %45 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* getelementptr inbounds ({{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* inttoptr (i32 0 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}*), i32 1) to i64))  
  %46 = bitcast i8* %45 to {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* 
  %47 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %46, i32 0, i32 0 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}** %44, {{i64 (i8*, i8*)*, i8*}*, i64}*** %47 
  %48 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %46, i32 0, i32 1 
  store  i64 1, i64* %48 
  %49 = alloca i64 
  store  i64 0, i64* %49 
  br label %cond_1 
cond_1:
  %50 = load  i64, i64* %49 
  %51 = icmp slt i64 %50, 1 
  br i1 %51, label %body_1, label %end_1 
body_1:
  %52 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %46, i32 0, i32 0 
  %53 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %52 
  %54 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %53, i64 %50 
  store  {{i64 (i8*, i8*)*, i8*}*, i64}* %39, {{i64 (i8*, i8*)*, i8*}*, i64}** %54 
  %55 = add   i64 %50, 1 
  store  i64 %55, i64* %49 
  br label %cond_1 
end_1:
  %56 = getelementptr  {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i8* (i8*, i8*)*, i8*}*, i64}**, i64}* %28, i32 0, i32 0 
  %57 = load  {{i8* (i8*, i8*)*, i8*}*, i64}**, {{i8* (i8*, i8*)*, i8*}*, i64}*** %56 
  %58 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %57, i64 0 
  %59 = load  {{i8* (i8*, i8*)*, i8*}*, i64}*, {{i8* (i8*, i8*)*, i8*}*, i64}** %58 
  %60 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %61 = bitcast i8* %60 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %62 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %59, i32 0, i32 0 
  %63 = load  {i8* (i8*, i8*)*, i8*}*, {i8* (i8*, i8*)*, i8*}** %62 
  %64 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %61, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %63, {i8* (i8*, i8*)*, i8*}** %64 
  %65 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i64}, {{i8* (i8*, i8*)*, i8*}*, i64}* %59, i32 0, i32 1 
  %66 = load  i64, i64* %65 
  %67 = inttoptr i64 %66 to i8* 
  %68 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %61, i32 0, i32 1 
  store  i8* %67, i8** %68 
  %69 =  call ccc  i8*  @f0({{i8* (i8*, i8*)*, i8*}*, i8*}*  %61)  
  %70 = ptrtoint i8* %69 to i64 
  %71 =  call ccc  {}*  @print_int10(i64  %70)  
  %72 = getelementptr  {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}, {{{i64 (i8*, i8*)*, i8*}*, i64}**, i64}* %46, i32 0, i32 0 
  %73 = load  {{i64 (i8*, i8*)*, i8*}*, i64}**, {{i64 (i8*, i8*)*, i8*}*, i64}*** %72 
  %74 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %73, i64 0 
  %75 = load  {{i64 (i8*, i8*)*, i8*}*, i64}*, {{i64 (i8*, i8*)*, i8*}*, i64}** %74 
  %76 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i8* (i8*, i8*)*, i8*}*, i8*}* getelementptr inbounds ({{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* inttoptr (i32 0 to {{i8* (i8*, i8*)*, i8*}*, i8*}*), i32 1) to i64))  
  %77 = bitcast i8* %76 to {{i8* (i8*, i8*)*, i8*}*, i8*}* 
  %78 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %75, i32 0, i32 0 
  %79 = load  {i64 (i8*, i8*)*, i8*}*, {i64 (i8*, i8*)*, i8*}** %78 
  %80 = bitcast {i64 (i8*, i8*)*, i8*}* %79 to i8* 
  %81 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, i8*}* getelementptr inbounds ({i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, i8*}*), i32 1) to i64))  
  %82 = bitcast i8* %81 to {i8* (i8*, i8*)*, i8*}* 
  %83 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %82, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$fo128, i8* (i8*, i8*)** %83 
  %84 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %82, i32 0, i32 1 
  store  i8* %80, i8** %84 
  %85 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %77, i32 0, i32 0 
  store  {i8* (i8*, i8*)*, i8*}* %82, {i8* (i8*, i8*)*, i8*}** %85 
  %86 = getelementptr  {{i64 (i8*, i8*)*, i8*}*, i64}, {{i64 (i8*, i8*)*, i8*}*, i64}* %75, i32 0, i32 1 
  %87 = load  i64, i64* %86 
  %88 = inttoptr i64 %87 to i8* 
  %89 = getelementptr  {{i8* (i8*, i8*)*, i8*}*, i8*}, {{i8* (i8*, i8*)*, i8*}*, i8*}* %77, i32 0, i32 1 
  store  i8* %88, i8** %89 
  %90 =  call ccc  i8*  @f0({{i8* (i8*, i8*)*, i8*}*, i8*}*  %77)  
  %91 = ptrtoint i8* %90 to i64 
  %92 =  call ccc  {}*  @print_int10(i64  %91)  
  ret i32 0 
}
