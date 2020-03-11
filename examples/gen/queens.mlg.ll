; ModuleID = './examples/queens.mlg'
source_filename = "./examples/queens.mlg"


 


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {}* @try11(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* 
  %4 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 0 
  %5 = load  {i64*, i64}*, {i64*, i64}** %4 
  %6 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 1 
  %7 = load  {i64*, i64}*, {i64*, i64}** %6 
  %8 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 2 
  %9 = load  {i64*, i64}*, {i64*, i64}** %8 
  %10 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 3 
  %11 = load  i64, i64* %10 
  %12 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 4 
  %13 = load  {{}* (i8*)*, i8*}*, {{}* (i8*)*, i8*}** %12 
  %14 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %3, i32 0, i32 5 
  %15 = load  {i64*, i64}*, {i64*, i64}** %14 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {{}* (i8*, i64)*, i8*}* 
  %18 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  store  {}* (i8*, i64)* @try11, {}* (i8*, i64)** %18 
  %19 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  store  i8* %0, i8** %19 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* getelementptr inbounds ({i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* inttoptr (i32 0 to {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* 
  %22 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 0 
  store  i64 %1, i64* %22 
  %23 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 1 
  store  {i64*, i64}* %5, {i64*, i64}** %23 
  %24 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 2 
  store  {i64*, i64}* %7, {i64*, i64}** %24 
  %25 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 3 
  store  {i64*, i64}* %9, {i64*, i64}** %25 
  %26 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 4 
  store  i64 %11, i64* %26 
  %27 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 5 
  store  {i64*, i64}* %15, {i64*, i64}** %27 
  %28 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21, i32 0, i32 6 
  store  {{}* (i8*, i64)*, i8*}* %17, {{}* (i8*, i64)*, i8*}** %28 
  %29 = bitcast {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %21 to i8* 
  %30 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %31 = bitcast i8* %30 to {{}* (i8*, i64)*, i8*}* 
  %32 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %31, i32 0, i32 0 
  store  {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %32 
  %33 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %31, i32 0, i32 1 
  store  i8* %29, i8** %33 
  %34 = icmp eq i64 %1, %11 
  %35 = alloca {}* 
  br i1 %34, label %then_0, label %else_0 
then_0:
  %36 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %13, i32 0, i32 0 
  %37 = load  {}* (i8*)*, {}* (i8*)** %36 
  %38 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %13, i32 0, i32 1 
  %39 = load  i8*, i8** %38 
  %40 =  call ccc  {}*  %37(i8*  %39)  
  store  {}* %40, {}** %35 
  br label %end_0 
else_0:
  %41 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %31, i32 0, i32 0 
  %42 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %41 
  %43 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %31, i32 0, i32 1 
  %44 = load  i8*, i8** %43 
  %45 =  call ccc  {}*  %42(i8*  %44, i64  0)  
  store  {}* %45, {}** %35 
  br label %end_0 
end_0:
  %46 = load  {}*, {}** %35 
  ret {}* %46 
}


define external ccc  {}* @printboard6(i8* )    {
  %2 = bitcast i8* %0 to {{i64*, i64}*, i64}* 
  %3 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %2, i32 0, i32 0 
  %4 = load  {i64*, i64}*, {i64*, i64}** %3 
  %5 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %2, i32 0, i32 1 
  %6 = load  i64, i64* %5 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*)*, i8*}* getelementptr inbounds ({{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* inttoptr (i32 0 to {{}* (i8*)*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {{}* (i8*)*, i8*}* 
  %9 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %8, i32 0, i32 0 
  store  {}* (i8*)* @printboard6, {}* (i8*)** %9 
  %10 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %8, i32 0, i32 1 
  store  i8* %0, i8** %10 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64*, i64}*, i64}* getelementptr inbounds ({{i64*, i64}*, i64}, {{i64*, i64}*, i64}* inttoptr (i32 0 to {{i64*, i64}*, i64}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {{i64*, i64}*, i64}* 
  %13 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %12, i32 0, i32 0 
  store  {i64*, i64}* %4, {i64*, i64}** %13 
  %14 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %12, i32 0, i32 1 
  store  i64 %6, i64* %14 
  %15 = bitcast {{i64*, i64}*, i64}* %12 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {{}* (i8*, i64)*, i8*}* 
  %18 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  store  {}* (i8*, i64)* @loopi7, {}* (i8*, i64)** %18 
  %19 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  store  i8* %15, i8** %19 
  %20 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  %21 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %20 
  %22 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  %23 = load  i8*, i8** %22 
  %24 =  call ccc  {}*  %21(i8*  %23, i64  0)  
  ret {}* %24 
}


declare external ccc  {}* @print(i8*)    


define external ccc  {}* @print0(i8* )    {
  %2 =  call ccc  {}*  @print(i8*  %0)  
  ret {}* %2 
}


@$globle_str_353 =  unnamed_addr  constant [3 x i8] c" O\00"


@$globle_str_354 =  unnamed_addr  constant [3 x i8] c" .\00"


@$globle_str_355 =  unnamed_addr  constant [2 x i8] c"\0a\00"


define external ccc  {}* @loopj9(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i64*, i64}*, i64, i64}* 
  %4 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %3, i32 0, i32 0 
  %5 = load  {i64*, i64}*, {i64*, i64}** %4 
  %6 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %3, i32 0, i32 1 
  %7 = load  i64, i64* %6 
  %8 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %3, i32 0, i32 2 
  %9 = load  i64, i64* %8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {{}* (i8*, i64)*, i8*}* 
  %12 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %11, i32 0, i32 0 
  store  {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %12 
  %13 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %11, i32 0, i32 1 
  store  i8* %0, i8** %13 
  %14 = icmp slt i64 %1, %9 
  %15 = alloca {}* 
  br i1 %14, label %then_0, label %else_1 
then_0:
  %16 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %17 = load  i64*, i64** %16 
  %18 = getelementptr  i64, i64* %17, i64 %7 
  %19 = load  i64, i64* %18 
  %20 = icmp eq i64 %19, %1 
  %21 = alloca i8* 
  br i1 %20, label %then_1, label %else_0 
then_1:
  store  i8* getelementptr inbounds ([3 x i8], [3 x i8]* @$globle_str_353, i32 0, i32 0), i8** %21 
  br label %end_0 
else_0:
  store  i8* getelementptr inbounds ([3 x i8], [3 x i8]* @$globle_str_354, i32 0, i32 0), i8** %21 
  br label %end_0 
end_0:
  %22 = load  i8*, i8** %21 
  %23 =  call ccc  {}*  @print0(i8*  %22)  
  %24 = add   i64 %1, 1 
  %25 =  call ccc  {}*  @loopj9(i8*  %0, i64  %24)  
  store  {}* %25, {}** %15 
  br label %end_1 
else_1:
  %26 =  call ccc  {}*  @print0(i8*  getelementptr inbounds ([2 x i8], [2 x i8]* @$globle_str_355, i32 0, i32 0))  
  store  {}* %26, {}** %15 
  br label %end_1 
end_1:
  %27 = load  {}*, {}** %15 
  ret {}* %27 
}


@$globle_str_356 =  unnamed_addr  constant [2 x i8] c"\0a\00"


define external ccc  {}* @loopi7(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {{i64*, i64}*, i64}* 
  %4 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %3, i32 0, i32 0 
  %5 = load  {i64*, i64}*, {i64*, i64}** %4 
  %6 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %3, i32 0, i32 1 
  %7 = load  i64, i64* %6 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {{}* (i8*, i64)*, i8*}* 
  %10 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 0 
  store  {}* (i8*, i64)* @loopi7, {}* (i8*, i64)** %10 
  %11 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %9, i32 0, i32 1 
  store  i8* %0, i8** %11 
  %12 = icmp slt i64 %1, %7 
  %13 = alloca {}* 
  br i1 %12, label %then_0, label %else_0 
then_0:
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64*, i64}*, i64, i64}* getelementptr inbounds ({{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* inttoptr (i32 0 to {{i64*, i64}*, i64, i64}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {{i64*, i64}*, i64, i64}* 
  %16 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %15, i32 0, i32 0 
  store  {i64*, i64}* %5, {i64*, i64}** %16 
  %17 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %15, i32 0, i32 1 
  store  i64 %1, i64* %17 
  %18 = getelementptr  {{i64*, i64}*, i64, i64}, {{i64*, i64}*, i64, i64}* %15, i32 0, i32 2 
  store  i64 %7, i64* %18 
  %19 = bitcast {{i64*, i64}*, i64, i64}* %15 to i8* 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {{}* (i8*, i64)*, i8*}* 
  %22 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %21, i32 0, i32 0 
  store  {}* (i8*, i64)* @loopj9, {}* (i8*, i64)** %22 
  %23 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %21, i32 0, i32 1 
  store  i8* %19, i8** %23 
  %24 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %21, i32 0, i32 0 
  %25 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %24 
  %26 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %21, i32 0, i32 1 
  %27 = load  i8*, i8** %26 
  %28 =  call ccc  {}*  %25(i8*  %27, i64  0)  
  %29 = add   i64 %1, 1 
  %30 =  call ccc  {}*  @loopi7(i8*  %0, i64  %29)  
  store  {}* %30, {}** %13 
  br label %end_0 
else_0:
  %31 =  call ccc  {}*  @print0(i8*  getelementptr inbounds ([2 x i8], [2 x i8]* @$globle_str_356, i32 0, i32 0))  
  store  {}* %31, {}** %13 
  br label %end_0 
end_0:
  %32 = load  {}*, {}** %13 
  ret {}* %32 
}


define external ccc  {}* @loop13(i8* , i64 )    {
; <label>:2:
  %3 = bitcast i8* %0 to {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* 
  %4 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 1 
  %7 = load  {i64*, i64}*, {i64*, i64}** %6 
  %8 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 2 
  %9 = load  {i64*, i64}*, {i64*, i64}** %8 
  %10 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 3 
  %11 = load  {i64*, i64}*, {i64*, i64}** %10 
  %12 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 4 
  %13 = load  i64, i64* %12 
  %14 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 5 
  %15 = load  {i64*, i64}*, {i64*, i64}** %14 
  %16 = getelementptr  {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}, {i64, {i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {i64*, i64}*, {{}* (i8*, i64)*, i8*}*}* %3, i32 0, i32 6 
  %17 = load  {{}* (i8*, i64)*, i8*}*, {{}* (i8*, i64)*, i8*}** %16 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {{}* (i8*, i64)*, i8*}* 
  %20 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %19, i32 0, i32 0 
  store  {}* (i8*, i64)* @loop13, {}* (i8*, i64)** %20 
  %21 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %19, i32 0, i32 1 
  store  i8* %0, i8** %21 
  %22 = icmp slt i64 %1, %13 
  %23 = alloca {}* 
  br i1 %22, label %then_0, label %else_1 
then_0:
  %24 = getelementptr  {i64*, i64}, {i64*, i64}* %15, i32 0, i32 0 
  %25 = load  i64*, i64** %24 
  %26 = getelementptr  i64, i64* %25, i64 %1 
  %27 = load  i64, i64* %26 
  %28 = icmp eq i64 %27, 0 
  %29 = add   i64 %1, %5 
  %30 = getelementptr  {i64*, i64}, {i64*, i64}* %9, i32 0, i32 0 
  %31 = load  i64*, i64** %30 
  %32 = getelementptr  i64, i64* %31, i64 %29 
  %33 = load  i64, i64* %32 
  %34 = icmp eq i64 %33, 0 
  %35 = and i1 %28, %34 
  %36 = add   i64 %1, 7 
  %37 = sub   i64 %36, %5 
  %38 = getelementptr  {i64*, i64}, {i64*, i64}* %11, i32 0, i32 0 
  %39 = load  i64*, i64** %38 
  %40 = getelementptr  i64, i64* %39, i64 %37 
  %41 = load  i64, i64* %40 
  %42 = icmp eq i64 %41, 0 
  %43 = and i1 %35, %42 
  %44 = alloca {}* 
  br i1 %43, label %then_1, label %else_0 
then_1:
  %45 = getelementptr  {i64*, i64}, {i64*, i64}* %15, i32 0, i32 0 
  %46 = load  i64*, i64** %45 
  %47 = getelementptr  i64, i64* %46, i64 %1 
  store  i64 1, i64* %47 
  %48 = add   i64 %1, %5 
  %49 = getelementptr  {i64*, i64}, {i64*, i64}* %9, i32 0, i32 0 
  %50 = load  i64*, i64** %49 
  %51 = getelementptr  i64, i64* %50, i64 %48 
  store  i64 1, i64* %51 
  %52 = add   i64 %1, 7 
  %53 = sub   i64 %52, %5 
  %54 = getelementptr  {i64*, i64}, {i64*, i64}* %11, i32 0, i32 0 
  %55 = load  i64*, i64** %54 
  %56 = getelementptr  i64, i64* %55, i64 %53 
  store  i64 1, i64* %56 
  %57 = getelementptr  {i64*, i64}, {i64*, i64}* %7, i32 0, i32 0 
  %58 = load  i64*, i64** %57 
  %59 = getelementptr  i64, i64* %58, i64 %5 
  store  i64 %1, i64* %59 
  %60 = add   i64 %5, 1 
  %61 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 0 
  %62 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %61 
  %63 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %17, i32 0, i32 1 
  %64 = load  i8*, i8** %63 
  %65 =  call ccc  {}*  %62(i8*  %64, i64  %60)  
  %66 = getelementptr  {i64*, i64}, {i64*, i64}* %15, i32 0, i32 0 
  %67 = load  i64*, i64** %66 
  %68 = getelementptr  i64, i64* %67, i64 %1 
  store  i64 0, i64* %68 
  %69 = add   i64 %1, %5 
  %70 = getelementptr  {i64*, i64}, {i64*, i64}* %9, i32 0, i32 0 
  %71 = load  i64*, i64** %70 
  %72 = getelementptr  i64, i64* %71, i64 %69 
  store  i64 0, i64* %72 
  %73 = add   i64 %1, 7 
  %74 = sub   i64 %73, %5 
  %75 = getelementptr  {i64*, i64}, {i64*, i64}* %11, i32 0, i32 0 
  %76 = load  i64*, i64** %75 
  %77 = getelementptr  i64, i64* %76, i64 %74 
  store  i64 0, i64* %77 
  %78 = add   i64 %1, 1 
  %79 =  call ccc  {}*  @loop13(i8*  %0, i64  %78)  
  store  {}* %79, {}** %44 
  br label %end_0 
else_0:
  %80 = add   i64 %1, 1 
  %81 =  call ccc  {}*  @loop13(i8*  %0, i64  %80)  
  store  {}* %81, {}** %44 
  br label %end_0 
end_0:
  %82 = load  {}*, {}** %44 
  store  {}* %82, {}** %23 
  br label %end_1 
else_1:
  %83 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %84 = bitcast i8* %83 to {}* 
  store  {}* %84, {}** %23 
  br label %end_1 
end_1:
  %85 = load  {}*, {}** %23 
  ret {}* %85 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 8 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i64* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64*, i64}* 
  %6 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  store  i64* %3, i64** %6 
  %7 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  store  i64 8, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 8 
  br i1 %10, label %body_0, label %end_0 
body_0:
  %11 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %12 = load  i64*, i64** %11 
  %13 = getelementptr  i64, i64* %12, i64 %9 
  store  i64 0, i64* %13 
  %14 = add   i64 %9, 1 
  store  i64 %14, i64* %8 
  br label %cond_0 
end_0:
  %15 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 8 
  %16 =  call ccc  i8*  @GC_malloc(i64  %15)  
  %17 = bitcast i8* %16 to i64* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i64*, i64}* 
  %20 = getelementptr  {i64*, i64}, {i64*, i64}* %19, i32 0, i32 0 
  store  i64* %17, i64** %20 
  %21 = getelementptr  {i64*, i64}, {i64*, i64}* %19, i32 0, i32 1 
  store  i64 8, i64* %21 
  %22 = alloca i64 
  store  i64 0, i64* %22 
  br label %cond_1 
cond_1:
  %23 = load  i64, i64* %22 
  %24 = icmp slt i64 %23, 8 
  br i1 %24, label %body_1, label %end_1 
body_1:
  %25 = getelementptr  {i64*, i64}, {i64*, i64}* %19, i32 0, i32 0 
  %26 = load  i64*, i64** %25 
  %27 = getelementptr  i64, i64* %26, i64 %23 
  store  i64 0, i64* %27 
  %28 = add   i64 %23, 1 
  store  i64 %28, i64* %22 
  br label %cond_1 
end_1:
  %29 = add   i64 8, 8 
  %30 = sub   i64 %29, 1 
  %31 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), %30 
  %32 =  call ccc  i8*  @GC_malloc(i64  %31)  
  %33 = bitcast i8* %32 to i64* 
  %34 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %35 = bitcast i8* %34 to {i64*, i64}* 
  %36 = getelementptr  {i64*, i64}, {i64*, i64}* %35, i32 0, i32 0 
  store  i64* %33, i64** %36 
  %37 = getelementptr  {i64*, i64}, {i64*, i64}* %35, i32 0, i32 1 
  store  i64 %30, i64* %37 
  %38 = alloca i64 
  store  i64 0, i64* %38 
  br label %cond_2 
cond_2:
  %39 = load  i64, i64* %38 
  %40 = icmp slt i64 %39, %30 
  br i1 %40, label %body_2, label %end_2 
body_2:
  %41 = getelementptr  {i64*, i64}, {i64*, i64}* %35, i32 0, i32 0 
  %42 = load  i64*, i64** %41 
  %43 = getelementptr  i64, i64* %42, i64 %39 
  store  i64 0, i64* %43 
  %44 = add   i64 %39, 1 
  store  i64 %44, i64* %38 
  br label %cond_2 
end_2:
  %45 = add   i64 8, 8 
  %46 = sub   i64 %45, 1 
  %47 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), %46 
  %48 =  call ccc  i8*  @GC_malloc(i64  %47)  
  %49 = bitcast i8* %48 to i64* 
  %50 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %51 = bitcast i8* %50 to {i64*, i64}* 
  %52 = getelementptr  {i64*, i64}, {i64*, i64}* %51, i32 0, i32 0 
  store  i64* %49, i64** %52 
  %53 = getelementptr  {i64*, i64}, {i64*, i64}* %51, i32 0, i32 1 
  store  i64 %46, i64* %53 
  %54 = alloca i64 
  store  i64 0, i64* %54 
  br label %cond_3 
cond_3:
  %55 = load  i64, i64* %54 
  %56 = icmp slt i64 %55, %46 
  br i1 %56, label %body_3, label %end_3 
body_3:
  %57 = getelementptr  {i64*, i64}, {i64*, i64}* %51, i32 0, i32 0 
  %58 = load  i64*, i64** %57 
  %59 = getelementptr  i64, i64* %58, i64 %55 
  store  i64 0, i64* %59 
  %60 = add   i64 %55, 1 
  store  i64 %60, i64* %54 
  br label %cond_3 
end_3:
  %61 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64*, i64}*, i64}* getelementptr inbounds ({{i64*, i64}*, i64}, {{i64*, i64}*, i64}* inttoptr (i32 0 to {{i64*, i64}*, i64}*), i32 1) to i64))  
  %62 = bitcast i8* %61 to {{i64*, i64}*, i64}* 
  %63 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %62, i32 0, i32 0 
  store  {i64*, i64}* %19, {i64*, i64}** %63 
  %64 = getelementptr  {{i64*, i64}*, i64}, {{i64*, i64}*, i64}* %62, i32 0, i32 1 
  store  i64 8, i64* %64 
  %65 = bitcast {{i64*, i64}*, i64}* %62 to i8* 
  %66 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*)*, i8*}* getelementptr inbounds ({{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* inttoptr (i32 0 to {{}* (i8*)*, i8*}*), i32 1) to i64))  
  %67 = bitcast i8* %66 to {{}* (i8*)*, i8*}* 
  %68 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %67, i32 0, i32 0 
  store  {}* (i8*)* @printboard6, {}* (i8*)** %68 
  %69 = getelementptr  {{}* (i8*)*, i8*}, {{}* (i8*)*, i8*}* %67, i32 0, i32 1 
  store  i8* %65, i8** %69 
  %70 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* getelementptr inbounds ({{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* inttoptr (i32 0 to {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}*), i32 1) to i64))  
  %71 = bitcast i8* %70 to {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* 
  %72 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 0 
  store  {i64*, i64}* %19, {i64*, i64}** %72 
  %73 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 1 
  store  {i64*, i64}* %35, {i64*, i64}** %73 
  %74 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 2 
  store  {i64*, i64}* %51, {i64*, i64}** %74 
  %75 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 3 
  store  i64 8, i64* %75 
  %76 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 4 
  store  {{}* (i8*)*, i8*}* %67, {{}* (i8*)*, i8*}** %76 
  %77 = getelementptr  {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}, {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71, i32 0, i32 5 
  store  {i64*, i64}* %5, {i64*, i64}** %77 
  %78 = bitcast {{i64*, i64}*, {i64*, i64}*, {i64*, i64}*, i64, {{}* (i8*)*, i8*}*, {i64*, i64}*}* %71 to i8* 
  %79 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{}* (i8*, i64)*, i8*}* getelementptr inbounds ({{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %80 = bitcast i8* %79 to {{}* (i8*, i64)*, i8*}* 
  %81 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 0 
  store  {}* (i8*, i64)* @try11, {}* (i8*, i64)** %81 
  %82 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 1 
  store  i8* %78, i8** %82 
  %83 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 0 
  %84 = load  {}* (i8*, i64)*, {}* (i8*, i64)** %83 
  %85 = getelementptr  {{}* (i8*, i64)*, i8*}, {{}* (i8*, i64)*, i8*}* %80, i32 0, i32 1 
  %86 = load  i8*, i8** %85 
  %87 =  call ccc  {}*  %84(i8*  %86, i64  0)  
  ret i32 0 
}
