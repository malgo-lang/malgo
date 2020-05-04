source_filename = "./examples/polytuple.mlg"
; ModuleID = './examples/polytuple.mlg'


 


define external ccc  {}* @f0({i8**, i64}* , i8* )    {
  %3 = getelementptr  {i8**, i64}, {i8**, i64}* %0, i32 0, i32 0 
  %4 = load  i8**, i8*** %3 
  %5 = getelementptr  i8*, i8** %4, i64 0 
  store  i8* %1, i8** %5 
  ret {}* undef 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 10 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i64* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64*, i64}* 
  %6 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  store  i64* %3, i64** %6 
  %7 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  store  i64 10, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 10 
  br i1 %10, label %body_0, label %end_0 
body_0:
  %11 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %12 = load  i64*, i64** %11 
  %13 = getelementptr  i64, i64* %12, i64 %9 
  store  i64 1, i64* %13 
  %14 = add   i64 %9, 1 
  store  i64 %14, i64* %8 
  br label %cond_0 
end_0:
  %15 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %16 = load  i64*, i64** %15 
  %17 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  %18 = load  i64, i64* %17 
  %19 = mul   i64 ptrtoint (i8** getelementptr inbounds (i8*, i8** inttoptr (i32 0 to i8**), i32 1) to i64), %18 
  %20 =  call ccc  i8*  @GC_malloc(i64  %19)  
  %21 = bitcast i8* %20 to i8** 
  %22 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8**, i64}* getelementptr inbounds ({i8**, i64}, {i8**, i64}* inttoptr (i32 0 to {i8**, i64}*), i32 1) to i64))  
  %23 = bitcast i8* %22 to {i8**, i64}* 
  %24 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 0 
  store  i8** %21, i8*** %24 
  %25 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 1 
  store  i64 %18, i64* %25 
  %26 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 0 
  %27 = load  i8**, i8*** %26 
  %28 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 1 
  store  i64 %18, i64* %28 
  %29 = alloca i64 
  store  i64 0, i64* %29 
  br label %cond_1 
cond_1:
  %30 = load  i64, i64* %29 
  %31 = icmp slt i64 %30, %18 
  br i1 %31, label %body_1, label %end_1 
body_1:
  %32 = getelementptr  i64, i64* %16, i64 %30 
  %33 = load  i64, i64* %32 
  %34 = inttoptr i64 %33 to i8* 
  %35 = getelementptr  i8*, i8** %27, i64 %30 
  store  i8* %34, i8** %35 
  %36 = add   i64 %30, 1 
  store  i64 %36, i64* %29 
  br label %cond_1 
end_1:
  %37 = inttoptr i64 10 to i8* 
  %38 =  call ccc  {}*  @f0({i8**, i64}*  %23, i8*  %37)  
  %39 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 0 
  %40 = load  i8**, i8*** %39 
  %41 = getelementptr  {i8**, i64}, {i8**, i64}* %23, i32 0, i32 1 
  %42 = load  i64, i64* %41 
  %43 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), %42 
  %44 =  call ccc  i8*  @GC_malloc(i64  %43)  
  %45 = bitcast i8* %44 to i64* 
  %46 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %47 = bitcast i8* %46 to {i64*, i64}* 
  %48 = getelementptr  {i64*, i64}, {i64*, i64}* %47, i32 0, i32 0 
  store  i64* %45, i64** %48 
  %49 = getelementptr  {i64*, i64}, {i64*, i64}* %47, i32 0, i32 1 
  store  i64 %42, i64* %49 
  %50 = getelementptr  {i64*, i64}, {i64*, i64}* %47, i32 0, i32 0 
  %51 = load  i64*, i64** %50 
  %52 = getelementptr  {i64*, i64}, {i64*, i64}* %47, i32 0, i32 1 
  store  i64 %42, i64* %52 
  %53 = alloca i64 
  store  i64 0, i64* %53 
  br label %cond_2 
cond_2:
  %54 = load  i64, i64* %53 
  %55 = icmp slt i64 %54, %42 
  br i1 %55, label %body_2, label %end_2 
body_2:
  %56 = getelementptr  i8*, i8** %40, i64 %54 
  %57 = load  i8*, i8** %56 
  %58 = ptrtoint i8* %57 to i64 
  %59 = getelementptr  i64, i64* %51, i64 %54 
  store  i64 %58, i64* %59 
  %60 = add   i64 %54, 1 
  store  i64 %60, i64* %53 
  br label %cond_2 
end_2:
  %61 = ptrtoint i8* %37 to i64 
  %62 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %63 = bitcast i8* %62 to {}* 
  ret i32 0 
}
