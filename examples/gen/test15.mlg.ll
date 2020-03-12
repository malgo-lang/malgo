; ModuleID = './examples/test15.mlg'
source_filename = "./examples/test15.mlg"


 


define external ccc  {}* @update1({i8**, i64}* , i64 , i8* )    {
  %4 = getelementptr  {i8**, i64}, {i8**, i64}* %0, i32 0, i32 0 
  %5 = load  i8**, i8*** %4 
  %6 = getelementptr  i8*, i8** %5, i64 %1 
  store  i8* %2, i8** %6 
  ret {}* undef 
}


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int5(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  void @GC_init()    


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), 3 
  %2 =  call ccc  i8*  @GC_malloc(i64  %1)  
  %3 = bitcast i8* %2 to i64* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64*, i64}* 
  %6 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  store  i64* %3, i64** %6 
  %7 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  store  i64 3, i64* %7 
  %8 = alloca i64 
  store  i64 0, i64* %8 
  br label %cond_0 
cond_0:
  %9 = load  i64, i64* %8 
  %10 = icmp slt i64 %9, 3 
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
  %17 = getelementptr  i64, i64* %16, i64 1 
  store  i64 2, i64* %17 
  %18 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %19 = load  i64*, i64** %18 
  %20 = getelementptr  i64, i64* %19, i64 2 
  store  i64 3, i64* %20 
  %21 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 0 
  %22 = load  i64*, i64** %21 
  %23 = getelementptr  {i64*, i64}, {i64*, i64}* %5, i32 0, i32 1 
  %24 = load  i64, i64* %23 
  %25 = mul   i64 ptrtoint (i8** getelementptr inbounds (i8*, i8** inttoptr (i32 0 to i8**), i32 1) to i64), %24 
  %26 =  call ccc  i8*  @GC_malloc(i64  %25)  
  %27 = bitcast i8* %26 to i8** 
  %28 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8**, i64}* getelementptr inbounds ({i8**, i64}, {i8**, i64}* inttoptr (i32 0 to {i8**, i64}*), i32 1) to i64))  
  %29 = bitcast i8* %28 to {i8**, i64}* 
  %30 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 0 
  store  i8** %27, i8*** %30 
  %31 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 1 
  store  i64 %24, i64* %31 
  %32 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 0 
  %33 = load  i8**, i8*** %32 
  %34 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 1 
  store  i64 %24, i64* %34 
  %35 = alloca i64 
  store  i64 0, i64* %35 
  br label %cond_1 
cond_1:
  %36 = load  i64, i64* %35 
  %37 = icmp slt i64 %36, %24 
  br i1 %37, label %body_1, label %end_1 
body_1:
  %38 = getelementptr  i64, i64* %22, i64 %36 
  %39 = load  i64, i64* %38 
  %40 = inttoptr i64 %39 to i8* 
  %41 = getelementptr  i8*, i8** %33, i64 %36 
  store  i8* %40, i8** %41 
  %42 = add   i64 %36, 1 
  store  i64 %42, i64* %35 
  br label %cond_1 
end_1:
  %43 = inttoptr i64 42 to i8* 
  %44 =  call ccc  {}*  @update1({i8**, i64}*  %29, i64  0, i8*  %43)  
  %45 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 0 
  %46 = load  i8**, i8*** %45 
  %47 = getelementptr  {i8**, i64}, {i8**, i64}* %29, i32 0, i32 1 
  %48 = load  i64, i64* %47 
  %49 = mul   i64 ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64), %48 
  %50 =  call ccc  i8*  @GC_malloc(i64  %49)  
  %51 = bitcast i8* %50 to i64* 
  %52 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64*, i64}* getelementptr inbounds ({i64*, i64}, {i64*, i64}* inttoptr (i32 0 to {i64*, i64}*), i32 1) to i64))  
  %53 = bitcast i8* %52 to {i64*, i64}* 
  %54 = getelementptr  {i64*, i64}, {i64*, i64}* %53, i32 0, i32 0 
  store  i64* %51, i64** %54 
  %55 = getelementptr  {i64*, i64}, {i64*, i64}* %53, i32 0, i32 1 
  store  i64 %48, i64* %55 
  %56 = getelementptr  {i64*, i64}, {i64*, i64}* %53, i32 0, i32 0 
  %57 = load  i64*, i64** %56 
  %58 = getelementptr  {i64*, i64}, {i64*, i64}* %53, i32 0, i32 1 
  store  i64 %48, i64* %58 
  %59 = alloca i64 
  store  i64 0, i64* %59 
  br label %cond_2 
cond_2:
  %60 = load  i64, i64* %59 
  %61 = icmp slt i64 %60, %48 
  br i1 %61, label %body_2, label %end_2 
body_2:
  %62 = getelementptr  i8*, i8** %46, i64 %60 
  %63 = load  i8*, i8** %62 
  %64 = ptrtoint i8* %63 to i64 
  %65 = getelementptr  i64, i64* %57, i64 %60 
  store  i64 %64, i64* %65 
  %66 = add   i64 %60, 1 
  store  i64 %66, i64* %59 
  br label %cond_2 
end_2:
  %67 = ptrtoint i8* %43 to i64 
  %68 = getelementptr  {i64*, i64}, {i64*, i64}* %53, i32 0, i32 0 
  %69 = load  i64*, i64** %68 
  %70 = getelementptr  i64, i64* %69, i64 0 
  %71 = load  i64, i64* %70 
  %72 =  call ccc  {}*  @print_int5(i64  %71)  
  ret i32 0 
}
