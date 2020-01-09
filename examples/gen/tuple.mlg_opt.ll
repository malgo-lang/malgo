; ModuleID = './examples/gen/tuple.mlg.ll'
source_filename = "./examples/tuple.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@0 = internal unnamed_addr constant [15 x i8] c" is the answer\00"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int1(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @println(i8*) local_unnamed_addr

define {}* @println2(i8*) local_unnamed_addr {
  %2 = tail call {}* @println(i8* %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline3() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

; Function Attrs: norecurse nounwind readonly
define i64 @fst_int4({ i64, i8* }* nocapture readonly) local_unnamed_addr #0 {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i64 0, i32 0
  %3 = load i64, i64* %2, align 8
  ret i64 %3
}

; Function Attrs: norecurse nounwind readonly
define i8* @snd_str5({ i64, i8* }* nocapture readonly) local_unnamed_addr #0 {
  %2 = getelementptr { i64, i8* }, { i64, i8* }* %0, i64 0, i32 1
  %3 = load i8*, i8** %2, align 8
  ret i8* %3
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64*
  store i64 42, i64* %2, align 8
  %3 = getelementptr i8, i8* %1, i64 8
  %4 = bitcast i8* %3 to i8**
  store i8* getelementptr inbounds ([15 x i8], [15 x i8]* @0, i64 0, i64 0), i8** %4, align 8
  %5 = tail call {}* @print_int(i64 42)
  %6 = load i8*, i8** %4, align 8
  %7 = tail call {}* @println(i8* %6)
  ret i32 0
}

attributes #0 = { norecurse nounwind readonly }
