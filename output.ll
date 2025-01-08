define i32 @main() {
Label0:
%R1 = add i32 0, 58
br label %Label1
Label1:
%R3 = phi i32 [%R1, %Label0], [%R2, %Label3]
br label %Label2
Label2:
%R4 = add i32 0, 45
%R5 = icmp sgt i32 %R3, %R4
br i1 %R5, label %Label3, label %Label4
Label3:
%R2 = add i32 0, 30
br i1 %R5, label %Label1, label %Label4
Label4:
ret i32 %R3
}
