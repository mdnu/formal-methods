// Given x,y:nat, write a program for y'=2^x.

import java.util.Scanner;

class binaryExponentiation {
	
	public static int x, y;
	
	public static void A() {
		if (x==0) {
			y = 1; x = 3;
		} else {
			x = x-1; y = 7;
			A();
			y = 2*y; x = 5;
		}
	}
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		x = in.nextInt();
		A();
		System.out.print(y);
	}
	
}

/* Proof:
	Refinements:

	y'=2^x	<=	if x=0 then (x=0 ⇒ y'=2^x) else (x>0 ⇒ y'=2^x) fi
	(x=0 ⇒ y'=2^x)	<=	y:=1 . x:= 3
	(x>0 ⇒ y'=2^x)	<=	(x>0 ⇒ y'=2^(x-1)).(y'=2*y)
	(x>0 ⇒ y'=2^(x-1))	<=	(x'=x-1).y'=2^x
	y'=2*y	<=	(y:=2*y).(x:=5)
	(x'=x-1)	<=	(x:=x-1).(y:=7) */
	
/* Compiler view:
	(translating the above proof into its compilation):
	
	A	<=	if x=0 then B else C fi
	B	<=	y:=1 . x:=3
	C	<=	D.E
	D	<=	F.A
	E	<=	y:=2*y . x:=5
	F	<=	x:=x-1 . y:=7 */
	
/* Macro-expansion: A <= if x=0 then y:=1 . x:=3 else x:=x-1 . y:=7 . A . y:=2*y . x:=5 fi 
	(then translate this into high-level code. In our case, Java). */
