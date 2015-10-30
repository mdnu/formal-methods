/* Given rational variables x,z and natural variable, write a program 
for z' = x^y that runs fast without using exponentiation. 
Note: This is provably the fastest program which performs exponentiation.*/

import java.util.Scanner;

class fastExponentiation {
	
	public static int x,y,z;
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		x = in.nextInt(); y = in.nextInt();
		z = 1;
		B();
		System.out.print(z);
	}
	
	public static void B() {
		if (y%2 == 0) {
			C();
		} else {
			D();
		}
	}
	
	public static void C() {
		if (y==0);
		else {
			E();
		}
	}
	
	public static void D() {
		z = z*x; y--; C();
	}
	
	public static void E() {
		x*=x; y/=2; F();
	}
	
	public static void F() {
		if (y%2==0) {
			E();
		} else {
			D();
		}
	}
}

/* 
Refinements:

(z'=x^y) <= z:=1 . P
P <= if (even y) then (even y =>P) else (odd y => P) fi
(even y => P) <= if (y=0) then ok else (even y && y>0 => P) fi
(odd y => P) <= z:= z*x . y:= y-1 . (even y => P)
(even y && y>0 => P) <= x:=x*x . y:=y/2 . (y>0 => P)
(y>0 => P) <= if (even y) then (even y && y>0 => P) else (odd y => P) fi

Compiler view:

A	<= z:=1 . B
B	<= if (even y) then C else D fi
C	<= if (y=0) then ok else E fi
D	<= z:= z*x . y:= y-1 . C
E	<= x:=x*x . y:=y/2 . F
F	<= if (even y) then E else D fi 
*/
