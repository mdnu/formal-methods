/*O(log n) algorithm for finding the n-th fibonacci number. 
note: int calculates up to fib(44), long calculates up to fib(93)*/

import java.util.Scanner;

class fibonacciNumbers {
	
	public static int n,x,y;
	
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		n = in.nextInt();
		P();
		System.out.println(x);
	}
	
	public static void P() {
		if (n==0) {
			x = 0; y = 1;
		} else {
			if (n%2 == 0) {
				C();
			} else {
				B();
			}
		}
	}
	
	public static void B() {
		n = (n-1)/2;
		P();
		D();
	}
	
	public static void C() {
		n = (n/2)-1;
		P();
		E();
	}
	
	public static void D() {
		n = x;
		x = (x*x) + (y*y);
		y = (2*n*y) + (y*y);
	}
	
	public static void E() {
		n = x;
		x = (2*x*y) + (y*y);
		y = (n*n) + (y*y) + x;
	}
}

/* 
Refinements:
P <= if n=0 then x:=0 . y:= 1 else (if even n then (even n && n>0 => P) else (odd n => P) fi) fi
odd n=>P <= n:=(n-1)/2 . P . x'=x^2 + y^2 && y' = 2*x*y + y^2
even n && n>0=>P <= n:=n/2 - 1 . P . x'=2*x*y + y^2 && y'=x^2 + y^2 + x'
x' = x^2 + y^2 && y' = 2*x*2 + y^2 <= n:= x . x:= x^2 + y^2 . y:= 2*n*y + y^2
x' = 2*x*y + y^2 && y'=x^2 + y^2 + x' <= n:=x . n:=2*x*y + y^2 . y:=n^2 + y^2 + x 

P = P
B = odd n=>P
C = even n && n>0=>P
D = x' = x^2 + y^2 && y' = 2*x*y + y^2
E = x' = 2*x*y + y^2 && y'=x^2 + y^2 + x'

Compiler:

P <= if n=0 then x:=0 . y:= 1 else (if even n then C else B fi) fi
B <= n:=(n-1)/2 . P . D
C <= n:=n/2 - 1 . P . E
D <= n:= x . x:= x^2 + y^2 . y:= 2*n*y + y^2
E <= n:=x . x:=2*x*y + y^2 . y:=n^2 + y^2 + x 
 */
