class listSummation {
	
	public static int s,n;
	public static int[] array = new int[]{1,2,3,4,5};
	
	public static void A() {
		s = 0; n = 0;
		B();
	}
	
	public static void B() {
		if (n==(array.length));
		else {
			s+=array[n];
			n++;
			B();
		}
	}
	
	public static void main(String[] args) {
		A();
		System.out.print(s);
	}
	
}

/* Compiler view:
	
	A	<=	s:=0 . n:=0 . B
	B	<=	if n=#L then C else D fi
	C	<=	ok
	D	<=	s:=s+Ln . n:=n+1 . B
	
/* Macro-expansion: B <= if n=#L then ok else s:=s+Ln . n:=n+1 . B fi
	(then translate this into high-level code. In our case, Java). */
	