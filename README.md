
Notes on Formal Methods 
========================================
i.e. Fun times with formal logic- CSC465/CSC2104.

We assume familiarity with mathematical logic. (see Enderton's text).
'Binary' Theory
===
"Binary Expressions" same as 'logical expressions'.
i.e. they can be atomic formulas/sentences, or built up using
atomic formulas/sentences to make molecular formulas.

	'Theorems' are 'Tautologies'.
	'Antitheorems' are 'Contradictions'.

Here, we define two special atomic formulas, T and ⊥ which are mapped via their 
truth functions to 'Truth and 'Falsity' respectively. Hence, in our language, 
T is our universal 'theorem' and ⊥ is our universal 'antitheorem'.

We also employ the following convention:
First, we may call T "top" and ⊥ "bottom". This is independent of
an "application area" - i.e. a model (recall that a model is an interpretation
of the semantic meaning of sentences in a logical structure). We'll see why this
is useful below. 

	Our logical connectives are ∧ ∨ ⇒ ⇐ = and !=. 
	(i.e. two-operand binary operators). with a one-operand binary operator ¬.

We should know all the truth tables by hand at this point.
It would be useful to review "logic with trees" by Howson for a convenient
shorthand way to parse truth values for molecular formulas using trees.

Some new material having to do with these operators:
	
	Recall that we called T "top" and ⊥ "bottom". If we remember our truth tables,
	we'd see that ∧ gives the minimum of two operands, and ∨ gives the maximum.
	See: T∧⊥, ⊥∧T and ⊥∧⊥ will always evaluate to ⊥, while ⊥∨⊥ is the only case 
	for this operator which evaluates to ⊥ (in all other cases, we evaluate to T). 
	So our convention's use becomes obvious.

	Likewise, we may also show that ⇒ forms a partial ordering on our operands:
	Indeed, if a,b,c are any operands, we have that a⇒a, (reflexivity),
	(a⇒b)∧(b⇒a) = (a=b) (antisymmetric) and (a⇒b)∧(b⇒c)⇒(a⇒c) (transitive).
	
	The convention we introduce here is that "a⇒b" in terms of the ordering may
	be interpreted as "a is stronger than or equal to b". Likewise, (a⇐b) interprets 
	as "a is weaker than or equal to b". 
	Conveniently, the "⇐" looks like "<=" i.e. ≤ in most programming languages, 
	so the interpretation seems natural.
	
We'll soon make much use of this notion of "strength/weakness" w.r.t. operands, when we
introduce "monotonicity" and "antimonotonicity".
	
In our formal language (let's call it FM), we employ parentheses simplification
using a precedence table for the connectives. (we don't do this in mathematical logic).
This is extremely annoying, but whatever. The precedence table is in the text.

	A theory (i.e, a model) is consistent iff no binary expression is both a theorem
	and an antitheorem. i.e. it says that a theory is consistent (has no contradictions).

	A theory is complete iff every 'fully instantiated binary expression' is either
	a theorem or an antitheorem. This is the same as the completeness theorem in 
	standard logic. i.e. Every logically valid formula in our model has a finite deduction.

We introduce our method of deduction. (here, we call it 'proof rules').
We only have five rules, from which we can determine whether an expression is a theorem
or an antitheorem. 

Let X be a binary expression (could be atomic, or molecular)

	1. Axiom Rule (Trivial) - If X is an axiom, then it is a theorem.
				i.e. sentences we specify in our language as axioms are
				equivalent to tautologies.
				Otherwise, if it's an antiaxiom, then it's an antitheorem.
	
	2. Evaluation Rule. - If all binary subexpressions of a binary expression
					are classified, then it is classified according to the
					truth tables.
					
					i.e. If all atomic formulas of a molecular formula have
					been specified a truth assignment, then the molecular 
					formula is specified according to how its particular
					distribution of truth values over its atomic formulas
					evaluates via the standard truth tables.
					
	3. Completion Rule. - If a binary expression contains unclassified binary
					subexpressions, and all ways of classifying the subexpressions
					places the binary expression in some class, then it is
					of that class.
					
					i.e. if a molecular formula has constituent atomic formulas
					or molecular subformulas that have no truth assignment, but
					that for all distributions of truth values over all its
					subformulas has it that the molecular formula evaluates
					to some truth value, then it must assume that truth value.
					
					example: (AvB), let A be a theorem. Even if B is unclassified,
					then (AvB) is still a theorem, since all possible distributions
					of truth values over B will still have (AvB) evaluate to true.
					
	4. Consistency Rule. - If a classified binary expression contains binary
						subexpressions, and the -only one- way of classifying
						them is consistent, then they are classified that way.
						
					i.e. If a molecular formula which has already been assigned a
					truth value contains subformulas which are unclassfied, then
					the only possible classification for those unclassified subexpressions
					must be the truth value assignments which evaluates the molecular
					formula to its current truth value.
					
	5. Instance rule. - If a binary expression is classified, then all its instances
					have that same classification.
					
					i.e. If an expression has an assigned truth value, then for any
					occurence of this expression in a sentence, it must always retain
					this assigned truth value.
					

					
===
'Bunch' Theory
===
a collection of objects. unpackaged, unindexed.
This is the 'simplest' data structure in our language.
	
Note - "package" just means set. hence a bunch would be the singletons of some
"package" set without regard for the set. Equivalently, and in being formal, we
could consuder bunches simply as elements in our language that are not contained
in a set. In mathematics, you'd normally consider every object to be contained in
some set, or objects being defined as some set with some properties. In our present
context, we'll have to abandon this notion and adopt a formalist perspective.
	
	Any number, character, binary value or set is an 'elementary bunch'/'element'.

	Unions of bunches is represented by a comma:
		A,B is the bunch of elements from A or B.
		
	Intersections of bunches is rep. by an apostrophe:
		A'B is the bunch of elements from A and B.
		
	¢A is the cardinality of A.
	
	A is a subset of B is represented by:
		A: B

Axioms that describe bunches:
	
	x:y = x=y				- elementary axiom
	x: A,B = x:A ∨ x:B		- compound axiom
	A,A = A					- idempotent
	A,B = B,A 				- symmetry	
	A,(B,C) = (A,B),C 		- associativity
	A'A = A 				- symmetry
	A'B = B'A 				- symmetry
	A'(B'C) = (A'B)'C 		- associativity
	A,B:C = A:C ∧ B:C 		- antidistributivity
	A: B'C = A:B ∧ A:C 		- distributivity
	A:A,B 					- generalization
	A'B:A 					- specialization

this is all basic set theory.
	
	A: A 							- reflexivity
	A:B ∧ B:A = A=B					- antisymmetry
	A:B ∧ B:C ⇒ A:C					- transitivity
	
- recall that any relation that is reflexive, transitive
and antisymmetric is called a 'partial-ordering', or 'ordering'.
	
	¢x = 1 							- size
	¢(A,B) + ¢(A'B) = ¢(A)+¢(B)		- size
	¬x:A ⇒ ¢(A'x)=0					- size
	A:B ⇒ ¢A ≤ ¢B
	
again, these are all trivial.
	
From these, we have some laws: (Theorems derived from the above axioms)

	A,(A'B) = A 					- absorption
	A'(A,B) = A 					- absorption
	A:B ⇒ C,A: C,B 					- monotonicity
	A:B ⇒ C'A: C'B 					- monotonicity
	A:B = (A,B = B) = (A = A'B)		- inclusion
	A,(B,C) = (A,B),(A,C)			- distributivity
	A,(B,C) = (A,B),(A,C)			- distributivity
	A'(B,C) = (A'B),(A'C)			- distributivity
	A'(B'C) = (A'B),(A'C)			- distributivity
	A:B ∧ C:D ⇒ A,C:B,D				- conflation
	A:B ∧ C:D ⇒ A'C:B'D				- conflation
	
Bunches that have special names: (i.e. we'll be using them a lot)

	null 							- the empty bunch
	bin = T⊥						- the binary values
	nat = 0,1,2,...					- 'natural' numbers (shouldn't include 0, but w/e)
	int = ...-2,-1,0,1,2...			- integers
	rat = ...-1,0,2/3,...			- rationals
	real = ..., 2^(1/2),...			- reals
	xnat = 0,1,2,..., ∞				- extended naturals
	xint = -∞,...int...,∞			- extended integers
	xrat = -∞,...rat...,∞			- extended rationals
	xreal = -∞, ... ,∞				- extended reals
	char = ...,"a", "A", ...		- character values
	
An added convention:
	
	x,...y 	means "x to y" only for x:int and y:xint for x≤y
						this is equivalent to [x,y) ‎⊂ Z
						so i:x,...y  =  x‎≤i<y (This is an axiom in our FL).
	
	¢(x,...y) = y-x
	

Meta-Properties of "Bunch" Theory.

Most operators distribute over bunches. 
example: -(1,3,7) = -1,-3, -7. 

Bunches under the binary operation + has the sum operation behave like multiplication
over the parentheses. i.e:
	
	(1,2) + (10,20) = 11, 12, 21, 22.
	(1,2) + 10 = 11, 12
	1 + 10 = 11
	null + 10 = null
	
Notice above that null behaves like '0' under '+' behaving like multiplication. 
The only case that stands out is the third, where + actually does behave like addition
that's because the 'bunches' here are singletons. To illustrate, let's perform
the analogous operation: (1,0)+(10,0) = 11,0,0,0. Simplify and you'll get 1+10=11.

Distributivity lets us define sets rather nicely:

	nat+2 = 2,3,4,5,6,...
	nat×2 = 0,2,4,6,8,...
	nat^2 = 0,1,4,9,16,...
	2^nat = 1,2,4,8,16,...
	
