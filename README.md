
Notes on Formal Methods 
========================================
i.e. Fun times with formal logic- CSC465/CSC2104.

We assume familiarity with mathematical logic. (see Enderton's text).
Since the text for which this work is based on is freely available online,
we'll leave this README as a concise summary of its material.

===
Binary Theory
===

Convention we employ:

	'Theorems' are 'Tautologies'.
	'Antitheorems' are 'Contradictions'.

Our logical connectives

	¬ ∧ v ⇒ ⇐ = and ≠. 
	
For completion, the truth table used is this:

		| TT | T⊥ | ⊥T | ⊥⊥|
	---------------------------
	| ∧|    T|   ⊥|  ⊥|   ⊥|
	| ∨|    T|   T|   T|   ⊥|
	| ⇒|    T|   ⊥|   T|   T|
	| ⇐|     T|   T|   ⊥|   T|
	| = |   T|   ⊥|   ⊥|   T|
	| ≠ |   ⊥|   T|   T|   ⊥|
	---------------------------

and we introduce the conditional composition:

						| TTT	| TT⊥	| T⊥T	| T⊥⊥	| ⊥TT	| ⊥T⊥	| ⊥⊥T	| ⊥⊥⊥ |
	-----------------------------------------------------------------------------------
	| if then else fi 	|  T	|  T	|  ⊥	|  ⊥	|  T	|  ⊥	|  T	|  ⊥  |
	-----------------------------------------------------------------------------------

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
					
On the Axiom Rule:

	The only axiom in Binary Theory is T and the only antiaxiom is ⊥. So by the axiom rule, 
	T is a theorem and ⊥ is an antitheorem. We'll introduce new axioms for new theories that
	we'll introduce. Using these given axioms, and the rules of deduction, we can find new
	theorems and antitheorems in the new theory. 
	
On the Evaluation Rule & Completion Rule:

	Consider "Tvx". It contains an unclassified binary subexpression (subformula).
	So we can't use the E.R. to classify this sentence. If x were a theorem, E.R. would say
	that "Tvx" is a theorem. However, if x were an antitheorem, the E.R. would also say that
	"Tvx" is a theorem. so we can use the Completion Rule to determine that the whole expression
	is a theorem.
	
	We don't need to know whether a subexpression is classified or not to be able to use the
	C.R. Any conclusion we arrive by using the C.R. is valid, even if we suppose subexpressions
	to be unclassified.
	
On the Consistency Rule:

	Suppose we had a classified binary expression (i.e. it assumes a-priori a truth value).
	Consider a subexpression of this binary expression. Suppose we were to classify this
	subexpression, but in doing so we get an inconsistency, then we should classify the
	subexpression with the opposite class. (If you hadn't guessed by now, 'classify' means
	to assign a truth value). This makes sense.
	
	Example: let A,B be two binary expressions. Suppose A⇒B is a theorem and A is a theorem.
	Clearly if B were an antitheorem, we'd have that A⇒B were an antitheorem, so we must have
	that B is also a theorem (B is in the class of theorems).

A word of note:
Here, we're working with complete theories. However, in an incomplete theory, we might have
that for some binary expression A, we could have that neither A nor ¬A are theorems. So an
"antitheorem" isn't the same colloquially as "not a theorem". ¬A is not a theorem, but it
certainly isn't an antitheorem either, since it is unclassified given an incomplete theory.


===
Writing Proofs in FM
===

First, let's explicitly state our rules of precedence.

	0		T, ⊥, (), {}, [], <>, if fi, do od, numbers, texts, names
	1		@, juxtaposition
	2		prefix- ¢, $, ↔, #, *, ~, Δ, √, superscript, subscript
	3		×, /, ∩
	4		+, infix-, +(small), ∪
	5		;, ;..., '
	6		,, ,.., |, <|, |>
	7		=, ≠, <, >, ≤, ≥, :, ::, ∈, ⊆
	8		¬
	9		∧
	10		v
	11		⇒, ⇐
	12		:=, !, ?
	13		exit, when, go to, wait until, assert, ensure, or
	14		., ||, result
	15		∀, ∃, Σ, Π, §, LIM, MAX, MIN, var, ivar, chan, frame
	16		≡, ⇒(big), ⇐(big).
	
So for now, with the symbols we know, let's clarify the preference order we're concerned with.
In order from highest to lowest:
	
	T, ⊥, (), if, fi, numbers, texts names,...
	¢, ↔, +, ;..., ,.., =, ≠, <, >, ≤, ≥, ¬, ∧, v, ⇒, ⇐, ≡, ⇒(big), ⇐(big)
	
A proof is a binary expression that is "clearly a theorem". i.e:
	
		expression0		hint 0
	=	expression 1	hint 1
	=	expression 2	hint 2
	= 	expression 3
	
	which is an equivalent way of writing:
	
		expression0 = expression1
	∧	expression1 = expression2
	∧	expression 2 = expression3
	
A formal proof is a proof in which every step fits the form of the law given as a hint.
Our reason for prefering formal proof is that in this format, each step can be checked by a
computer, and so its validity is ensured. We'll go through some examples:

	Proving the 'Law of Portation': a∧b⇒c ≡ a⇒(b⇒c)
	
		a∧b⇒c		Material Implication
	≡	¬(a∧b)vc	Duality
	≡	¬av¬bvc		Material Implication
	≡	a⇒¬bvc		Material Implication
	≡	a⇒(b⇒c)		

	in a different form:
	
		(a∧b⇒c ≡ a⇒(b⇒c))			M.I. 3 times.
	≡	(¬(a∧b)vc ≡ ¬av(¬bvc))		Duality
	≡	(¬av¬bvc ≡ ¬av¬bvc)			Reflexivity of ≡
	≡	T
	
Note that not all proofs can be converted from one form to another.
The following can't be converted:

		(a⇒b ≡ (a∧b))=a			Associative Law for =
	≡	(a⇒b ≡ (a∧b≡a))			a Law of Inclusion
	≡	T
	
Note that hints are optional, since the proofs aren't meant for computer interpretation
but for human-reading.

===
Monotonicity & Antimonotonicity
===

	Convention: a⇒b is read as "a is stronger than or equal to b"
	and likewise, b⇐a is read as "b is weaker than or equal to a".
	
	Convention:
	⊥ is stronger than ⊤
	
	Proof strategy:
	Using the rules of FM;
	Showing that a statement is "stronger" than ⊥ is equivalent to
	showing that it is false.
	Likewise, showing that a statement is "weaker" than ⊤ is equivalent
	to showing that it is true.

	Applying monotonicity to instantiation:
	In the below, we're assuming that we instantiate these variables with
	statements which are "stronger" or "weaker" than the original.
	
	¬a is antimonotonic in a
	a∧b is monotonic in a and monotonic in b
	a∨b is monotonic in a and monotonic in b
	a⇒b is antimonotonic in a and monotonic in b
	a⇐b is monotonic in a and antimonotonic in b
	"if a then b else c fi" is monotonic in b and monotonic in c
	
	An example:
	Since a⇒b is antimonotonic in a and monotonic in b, suppose we
	instantiate a with a "stronger" statement. for example, ⊥⇒a
	so replace a with ⊥
	from a⇒b to ⊥⇒b. So ⊥⇒b is "weaker" than a⇒b. i.e.
	(⊥⇒b)⇐(a⇒b).
	
===
Context Rules
===

	A list of context rules for proof:
	∧ v ⇒ ⇐ = and ≠ ¬
	
	In exp0∧exp1, when changing exp0, we can assume exp1.
	In exp0∧exp1, when changing exp1, we can assume exp0.
	
	In exp0vexp1, when changing exp0, we can assume ¬exp1.
	In exp0vexp1, when changing exp1, we can assume ¬exp0.
	
	In exp0⇒exp1, when changing exp0, we can assume ¬exp1.
	In exp0⇒exp1, when changing exp1, we can assume exp0.
	
	In exp0⇐exp1, when changing exp0, we can assume exp1.
	In exp0⇐exp1, when changing exp1, we can assume ¬exp0.
	
	In if exp0 then exp1 else exp2 fi, when changing exp0, we can assume
	exp1≠exp2.
	In if exp0 then exp1 else exp2 fi, when changing exp1, we can assume
	exp0.
	In if exp0 then exp1 else exp2 fi, when changing exp2, we can assume
	¬exp0.
	
	
===
Number-syntactic theory.
===
	
	called 'number theory' in the text, not the same as number theory.
	Everything here is standard.
	

===
'Bunch'-data Theory
===
	
===
'Set'-data Theory
===

===
'String'-data Theory
===

===
'List'-data Theory
===

===
Function-programming Theory
===

===
Quantifiers
===

===
Function Fine Points
===

===
List as Function
===

===
Limits and Reals
===