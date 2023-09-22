package flatten

/*
Sketch of the algorithm (flattening nested copatterns):

	x, _ is a variable.
	h is a label.
	p is a pattern.
	q is a copattern. Copatterns may be include patterns as parameters.
	alts is the rest of alternatives.

	F({ # p^n -> t, alts })
		= let alts' = F({alts})
		  in \x^n -> case x^n { p^n -> t, ExpandCase(alts') }
	F({ .h # -> t, alts })
		= let alts' = F({alts})
		  in { .h : t, alts' } // Merge objects.
	F({ q (# p^n) -> t, alts })
		= let alts' = F({alts})
		  in \x^n -> case x^n { p^n -> { F({ q # -> t}), ApplyPattern(alts', p^n) }, ExpandCase(alts') }
	F({ q (.h #) -> t, alts })
		= let alts' = F({alts})
		  in { .h : { F({q # -> t}), Access(alts', .h) }, alts' }

	ExpandCase(e):
		e is LambdaCase formed as `\y^n -> case y^n { p^n_1 -> t_1, ... }`.
		ExpandCase returns `p1 -> t1, ...`.

	ApplyPattern(e, v):
		e is LambdaCase formed as `\y^n -> case y^n { p^n_1 -> t_1, ... }`.
		v is a expression (pattern).
		ApplyPattern returns an object as `{ t1[v/p], ... }`.
		If p matches v, then t1[v/p] is t1 with p replaced by v (each variable in p is replaced by the corresponding variable in v).
		Otherwise, t1 is not included in the result.

	Access(e, x):
		e is an object.
		x is a label.
		Access returns the body of the label x in e.

	After flattening, we need to compile case expressions to switch expressions.
	It could be done with the F function, but it is easier to do it in a separate and well-known algorithm (e.g. Construct a dicision tree)

Examples:

	F({ # (Cons x xs) -> x, # Nil -> 0 })
	=> let alts' = F({ # Nil -> 0 })
	   in \a -> case a { (Cons x xs) -> x, ExpandCase(alts') }
	=> let alts' = \b -> case b { Nil -> 0 }
	   in \a -> case a { (Cons x xs) -> x, ExpandCase(alts') }
	=> \a -> case a { (Cons x xs) -> x, Nil -> 0 }

	F({ .fst # -> 0, .snd # -> 1 })
	=> let alts' = F({ .snd # -> 1 })
	   in { .fst : 0, alts'}
	=> let alts' = { .snd : 1 }
	   in { .fst : 0, alts' }
	=> { .fst : 0, .snd : 1 }

	F({ .head (# (Cons x xs)) -> x, .head (# Nil) -> 0 })
	=> let alts' = F({ .head (# Nil) -> 0 })
	   in \a -> case a { (Cons x xs) -> { F({.head # -> x}), Apply(alts', (Cons x xs)) }, ExpandCase(alts') }
	=> let alts' = \b -> case b { Nil -> { .head : 0 } }
	   in \a -> case a { (Cons x xs) -> { .head : x, Apply(alts', (Cons x xs)) }, ExpandCase(alts') }
	// Apply(\b -> case b { Nil -> { .head : 0 } }, (Cons x xs)) => {}
	=> \a -> case a { (Cons x xs) -> { .head : x }, Nil -> { .head : 0 } }

	F({ # (Cons x Nil) -> x, # (Cons x xs) -> f xs, # Nil -> 0 })
	=> let alts' = F({ # (Cons x xs) -> f xs, # Nil -> 0 })
	   in \a -> case a { (Cons x Nil) -> x, ExpandCase(alts') }

		F({ # (Cons x xs) -> f xs, # Nil -> 0 })
		=> let alts' = F({ # Nil -> 0 })
		   in \b -> case b { (Cons x xs) -> f xs, ExpandCase(alts') }
		=> let alts' = \c -> case c { Nil -> 0 }
		   in \b -> case b { (Cons x xs) -> f xs, ExpandCase(alts') }
		=> \b -> case b { (Cons x xs) -> f xs, Nil -> 0 }

	=> let alts' = \b -> case b { (Cons x xs) -> f xs, Nil -> 0 }
	   in \a -> case a { (Cons x Nil) -> x, ExpandCase(alts') }
	=> \a -> case a { (Cons x Nil) -> x, (Cons x xs) -> f xs, Nil -> 0 }
*/
