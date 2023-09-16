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
		  in \x^n -> case x^n { p^n -> t, S@(alts', x^n) }
	F({ .h # -> t, alts })
		= let alts' = F({alts})
		  in { .h : t, alts' } // Merge objects.
	F({ q (# p^n) -> t, alts })
		= let alts' = F({alts})
		  in \x^n -> case x^n { p^n -> { F({ q # -> t}), S@(alts', p^n) }, S@(alts', x^n) }
	F({ q (.h #) -> t, alts })
		= let alts' = F({alts})
		  in { .h : { F({q # -> t}), S.(.h, alts') }, alts' }

	S@(e, x^n) is a static application. e is LambdaCase formed as `\y^n -> case y^n { p^n_1 -> t_1, ... }` and x is a expression. S@ returns `p1[x^n/y^n] -> t1[x^n/y^n], ...`
	S.(x, e) is a static access. x is a label and e is a Object.

	After flattening, we need to compile case expressions to switch expressions.
	It could be done with the F function, but it is easier to do it in a separate and well-known algorithm (e.g. Construct a dicision tree)

Examples:

	F({ # (Cons x xs) -> x, # Nil -> 0 })
	=> let alts' = F({ # Nil -> 0 })
	   in \a -> case a { (Cons x xs) -> x, S@(alts', a) }
	=> let alts' = \b -> case b { Nil -> 0 }
	   in \a -> case a { (Cons x xs) -> x, S@(alts', a) }
	=> let alts' = \b -> case b { Nil -> 0 }
	   in \a -> case a { (Cons x xs) -> x, Nil -> 0 }

	F({ .fst # -> 0, .snd # -> 1 })
	=> let alts' = F({ .snd # -> 1 })
	   in { .fst : 0, alts'}
	=> let alts' = { .snd : 1 }
	   in { .fst : 0, alts' }
	=> { .fst : 0, .snd : 1 }

	F({ .head (# (Cons x xs)) -> x, .head (# Nil) -> 0 })
	=> let alts' = F({ .head (# Nil) -> 0 })
	   in \a -> case a { (Cons x xs) -> { F({.head # -> x}), S@(alts', (Cons x xs)) }, S@(alts', a) }
	=> let alts' = \b -> case b { Nil -> { .head : 0 } }
	   in \a -> case a { (Cons x xs) -> { .head : x, S@(alts', (Cons x xs)) }, S@(alts', a) }
	// S@(\b -> case b { Nil -> { .head : 0 } }, (Cons x xs)) => {}
	=> let alts' = \b -> case b { Nil -> { .head : 0 } }
	   in \a -> case a { (Cons x xs) -> { .head : x }, Nil -> { .head : 0 } }

	F({ # (Cons x Nil) -> x, # (Cons x xs) -> f xs, # Nil -> 0 })
	=> let alts' = F({ # (Cons x xs) -> f xs, # Nil -> 0 })
	   in \a -> case a { (Cons x Nil) -> x, S@(alts', a) }

		F({ # (Cons x xs) -> f xs, # Nil -> 0 })
		=> let alts' = F({ # Nil -> 0 })
		   in \b -> case b { (Cons x xs) -> f xs, S@(alts', b) }
		=> let alts' = \c -> case c { Nil -> 0 }
		   in \b -> case b { (Cons x xs) -> f xs, S@(alts', b) }
		=> \b -> case b { (Cons x xs) -> f xs, Nil -> 0 }

	=> let alts' = \b -> case b { (Cons x xs) -> f xs, Nil -> 0 }
	   in \a -> case a { (Cons x Nil) -> x, S@(alts', a) }
	=> \a -> case a { (Cons x Nil) -> x, (Cons x xs) -> f xs, Nil -> 0 }
*/
