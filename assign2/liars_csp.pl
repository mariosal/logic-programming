:- lib(ic).

genrand(N, List) :-
  length(List, N),
  make_list(N, List).

make_list(_, []).
make_list(N, [X|List]) :-
  random(R),
  X is R mod (N+1),
  make_list(N, List).

flat([], []).
flat([Head|InTail], Out) :-
	flat(Head, FlatHead),
	flat(InTail, OutTail),
	append(FlatHead, OutTail, Out).
flat([Head|InTail], [Head|OutTail]) :-
	Head \= [],
	Head \= [_|_],
  flat(InTail, OutTail).

liars_csp(Claims, Liars) :-
  length(Claims, NumClaims),
  length(Liars, NumClaims),
  Liars #:: [0, 1],
  NumLiars #:: 0..NumClaims,
  constrain(Claims, Liars, NumLiars),
  flat([Liars, NumLiars], Domain),
  search(Domain, 0, input_order, indomain, complete, []).

constrain(Claims, Liars, NumLiars) :-
  NumLiars #= sum(Liars),
  constrainPerClaim(Claims, Liars, NumLiars).

constrainPerClaim([], [], _).
constrainPerClaim([HC | TC], [HL | TL], NumLiars) :-
  HL #= (HC #> NumLiars),
  constrainPerClaim(TC, TL, NumLiars).
