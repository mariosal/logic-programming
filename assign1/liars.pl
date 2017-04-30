% Returns the answer list as if N is the correct number of liars
greater([], _, []).
greater([H | T], N, [0 | M]) :-
    H =< N,
    greater(T, N, M).
greater([H | T], N, [1 | M]) :-
    H > N,
    greater(T, N, M).

% Looping N to 0 and checking if the sum of the "greater" list is equal to N
check(L, N, K) :-
    N >= 0,
    greater(L, N, K),
    sumlist(K, N).
check(L, N, P) :-
    N > 0,
    greater(L, N, K),
    sumlist(K, M),
    N =\= M,
    N1 is N - 1,
    check(L, N1, P).

liars(L, K) :-
    length(L, N),
    check(L, N, K).
