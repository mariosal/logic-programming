% Creates a list [X, X, X, ...] of length N
repeat_el(_, 0, []).
repeat_el(X, N, [X | L]) :-
    N > 0,
    N1 is N - 1,
    repeat_el(X, N1, L).

% Checking if argument is a tuple and the second elm is int
check_tuple((_, N)) :-
    integer(N).

decode_rl([], []).
decode_rl([(X, Y) | T], L) :-
    check_tuple((X, Y)),
    repeat_el(X, Y, L1),
    decode_rl(T, L2),
    append(L1, L2, L).
decode_rl([H | T], [H | L]) :-
    \+ check_tuple(H),
    decode_rl(T, L).

% Counts how many same successive elements are at the head of the list
count([], 0).
count([_], 1).
count([H1, H2 | _], 1) :-
    H1 \== H2.
count([H, H | T], N) :-
    count([H | T], N1),
    N is N1 + 1.

% Removes the first N elements from the list
move(L, 0, L).
move([_ | T], N, L) :-
    N > 0,
    N1 is N - 1,
    move(T, N1, L).

% If N == 1 returns H, else (H, N)
tuplify(H, 1, H).
tuplify(H, N, (H, N)) :-
    N > 1.

encode_rl([], []).
encode_rl([H | T], [H1 | L]) :-
    count([H | T], N),
    move([H | T], N, ML),
    tuplify(H, N, H1),
    encode_rl(ML, L).

/*
?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L).
L = [p(3), p(X), q(X), q(Y), q(4)] ;
Y = 4,
L = [p(3), p(X), q(X), (q(4), 2)] ;
X = Y,
L = [p(3), p(Y), (q(Y), 2), q(4)] ;
X = Y, Y = 4,
L = [p(3), p(4), (q(4), 3)] ;
X = 3,
L = [ (p(3), 2), q(3), q(Y), q(4)] ;
X = 3,
Y = 4,
L = [ (p(3), 2), q(3), (q(4), 2)] ;
X = Y, Y = 3,
L = [ (p(3), 2), (q(3), 2), q(4)] ;
false.

Ο O(n) αλγόριθμός μου για κάθε στοιχείο της λίστας μετράει πόσες φορές
εμφανίζεται διαδοχικά, έστω Κ, έπειτα δημιουργεί το πιθανό tuple (στοιχείο, Κ),
προσπερνάει με την move Κ στοιχεία και συνεχίζει την αναδρομή.

Οι πολλαπλές λύσεις δημιουργούνται λόγω του ορισμού του κατηγορήματος count,
όπου στο `count([H, H | T], N)` ο interpreter προσπαθεί να κάνει unification των
δυο διαδοχικών τιμών της λίστας λόγω του pattern matching.
*/
