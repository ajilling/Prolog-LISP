:-['preamble.pl'].
:-['sem.pl'].


double(0,0).

double(X,Y) :-
    T1 is X-1,
    double(T1,T2),
    Y is T2+2.

:- double(80,X), X = 160.