% proof2.pl
:-['preamble.pl'].
:-['sem.pl'].

:- >>> 'define the parts of our program'.
init(assign(i,1) seq assign(z,0)).
guard(le(i,n)).
body(assign(i,add(i,1)) seq assign(z,add(z,2))).

:- >>> 'define our double operation'.
:- dynamic double/2.

double(X,Y) :-
    T1 is X - 1,
    Y is T1 * 2.

:- >>> 'first proof obligation'.
:- >>> 'assume precondition'.
:- asserta(lookup(n,q,vn)).
:- >>> 'prove the invariant'.
:- init(I),(I,q) -->> Q,lookup(z,Q,VZ),lookup(i,Q,VI), double(VI,VZ).
:- retract(lookup(n,q,vn)).

:- >>> 'second proof obligation'.
:- >>> 'assume invariant on q'.
:- asserta(lookup(z,q,vz)).
:- asserta(lookup(i,q,vi)).
:- asserta(double(vi,vz)).
% implies
:- asserta(double(vi+1,vz+2)).
:- >>> 'assume guard on s'.
:- asserta((le(i,n),q) -->> true).
:- >>> 'prove the invariant on Q'.

:- body(Bd),(Bd,q) -->> Q,lookup(z,Q,VZ),lookup(i,Q,VI), double(VI,VZ).

:- retract(lookup(z,q,vz)).
:- retract(lookup(i,q,vi)).
:- retract(double(vi,vz)).
:- retract(double(vi+1,vz+2)).
:- retract((le(i,n),q) -->> true).

:- >>> 'third proof obligation'.
:- >>> 'assume the invariant on q'.
:- asserta(lookup(z,q,vz)).
:- asserta(lookup(i,q,vi)).
:- asserta(double(vi,vz)).

:- >>> 'assume NOT guard on q'.
:- asserta((not(le(i,n)),q) -->> false).
:- asserta(double(vn,vz)).
:- >>> 'prove postcondition on q'.
:- lookup(z,q,VZ),double(vn,VZ).
:- retract(lookup(z,q,vz)).
:- retract(lookup(i,q,vi)).
:- retract(double(vi,vz)).

:- retract((not(le(i,n)),q) -->> false).
:- retract(double(vn,vz)).














