:-['preamble.pl'].
:-['sem.pl'].

:- >>> 'define the parts of our program'.
init(assign(i,1) seq assign(z,0)).
guard(not(eq(i,n))).
body(assign(i,add(i,1)) seq assign(z,add(z,2))).

:- >>> 'define our sum operation'.
:- dynamic magic/2.

% magic(+input,-output) where output is 2*input-2
magic(1,0).

magic(X,Y) :- Z is 2*X, Y is Z-2.

:- >>> 'first proof obligation'.
:- >>> 'assume precondition'.
:- asserta(lookup(n,s,vn)).
:- >>> 'proof the invariant'.
:- init(I),(I,s) -->> Q,lookup(z,Q,VZ),lookup(i,Q,VI), magic(VI,VZ).
:- retract(lookup(n,s,vn)).

:- >>> 'second proof obligation'.
:- >>> 'assume invariant on s'.
:- asserta(lookup(z,s,vz)).
:- asserta(lookup(i,s,vi)).
:- asserta(magic(vi,vz)).
% implies
:- asserta(magic(vi+1,vz+2)).
:- >>> 'assume guard on s'.
:- asserta((not(eq(i,n)),s) -->> true).
:- >>> 'proof the invariant on Q'.
%:- body(Bd),(Bd,s) -->> Q, writeln(Q).
:- body(Bd),(Bd,s) -->> Q,lookup(z,Q,VZ),lookup(i,Q,VI), magic(VI,VZ).
:- retract(lookup(z,s,vz)).
:- retract(lookup(i,s,vi)).
:- retract(magic(vi,vz)).
:- retract(magic(vi+1,vz+2)).
:- retract((not(eq(i,n)),s) -->> true).

%%:- >>> 'third proof obligation'.
%%:- >>> 'assume the invariant on s'.
%%:- asserta(lookup(p,s,vp)).
%%:- asserta(lookup(i,s,vi)).
%%:- asserta(sum(vi,vp)).
%%:- >>> 'assume NOT guard on s'.
%%:- asserta((not(eq(i,n)),s) -->> not(true)).
% implies
%%:- asserta((eq(i,n),s) -->> true).
% implies
%%:- asserta(sum(vn,vp)).
%%:- >>> 'prove postcondition on s'.
%%:- lookup(p,s,VP),sum(vn,VP).
%%:- retract(lookup(p,s,vp)).
%%:- retract(lookup(i,s,vi)).
%%:- retract(sum(vi,vp)).
%%:- retract((not(eq(i,n)),s) -->> not(true)).
%%:- retract((eq(i,n),s) -->> true).
%%:- retract(sum(vn,vp)).