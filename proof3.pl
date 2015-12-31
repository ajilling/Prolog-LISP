% proof3.pl
:- ['preamble.pl'].

:- >>> 'prove that [add,a,b] ~ [add,b,a]'.

% show that
% (forall a,b)(exists V0,V1)
%         ([add,a,b],s)-->>V0 ^ ([add,b,a],s)-->>V1 ^ = (V0,V1))
% assuming
% (a,s) -->> va.
% (b,s) -->> vb.

% load semantics
:- ['lisp.pl'].

% assumptions on semantic values of expressions
:- asserta((a,s)-->>va).
:- asserta((b,s)-->>vb).

% proof
:- ([add,b,a],s) -->> V0, ([add,a,b],s) -->> V1, V0=V1.