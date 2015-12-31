% lisp.pl
%
% SForm ::= [defvar,x,SExp]
%        |  [setq,x,SExp]
%        |  [defun,f,[FL],SExp]
%        |  [defun,f,[],SExp]
%
% SExp ::= n
%       |  x
%       |  true
%       |  false
%       |  [add,SExp,SExp,...]
%       |  [sub,SExp,SExp]
%       |  [mult,SExp,SExp,...]
%       |  [eq,SExp,SExp]
%       |  [le,SExp,SExp]
%       |  [neg,SExp]
%       |  [and,SExp,SExp,...]
%       |  [or,SExp,SExp,...]
%       |  [if,SExp,SExp,SExp]
%       |  [let,x,SExp,SExp]
%       |  [f]
%       |  [f,SExp,...]
%

:- ['preamble.pl'].
:- op(1200,yfx,seq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SForm

([defvar,X,SExp],State) -->> OState :-          % define var
    (SExp,State) -->> ValX,
    declarevar(X,ValX,State,OState),!.

([setq,X,SExp],State) -->> OState :-            % set var
    lookup(X,State,_),
    (X,State) -->> (ValX,SExp),
    bindval(X,ValX,SExp,OState),!.

([defun,F,[FL],SExp],State) -->> OState :-      % define function (w/ parameters)
    declarevar(F,[FL,SExp],State,OState),!.

([defun,F,[],SExp],State) -->> OState :-        % define function (empty list)
    declarevar(F,[SExp],State,OState),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SExp

([B],State) -->> (Val,State) :-                 % constants (bool)
    bool(B),
    Val xis B,!.

([N],State) -->> (Val,State) :-                 % constants (int)
    int(N),
    Val xis N,!.

([add,T1,T2],State) -->> (Val,State) :-         % addition (2 terms)
    int(T1),
    int(T2),
    Val xis T1 + T2,!.

([add,T1,T2|Rest],State) -->> (Val,State) :-    % addition (>2 terms)
    int(T1),
    Val xis T1 + [add,T2,Rest],!.

([sub,T1,T2],State) -->> (Val,State) :-         % subtract
    int(T1),
    int(T2),
    Val xis T1 - T2,!.

([mult,T1,T2],State) -->> (Val,State) :-        % multiplication (2 terms)
    int(T1),
    int(T2),
    Val xis T1 * T2,!.

([mult,T1,T2|Rest],State) -->> (Val,State) :-   % multiplication (>2 terms)
    int(T1),
    Val xis T1 * [mult,T2,Rest],!.

([and,T1,T2],State) -->> (Val,State) :-         % and (2 terms)
    bool(T1),
    bool(T2),
    Val xis (T1 and T2),!.

([and,T1,T2|Rest],State) -->> (Val,State) :-    % and (>2 terms)
    bool(T1),
    Val xis (T1 and [and,T2,Rest]),!.

([or,T1,T2],State) -->> (Val,State) :-          % or (2 terms)
    bool(T1),
    bool(T2),
    Val xis (T1 or T2),!.

([or,T1,T2|Rest],State) -->> (Val,State) :-     % or (>2 terms)
    bool(T1),
    Val xis (T1 or [or,T2,Rest]),!.

([neg,T1],State) -->> (Val,State) :-            % neg
    bool(T1),
    Val xis (not T1),!.

([eq,T1,T2],State) -->> (Val,State) :-          % eq
    int(T1),
    int(T2),
    Val xis (T1 =:= T2),!.

([le,T1,T2],State) -->> (Val,State) :-          % le
    int(T1),
    int(T2),
    Val xis (T1 =< T2),!.

([if,B,C0,_],State) -->> OState :-              % if true
    (B,State) -->> true,
    (C0,State) -->> OState,!.

([if,B,_,C1],State) -->> OState :-              % if false
    (B,State) -->> false,
    (C1,State) -->> OState,!.

([let,X,SE1,SE2],State) -->> OState :-          % let
    (X,SE1) -->> ValX,
    put(ValX,SE2,State,OState),!.

([F],State) -->> OState :-                      % function call (empty list)
    (F,State) -->> OState,!.

([F,P1|PRest],State) -->> OState :-             % function call (w/ parameters)
    ([F,PRest],OState),
    (P1,State) -->> OState,!.

(X,State) -->> (X,State) :-                     % variables
    atom(X),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'lookup(+Variable,+State,-Value)' looks up
% the variable in the state and returns its bound value.
:- dynamic lookup/3.                % modifiable predicate

lookup(_,s0,0).

lookup(X,state([],S),Val) :-
    lookup(X,S,Val).

lookup(X,state([bind(Val,X)|_],_),Val).

lookup(X,state([_|Rest],S),Val) :-
    lookup(X,state(Rest,S),Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'put(+Variable,+Value,+State,-FinalState)' adds
% a binding term to the state.
:- dynamic put/4.                   % modifiable predicate

put(X,Val,state(L,S),state([bind(Val,X)|L],S)).

put(X,Val,S,state([bind(Val,X)],S)) :-
    atom(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'declarevar(+Variable,+Val,+State,-FinalState)' declares
% a variable by inserting a new binding term into the current
% environment.
:- dynamic declarevar/4.                   % modifiable predicate

declarevar(X,V,S,env([bind(V,X)],S)) :-
    atom(S),!.

declarevar(X,V,env(L,S),env([bind(V,X)|L],S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'declareparams(+ActualList,+State,-FinalState)' declares
% a non-empty list of formal variables by inserting new binding terms into
% the current environment.
:- dynamic declareparams/3.                   % modifiable predicate

% Note: positional correspondence can easily be implemented by
% co-recusion onf the formal parameter list

declareparams([assign(X,A)],State,OState) :-
    (A,State) -->> (ValA,S1),
    declarevar(X,ValA,S1,OState).

declareparams([assign(X,A)|TA],State,OState) :-
    (A,State) -->> (ValA,S1),
    declarevar(X,ValA,S1,S2),
    declareparams(TA,S2,OState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'pushenv(+State,-FinalState)' pushes
% a new binding term list on the stack
:- dynamic pushenv/2.

pushenv(S,env([],S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'popenv(+State,-FinalState)' pops
% a  binding term list off the stack
:- dynamic popenv/2.

popenv(env(_,S),S).