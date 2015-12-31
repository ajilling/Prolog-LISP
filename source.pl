% sem.pl
% Version 2.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ['preamble.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% complete semantics of the source language:
%
%  A ::= n
%     |  x
%     |  add(A,A)
%     |  sub(A,A)
%     |  mult(A,A)
%
%  B ::= true
%     |  false
%     |  eq(A,A)
%     |  le(A,A)
%     |  not(B)
%     |  and(B,B)
%     |  or(B,B)
%
%  C ::= skip
%     |  assign(x,A)
%     |  seq(C,C)
%     |  if(B,C,C)
%     |  whiledo(B,C)
%
% for convenience sake make seq infix and left associative
:- op(1200,yfx,seq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of arithmetic expressions

(C,_) -->> C :-                    % constants
    int(C),!.

(X,Env) -->> Val :-              % variables
    atom(X),
    lookup(X,Env,Val),!.

(add(A,B),Env) -->> Val :-       % addition
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis ValA + ValB,!.

(sub(A,B),Env) -->> Val :-       % subtraction
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis ValA - ValB,!.

(mult(A,B),Env) -->> Val :-     % multiplication
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis ValA * ValB,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of boolean expressions

% Note: we introduce new terms for the Prolog conjunction and 
% disjunction.  We could have used the built-in ',' and ';'
% operators but this would make terms difficult to read.

(true,_) -->> true :- !.               % constants

(false,_) -->> false :- !.             % constants

(eq(A,B),Env) -->> Val :-            % equality
    (A,Env) -->> ValA,         
    (B,Env) -->> ValB,         
    Val xis (ValA =:= ValB),!. 
    
(le(A,B),Env) -->> Val :-            % le
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis (ValA =< ValB),!.
    
(not(A),Env) -->> Val :-             % not
    (A,Env) -->> ValA,
    Val xis (not ValA),!.

(and(A,B),Env) -->> Val :-           % and
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis (ValA and ValB),!.

(or(A,B),Env) -->> Val :-            % or
    (A,Env) -->> ValA,
    (B,Env) -->> ValB,
    Val xis (ValA or ValB),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of commands

(skip,Env) -->> Env :- !.          % skip

(assign(X,A),Env) -->> OEnv :-     % assignment
    (A,Env) -->> ValA,
    put(X,ValA,Env,OEnv),!.

(seq(C0,C1),Env) -->> OEnv :-      % composition, seq
    (C0,Env) -->> S0,
    (C1,S0) -->> OEnv,!.

(if(B,C0,_),Env) -->> OEnv :-     % if
    (B,Env) -->> true,
    (C0,Env) -->> OEnv,!.

(if(B,_,C1),Env) -->> OEnv :-     % if
    (B,Env) -->> false,
    (C1,Env) -->> OEnv,!.

(whiledo(B,_),Env) -->> OEnv :-    % while
    (B,Env) -->> false,
    Env=OEnv,!.

(whiledo(B,C),Env) -->> OEnv :-    % while
    (B,Env) -->> true,
    (C,Env) -->> SC,
    (whiledo(B,C),SC) -->> OEnv,!. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'lookup(+Variable,+Env,-Value)' looks up
% the variable in the env and returns its bound value.
:- dynamic lookup/3.                % modifiable predicate

lookup(_,s0,0).

lookup(X,env([],S),Val) :-
    lookup(X,S,Val).

lookup(X,env([bind(Val,X)|_],_),Val).

lookup(X,env([_|Rest],S),Val) :- 
    lookup(X,env(Rest,S),Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'put(+Variable,+Value,+Env,-FinalEnv)' adds
% a binding term to the env.
:- dynamic put/4.                   % modifiable predicate

put(X,Val,env(L,S),env([bind(Val,X)|L],S)).

put(X,Val,S,env([bind(Val,X)],S)) :- 
    atom(S).


