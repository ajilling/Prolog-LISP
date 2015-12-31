% lisp.pl
:- ['preamble.pl'].

% syntax of the lisp language:
%
%  C ::= skip
%     |  assign(x,E)
%     |  seq(C,C)
%     |  if(E,C,C)       -- [] == false
%     |  whiledo(E,C)    -- [] == false
%
%  E ::= x                -- any legal variable name
%     |  []
%     |  [ Elems ]
%     |  add(E,E)         -- append the two lists
%     |  mult(E,E)        -- return as many copies of the second list as there are elements in the first list
%     |  first(E)         -- return the first element of list E as a list
%     |  rest(E)          -- return the tail of list E
%
%  Elems ::= Elem
%         | Elem , Elems
%
%  Elem ::= a | b | ... | x | y | z | 0 | 1 | ... | 8 | 9
%

:- op(1200,yfx,seq).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of commands

%(skip,State) -->> State :- !.          % skip

%(assign(X,E),State) -->> OState :-     % assignment
%    atom(X),
%    (E,State) -->> ValE,
%    put(X,ValE,State,OState),!.

%(seq(C0,C1),State) -->> OState :-      % composition, seq
%    (C0,State) -->> S0,
%    (C1,S0) -->> OState,!.

%(if(E,C0,_),State) -->> OState :-     % if
%    (E,State) -->> [_|_], % non-empty list mean 'true'
%    (C0,State) -->> OState,!.

%(if(E,_,C1),State) -->> OState :-     % if
%    (E,State) -->> [], % empty list means 'false'
%    (C1,State) -->> OState,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of arithmetic expressions

%(X,State) -->> Val :-              % variables
%    atom(X),
%    lookup(X,State,Val),!.

([],_) -->> [] :- !.           % list values in list are just mapped
                                   % to Prolog lists.

([H|T],_) -->> [H|T] :- !.     % Note: here we assume that the list
                                   % has the right format -- should check this.


%([add|L],State) -->> [R] :-
%    (L,State) -->> [R],!.


%(add(A,B),State) -->> Val :-       % adding lists means appending them.
%    (A,State) -->> ValA,
%    (B,State) -->> ValB,
%    append(ValA,ValB,Val),!.


(first(E),State) -->> [H] :-      % return the first element of E.
    (E,State) -->> [H|_],!.       % NOTE: this fails if E evaluates to
                                  % an empty list.


(rest(E),State) -->> T :-
    (E,State) -->> [_|T],!.

([add|Rest],State) -->> T :-
    (Rest,State) -->> T,!.


%(rest(E),State) -->> T :-         % return the rest of E - list without
%    (E,State) -->> [_|T],!.       % the first element.
                                  % NOTE: this fails if E evaluates to
                                  % an empty list.

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

