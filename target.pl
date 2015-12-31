% target.pl
% Version 4.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ['preamble.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of our target language (a stack machine):
%
% PROG ::= [ CMSEQ ] 
%       |  [ ]
%
% CMSEQ ::= CM
%        |  CM , CMSEQ
%
% CM ::= push(V)
%     |  add
%     |  sub
%     |  mult
%     |  and
%     |  or
%     |  neg
%     |  eq
%     |  le
%     |  pop(x)
%     |  label(L)
%     |  jmpt(L)
%     |  jmpf(L)
%     |  jmp(L)
%     |  stop
%
% V ::= x | n | true | false
% 
% L ::= <alpha string>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A state in our machine is a term of arity two where the first component 
% is an integer stack used for expression evaluation and the second component 
% is a binding environment for variables:
%    '(Stack,Environment)'

%%% flow of control instructions
([],_,State) -->> State :- !.         % an empty instruction sequence is a noop

([stop|_],_,State) -->> State :- !.   % the 'stop' instruction ignores the rest
                                      % of the program

([jmp(L)|_],Cont,State) -->> OState :-
	afindlabel(L,Cont,JT),
	(JT,Cont,State) -->> OState,!.

([jmpt(_)|P],Cont,([false|Stk],Env)) -->> OState :-
        (P,Cont,(Stk,Env)) -->> OState,!.
    
([jmpt(L)|_],Cont,([true|Stk],Env)) -->> OState :-
	afindlabel(L,Cont,JT),(JT,Cont,(Stk,Env)) -->> OState,!.
    
([jmpf(L)|_],Cont,([false|Stk],Env)) -->> OState :-
	afindlabel(L,Cont,JT),(JT,Cont,(Stk,Env)) -->> OState,!.

([jmpf(_)|P],Cont,([true|Stk],Env)) -->> OState :-
        (P,Cont,(Stk,Env)) -->> OState,!.
    
%%% computational instructions
([Instr|P],Cont,State) -->> OState :-  % interpret an instruction sequence.
	(Instr,Cont,State) -->> IState,
	(P,Cont,IState) -->> OState,!.

(push(C),_,(Stk,Env)) -->> ([C|Stk],Env) :-        % constants
    int(C),!.

(push(B),_,(Stk,Env)) -->> ([B|Stk],Env) :-        % bool constants
    bool(B),!.

(push(X),_,(Stk,Env)) -->> ([ValX|Stk],Env) :-     % variables
	atom(X),
	alookup(X,Env,ValX),!.

(add,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % addition
    int(ValA),
    int(ValB),
    Val xis ValA + ValB,!.

(sub,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % subtraction
    int(ValA),
    int(ValB),
	Val xis ValA - ValB,!.

(mult,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % multiplication
    int(ValA),
    int(ValB),
	Val xis ValA * ValB,!.

(and,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % and
    bool(ValA),
    bool(ValB),
    Val xis (ValA and ValB),!.

(or,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % or
    bool(ValA),
    bool(ValB),
    Val xis (ValA or ValB),!.

(eq,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % equal
    int(ValA),
    int(ValB),
    Val xis (ValA =:= ValB),!.

(le,_,([ValB,ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % less equal
    int(ValA),
    int(ValB),
	Val xis (ValA =< ValB),!.

(neg,_,([ValA|Stk],Env)) -->> ([Val|Stk],Env) :-  % not
    bool(ValA),
    Val xis (not ValA),!.

(pop(X),_,([ValA|Stk],Env)) -->> (Stk,OEnv) :-  % store
    int(ValA),
	aput(X,ValA,Env,OEnv),!.

(label(_),_,State) -->> State :- !.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'afindlabel(+Label,+Continuation,-JumpTarget)' 
% looks up a label definition in the continutation and returns its associate code.
:- dynamic afindlabel/3.

afindlabel(L,[label(L)|P],[label(L)|P]).

afindlabel(L,[_|P],JT) :-
	afindlabel(L,P,JT).
    
afindlabel(_,[],_) :- 
	writeln('ERROR: label not found.'),!,fail.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'alookup(+Variable,+Env,-Value)' looks up
% the variable in the binding environment and returns its bound value.
:- dynamic alookup/3.                % modifiable predicate

alookup(_,s0,0).

alookup(X,env([],S),Val) :-
	alookup(X,S,Val).

alookup(X,env([bind(Val,X)|_],_),Val).

alookup(X,env([_|Rest],S),Val) :-
	alookup(X,env(Rest,S),Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'aput(+Variable,+Value,+State,-FinalState)' adds
% a binding term to the state.
:- dynamic aput/4.                   % modifiable predicate

aput(X,Val,env(L,S),env([bind(Val,X)|L],S)).

aput(X,Val,S,env([bind(Val,X)],S)) :- 
	atom(S).

