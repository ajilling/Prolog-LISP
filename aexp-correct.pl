:- ['translate.pl'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- >>> 'case c (constants)'.

:- asserta((n,e) -->> vn).
:- asserta(int(vn)).
:- asserta(translate(n,cn)).
:- asserta((cn,_,(S,E)) -->> ([vn|S],E)).

:- (n,e) -->> V1,
    translate(n,C),
    (C,C,([],e)) -->> ([V2],e),
    V1 = V2.

:- retract((n,e) -->> vn).
:- retract(int(vn)).
:- retract(translate(n,cn)).
:- retract((cn,_,(S,E)) -->> ([vn|S],E)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- >>> 'case x (variables)'.

:- asserta((x,e) -->> vx).
:- asserta(translate(x,cx)).
:- asserta((cx,_,(S,E)) -->> ([vx|S],E)).

:- (x,e) -->> V1,
    translate(x,C),
    (C,C,([],e)) -->> ([V2],e),
    V1 = V2.

:- retract((x,e) -->> vx).
:- retract(translate(x,cx)).
:- retract((cx,_,(S,E)) -->> ([vx|S],E)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the following holds for add, sub, and mult

:- asserta((a,e) -->> va).
:- asserta((b,e) -->> vb).
:- asserta(int(va)).
:- asserta(int(vb)).
:- asserta(translate(a,ca)).
:- asserta(translate(b,cb)).
:- asserta((ca,_,(S,E)) -->> ([va|S],E)).
:- asserta((cb,_,(S,E)) -->> ([vb|S],E)).

:- >>> 'case add(a,b) (addition)'.
:- (add(a,b),e) -->> V1,
    translate(add(a,b),C),
    (C,C,([],e))  -->> ([V2],e),
    V1 = V2.

:- >>> 'case sub(a,b) (subtraction)'.
:- (sub(a,b),e) -->> V1,
    translate(sub(a,b),C),
    (C,C,([],e))  -->> ([V2],e),
    V1 = V2.

:- >>> 'case mult(a,b) (multiplication)'.
:- (mult(a,b),e) -->> V1,
    translate(mult(a,b),C),
    (C,C,([],e))  -->> ([V2],e),
    V1 = V2.

:- retract((a,e) -->> va).
:- retract((b,e) -->> vb).
:- retract(int(va)).
:- retract(int(vb)).
:- retract(translate(a,ca)).
:- retract(translate(b,cb)).
:- retract((ca,_,(S,E)) -->> ([va|S],E)).
:- retract((cb,_,(S,E)) -->> ([vb|S],E)).