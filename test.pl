% proof2.pl
:-['preamble.pl'].
:-['sem.pl'].

program(assign(i,1) seq
        assign(z,0) seq
        whiledo(le(i,n),
            assign(i,add( i,1)) seq
            assign(z,add(z,2)))).

:- assert(lookup(n,s,4)).


:- program(P),
    (P,s) -->> Q,
    writeln(Q).


:- retract(lookup(n,s,4)).