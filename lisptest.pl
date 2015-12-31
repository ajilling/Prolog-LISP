:- ([[defun,inc,[i],[add,i,1]],[defvar,x,0],[setq,x,[inc,x]]],s) -->> V, writeln(V).

:- ([[defun,fact,[i],[if,[eq,i,1],1,[mult,i,[fact,[sub,i,1]]]]],[defvar,x,[fact,3]]],s) -->> V, writeln(V).