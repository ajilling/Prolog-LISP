# Prolog-LISP
A simple implementation of LISP in Prolog.
<br><br>
Syntax is limited to the following:
<br><br>
``Unit ::= [ SFormList ] | []``<br>

``SFormList ::= SForm | SForm , SFormList``<br>

``SForm ::= [ defvar , x , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ setq , x , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ defun , f , [ FL ] , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ defun , f , [ ] , SExp ]``<br>

``SExp ::= n``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| x``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| true``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| false``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ add , SExp , SExp , ... ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ sub , SExp , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ mult , SExp , SExp , ... ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ eq , SExp , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ le , SExp , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ neg , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ and , SExp , SExp , ... ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ or , SExp , SExp , ... ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ if , SExp , SExp , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ let , x , SExp , SExp ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ f ]``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| [ f , SExp , ... ]``<br>

``FL ::= x , FL``<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;``| x``<br>
