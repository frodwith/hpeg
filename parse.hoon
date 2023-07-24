/+  *hpeg
/+  pick
::  at some point a visualizer tool would be cool,
::  but we probably do that in javascript. or with mark's
::  new ncurses stuff?  following guido's visualiser?
::
::  next step: rearrange the code.
::  separate out into sur and lib files,
::  have a library of useful helper functions, a library
::  for parsing the dsl, a generator that runs this test.
::
::  a json parser
::
::  maybe a little concatenative language based on nock 7 ;)
!:
:-  %say
|=  *
:-  %noun
=/  hol  [@tD tape ,~ ,~ ,~]
=/  heh  ~(. sur hol)
=/  hib  ~(. lib hol)
=/  tapes=toke.heh
  :-  `puff.heh`|=(@tD `@`+<)
  `pass.heh`|=(tape +<)
=/  gel=gram
  :-  %$  :_  [~ ~]  :-  %$
  [t+%h t+%e t+%l t+%l t+%o t+',']
=/  hello-compiled=exe.heh  (bake.hib gel ~)
=/  hello-parser
  |=  input=tape
  (exec.hib tapes no-memo hello-compiled input ~ ~)
?>  .=  [t+%h t+%e t+%l t+%l t+%o t+',']
    (hello-parser "hello, anything!")
?>  =(| (hello-parser "helo, nothing!"))
?>  .=  pick-grammar.pick
    (parse-ascii-cord.pick ascii-grammar.pick)
%ok
