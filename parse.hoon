/-  *hpeg
/+  *hpeg
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
=/  no-memo  =/(z |=(* ~) z^z)
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
=/  peg-grammar=gram
  =/  arrow=plan  [t+'<' t+'-']
  =/  soq=plan    t+'\''
  =/  dot=plan    any+|
  =/  sp=plan     r+%sp
  %-  meg
  :~  :-  %grammar
    lus+tag+def+[r+%nonterminal arrow sp r+%pattern]
    ::::
      :-  %pattern
    :-  [%rep 1 r+%alternative]
    tar+tag+pat+[t+'/' sp r+%alternative]
    ::::
      :-  %alternative
    lus+tag+alt1+[max+1+(chas '!' '&' ~) sp r+%suffix]
    ::::
      :-  %count
    [%min 1 %run '0' '9']
    ::::
      :-  %suffix
    :-  r+%primary  :_  sp  :-  %wut
    :+  %or  (chas '*' '+' '?' ~)
    :+  %tag  %quantifier
    :-  t+'{'  :_  t+'}'
    :+  %or  [t+',' r+%count]
    [r+%count %mid 0 1 t+',' wut+r+%count]
    ::::
      :-  %primary
    :-  %or  :_  [r+%nonterminal not+arrow]
    :+  %tag  %head
    :_  sp
    :+  %or  tag+grouped+[t+'(' sp r+%pattern t+')']
    :+  %or  t+'.'
    :+  %or  tag+literal+[soq [%tar not+soq r+%char] soq]
    :+  %or  tag+tagged+[t+':' r+%identifier wut+t+'!' sp r+%pattern]
    :+  %or  tag+memoized+[t+'#' sp r+%pattern]
    :*  %tag  %charclass
        t+'['
        :*  %tar
            not+t+']'
            [%or tag+range+[dot t+'-' dot] r+%char]
        ==
        t+']'
    ==
    ::::
      :-  %char
    :+  %or
      [t+'\\' (chas 't' 'n' '\\' '\'' ~)]
    dot
    ::::
      :-  %identifier
    [run+[%a %z] %tar %or run+[%a %z] %or run+[%'0' %'9'] t+'-']
    ::::
      :-  %nonterminal
    tag+head+[r+%identifier sp]
    ::::
      :-  %sp
    tar+(chas ' ' 9 10 ~)
  ==
=/  usr
  $@  %sp
  $%  [%nt name=@tas]
      [%plan p=plan]
      [%gram g=gram]
      [%num n=@]
      [%run from=@ to=@]
      [%def name=@tas p=plan]
      [%rep n=@]
      [%min n=@]
      [%max n=@]
      [%mid from=@ to=@]
  ==
=/  pom  [@ta ,[@ud @ud @ta] usr ,~ ,~]
=/  pep  ~(. sur pom)
=/  pib  ~(. lib pom)
=/  ascii-cords=toke.pep
  :-  |=(@ta `@`+<)
  =/  c  %*(. cut a 3, c 1)
  |=  [i=@ud len=@ud txt=@ta]
  ?:  (gte i len)  ~
  :_  [+(i) +<+]
  (c(b i, d txt))
=/  expect-seq
  |=  t=tree.pep
  ?^  -.t  t
  ~|  [%seq t]  !!
=/  expect-tar
  |=  t=tree.pep
  ^-  [n=@ ts=(list tree.pep)]
  ?:  ?=(%r -.t)  +.t
  ~|  [%tar t]  !!
=/  expect-lus
  |=  t=tree.pep
  ^-  [n=@ ts=(lest tree.pep)]
  ?:  ?=([%r @ ^ *] t)
    [n.t i.l.t t.l.t]
  ~|  [%lus t]  !!
=/  expect-wut
  |=  t=tree.pep
  ^-  $@(~ tree.pep)
  =/  tar  (expect-tar t)
  ?~  ts.tar  ~
  ?~  t.ts.tar  i.ts.tar
  ~|  [%wut t]  !!
=/  expect-token
  ::  this should work for any tree, so it feels like it should
  ::  be a wet gate and outside of pep. or inside pep, moist.
  ::  or both.
  |=  t=tree.pep
  ?:  ?=(%t -.t)  tok.t
  ~|  [%token t]  !!
=/  seq-drop
  |=  [n=@ t=tree.pep]
  ?:  =(0 n)  t
  $(n (dec n), t q:(expect-seq t))
=/  seq-head
  |=  t=tree.pep
  p:(expect-seq t)
=/  seq-nth
  |=  [n=@ t=tree.pep]
  ~|  [%seq-nth n t]
  (seq-head (seq-drop n t))
=/  expect-plan
  |=  t=tree.pep  !.
  ^-  plan
  ?:  ?=([%u %plan *] t)  p.t
  ~|  [%plan t]  !!
=/  expect-nonterminal
  |=  t=tree.pep
  ^-  @tas
  ?:  ?=([%u %nt *] t)  name.t
  ~|  [%nonterminal t]  !!
=/  expect-count
  |=  t=tree.pep
  ^-  @
  ?:  ?=([%u %num *] t)  n.t
  ~|  [%count t]  !!
=/  plan-actions=mean.pep
  %-  sam.pib
  :~  :-  %def
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %def
    =/  top  (expect-seq t)
    :-  (expect-nonterminal p.top)
    (expect-plan (seq-drop 2 q.top))
    ::::
      :-  %grammar
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =/  l  ts:(expect-lus t)
    =/  expect-def
      |=  t=tree.pep
      ?:  ?=([%u %def d=*] t)
        d.t
      ~|  [%def t]  !!
    =/  first  (expect-def i.l)
    =/  rest   (turn t.l expect-def)
    [%u %gram (meg first rest)]
      :-  %pat
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    (seq-drop 2 t)
    ::::
      :-  %pattern
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %plan
    =/  top  (expect-seq t)
    =/  mor=(lest tree.pep)
      :-  =/  tar  (expect-tar p.top)
          ?>  ?=([* ~] ts.tar)
          i.ts.tar
      ts:(expect-tar q.top)
    |-  ^-  plan
    =/  p=plan  (expect-plan i.mor)
    ?~  t.mor  p
    [%or p $(mor t.mor)]
    ::::
      :-  %alt1
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %plan
    =/  top  (expect-seq t)
    =/  suf  (expect-plan (seq-drop 1 q.top))
    ?+  (expect-wut p.top)  !!
      ~          suf
      [%t %'!']  [%not suf]
      [%t %'&']  [%and suf]
    ==
    ::::
      :-  %alternative
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %plan
    =/  kids  ts:(expect-lus t)
    |-  ^-  plan
    =/  p  (expect-plan i.kids)
    ?~  t.kids  p
    [p $(kids t.kids)]
      :-  %count
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %num
    =/  dig  (flop ts:(expect-tar t))
    =|  [pac=_1 n=@]
    |-  ^-  @
    ?~  dig  n
    %=  $
      dig  t.dig
      n    (add n (mul pac (sub (expect-token i.dig) '0')))
    ==
    ::::
      :-  %quantifier
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    ~|  [%quantifier t]
    =/  qua  (expect-seq (seq-nth 1 t))
    :-  %u
    ?:  ?=([%t %','] p.qua)
      [%max (expect-count q.qua)]
    =/  min  (expect-count p.qua)
    =/  two  (expect-wut q.qua)
    ?~  two  [%rep min]
    =/  max  (expect-wut (seq-drop 1 two))
    ?~  max  [%min min]
    [%mid min (expect-count max)]
    ::::
      :-  %suffix
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    :+  %u  %plan  ^-  plan
    =/  top  (expect-seq t)
    =/  nut  (expect-plan p.top)
    =/  suf  (expect-wut (seq-head q.top))
    ?~  suf  nut
    ?.  ?=(%u -.suf)
      ?+  (expect-token suf)  !!
        %'*'  [%tar nut]
        %'?'  [%wut nut]
        %'+'  [%lus nut]
      ==
    ?+  +<.suf  !!
      %min  [%min n.suf nut]
      %max  [%max n.suf nut]
      %mid  [%mid from.suf to.suf nut]
      %rep  [%rep n.suf nut]
    ==
    ::::
      :-  %head
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    (seq-head t)
    ::::
      :-  %grouped
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    (seq-nth 2 t)
    ::::
      :-  %tagged
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =>  .(t (expect-seq (seq-drop 1 t)))
    =/  nym  (expect-nonterminal p.t)
    =>  .(t (expect-seq q.t))
    =/  zap  (expect-wut p.t)
    =/  pin  (expect-plan (seq-drop 1 q.t))
    =/  nap  [nym pin]
    :+  %u  %plan
    ?~(zap tag+nap yel+nap)
    ::::
      :-  %memoized
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    u+plan+mem+(expect-plan (seq-drop 2 t))
    ::::
      :-  %primary
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    ?+  t  t
      [%u %nt *]  u+plan+r+name.t
      [%t %'.']   u+plan+any+|
    ==
    ::::
      :-  %literal
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =/  sin  ts:(expect-tar (seq-nth 1 t))
    :+  %u  %plan
    ?~  sin  any+&
    |-  ^-  plan
    =/  tok=plan  t+(expect-token i.sin)
    ?~  t.sin  tok
    [tok $(sin t.sin)]
      :-  %char
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    ?:  ?=(%t -.t)  t
    :-  %t
    ?+  (expect-token (seq-drop 1 t))  !!
      %'\''  '\''
      %'\\'  '\\'
      %t     '\09'
      %n     '\0a'
    ==
    ::::
      :-  %range
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =/  top   (expect-seq t)
    =/  lo=@  (expect-token p.top)
    =/  hi=@  (expect-token (seq-drop 1 q.top))
    [%u %run lo hi]
    ::::
      :-  %charclass
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =/  kids  ts:(expect-tar (seq-nth 1 t))
    :+  %u  %plan
    ?~  kids  any+|
    =|  [s=(set @) out=(list (each (set @) (pair @ @)))]
    |^  ^-  plan
        =*  k  i.kids
        ?:  ?=(%t -.k)
          next(s (~(put in s) tok.k))
        %=  next
          s    ~
          out  ?>  ?=([%u %run *] k)
               =/  r  |+[from.k to.k]
               ?~  s  [r out]  ::  eliminate empty sets
               [r &+s out]
        ==
    ++  el
      |=  e=(each (set @) (pair @ @))
      ^-  plan
      ?-  -.e
        %|  [%run p.e]
        %&  =*  s  p.e
            ?:  ?=([* ~ ~] s)  ::  handle singleton sets
              t+n.s
            set+s
      ==
    ++  next
      ?.  ?=(~ t.kids)  $(kids t.kids)
      =?  out  ?=(^ s)  [&+s out]
      ?~  out  !!
      =/  r=plan  (el i.out)
      =>  .(out t.out)
      |-  ^-  plan
      ?~  out  r
      $(out t.out, r [%or (el i.out) r])
    --
    ::::
      :-  %identifier
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    =/  top  (expect-seq t)
    :+  %u  %nt
    ^-  @tas
    %+  rep  3
    :-  (expect-token p.top)
    (turn ts:(expect-tar q.top) expect-token)
    ::::
      :-  %sp
    |=  [t=tree.pep *]
    :_  ~  ^-  tree.pep
    u+%sp
  ==
=/  peg-compiled=exe.pep  (bake.pib peg-grammar plan-actions)
=/  peg-parser
  |=  input=@ta
  =/  tos  [0 (met 3 input) input]
  =/  r    (exec.pib ascii-cords no-memo peg-compiled tos ~ ~)
  ?>  ?=([%u %gram *] r)
  g.r
:::-  p=peg-grammar
::^=  q
?>  .=  peg-grammar
%-  peg-parser
::  from the lpeg paper, modified:
::    restrict nonterminals to @tas
::    escapes for accurate self-parsing
::    added tag and memo syntax
::    added {1,2}-style quantifiers (and used them artificially)
'''
grammar     <- (:def nonterminal '<-' sp pattern)+
pattern     <- alternative{1} (:pat '/' sp alternative)*
alternative <- (:alt1 [!&]{,1} sp suffix)+
count       <- [0-9]{1,}
suffix      <- primary (
                 [*+?]
                 / (:quantifier '{' (
                   ',' count
                   / count (',' count?){0,1}
                 ) '}')
               )? sp
primary     <- (:head (
                  (:grouped '(' sp pattern ')')
                / '.'
                / (:literal '\'' (!'\'' char)* '\'')
                / (:tagged ':' identifier '!'? sp pattern)
                / (:memoized '#' sp pattern)
                / (:charclass '['
                   ( !']' ( (:range . '-' .) / char ) )*
                  ']')
                ) sp)
               / nonterminal !'<-'
char        <- '\\' [tn'\\] / .
identifier  <- [a-z] [a-z0-9-]*
nonterminal <- :head identifier sp
sp          <- [ \t\n]*
'''
%ok
