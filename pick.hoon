/+  *hpeg
=>
|%
+$  leaves
  $@  %sp
  $%  [%non name=@tas]
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
+$  pine  (trek @t leaves)
++  pict  |$  tos  (pact pine * tos)
++  parm  |$  tos  [@t tos leaves ,~ ,~]
--
=/  pick-grammar=gram
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
=+  :*  !.
  x=(expect pine)
  ^=  xnon
  |=  t=pine
  ^-  @tas
  ?:  ?=([%u %non *] t)  name.t
  ~|  [%xnon t]  !!
  ::
  ^=  xplan
  |=  t=pine
  ^-  plan
  ?:  ?=([%u %plan *] t)  p.t
  ~|  [%xplan t]  !!
  ::
  ^=  xcount
  |=  t=pine
  ^-  @
  ?:  ?=([%u %num *] t)  n.t
  ~|  [%xcount t]  !!
  ::
  ^=  xgram
  |=  t=pine
  ^-  gram
  ?:  ?=([%u %gram *] t)  g.t
  ~|  [%xgram t]  !!
  ==
=/  acts
  %-  ~(gas by *(map @tas $-([pine *] [pine ~])))
  :~  :-  %def
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %def
    =/  top  (seq.x t)
    :-  (xnon p.top)
    (xplan (drop.x 2 q.top))
    ::::
      :-  %grammar
    |=  [t=pine *]
    :_  ~  ^-  pine
    =/  l  (lus.x t)
    =/  xdef
      |=  t=pine
      ?:  ?=([%u %def d=*] t)
        d.t
      ~|  [%def t]  !!
    =/  first  (xdef i.l)
    =/  rest   (turn t.l xdef)
    [%u %gram (meg first rest)]
      :-  %pat
    |=  [t=pine *]
    :_  ~  ^-  pine
    (drop.x 2 t)
    ::::
      :-  %pattern
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %plan
    =/  top  (seq.x t)
    =/  mor=(lest pine)
      :-  =/  tar  (tar.x p.top)
          ?>  ?=([* ~] tar)
          i.tar
      (tar.x q.top)
    |-  ^-  plan
    =/  p=plan  (xplan i.mor)
    ?~  t.mor  p
    [%or p $(mor t.mor)]
    ::::
      :-  %alt1
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %plan
    =/  top  (seq.x t)
    =/  suf  (xplan (drop.x 1 q.top))
    ?+  (wut.x p.top)  !!
      ~          suf
      [%t %'!']  [%not suf]
      [%t %'&']  [%and suf]
    ==
    ::::
      :-  %alternative
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %plan
    =/  kids  (lus.x t)
    |-  ^-  plan
    =/  p  (xplan i.kids)
    ?~  t.kids  p
    [p $(kids t.kids)]
      :-  %count
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %num
    =/  dig  (flop (tar.x t))
    =|  [pac=_1 n=@]
    |-  ^-  @
    ?~  dig  n
    %=  $
      dig  t.dig
      n    (add n (mul pac (sub (token.x i.dig) '0')))
    ==
    ::::
      :-  %quantifier
    |=  [t=pine *]
    :_  ~  ^-  pine
    ~|  [%quantifier t]
    =/  qua  (seq.x (nth.x 1 t))
    :-  %u
    ?:  ?=([%t %','] p.qua)
      [%max (xcount q.qua)]
    =/  min  (xcount p.qua)
    =/  two  (wut.x q.qua)
    ?~  two  [%rep min]
    =/  max  (wut.x (drop.x 1 two))
    ?~  max  [%min min]
    [%mid min (xcount max)]
    ::::
      :-  %suffix
    |=  [t=pine *]
    :_  ~  ^-  pine
    :+  %u  %plan  ^-  plan
    =/  top  (seq.x t)
    =/  nut  (xplan p.top)
    =/  suf  (wut.x (head.x q.top))
    ?~  suf  nut
    ?.  ?=(%u -.suf)
      ?+  (token.x suf)  !!
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
    |=  [t=pine *]
    :_  ~  ^-  pine
    (head.x t)
    ::::
      :-  %grouped
    |=  [t=pine *]
    :_  ~  ^-  pine
    (nth.x 2 t)
    ::::
      :-  %tagged
    |=  [t=pine *]
    :_  ~  ^-  pine
    =>  .(t (seq.x (drop.x 1 t)))
    =/  nym  (xnon p.t)
    =>  .(t (seq.x q.t))
    =/  zap  (wut.x p.t)
    =/  pin  (xplan (drop.x 1 q.t))
    =/  nap  [nym pin]
    :+  %u  %plan
    ?~(zap tag+nap yel+nap)
    ::::
      :-  %memoized
    |=  [t=pine *]
    :_  ~  ^-  pine
    u+plan+mem+(xplan (drop.x 2 t))
    ::::
      :-  %primary
    |=  [t=pine *]
    :_  ~  ^-  pine
    ?+  t  t
      [%u %non *]  u+plan+r+name.t
      [%t %'.']   u+plan+any+|
    ==
    ::::
      :-  %literal
    |=  [t=pine *]
    :_  ~  ^-  pine
    =/  sin  (tar.x (nth.x 1 t))
    :+  %u  %plan
    ?~  sin  any+&
    |-  ^-  plan
    =/  tok=plan  t+(token.x i.sin)
    ?~  t.sin  tok
    [tok $(sin t.sin)]
      :-  %char
    |=  [t=pine *]
    :_  ~  ^-  pine
    ?:  ?=(%t -.t)  t
    :-  %t
    ?+  (token.x (drop.x 1 t))  !!
      %'\''  '\''
      %'\\'  '\\'
      %t     '\09'
      %n     '\0a'
    ==
    ::::
      :-  %range
    |=  [t=pine *]
    :_  ~  ^-  pine
    =/  top   (seq.x t)
    =/  lo=@  (token.x p.top)
    =/  hi=@  (token.x (drop.x 1 q.top))
    [%u %run lo hi]
    ::::
      :-  %charclass
    |=  [t=pine *]
    :_  ~  ^-  pine
    =/  kids  (tar.x (nth.x 1 t))
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
    |=  [t=pine *]
    :_  ~  ^-  pine
    =/  top  (seq.x t)
    :+  %u  %non
    ^-  @tas
    %+  rep  3
    :-  (token.x p.top)
    (turn (tar.x q.top) token.x)
    ::::
      :-  %sp
    |=  [t=pine *]
    :_  ~  ^-  pine
    u+%sp
  ==
=/  ascii-grammar
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
|%
++  parse-ascii-cord
  =/  lib  ~(. lib (parm @ud))
  =/  exe  (bake.lib pick-grammar acts)
  |=  txt=@t
  (xgram (exec.lib [tuff (ascii txt)] no-memo exe 0 ~ ~))
--
