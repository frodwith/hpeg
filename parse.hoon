::  at some point a visualizer tool would be cool,
::  but we probably do that in javascript. or with mark's
::  new ncurses stuff?  following guido's visualiser?
!:
:-  %say
|=  *
:-  %noun
=<  =/  heh  (hpeg @tD tape ,~ ,~ ,~)
    =/  tapes=toke.heh
      :-  |=(@tD `@`+<)
      |=(tape +<)
    =/  no-memo  =/(z |=(* ~) z^z)
    =/  gel=gram
      :-  %$  :_  [~ ~]  :-  %$
      [t+%h t+%e t+%l t+%l t+%o t+',']
    =/  hello-compiled=exe.heh  (bake.heh gel ~)
    =/  hello-parser
      |=  input=tape
      (heh tapes no-memo hello-compiled input ~ ~)
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
        lus+[r+%nonterminal arrow sp r+%pattern]
          :-  %pattern
        :-  r+%alternative
        tar+[t+'/' sp r+%alternative]
          :-  %alternative
        lus+[wut+(chas '!' '&' ~) sp r+%suffix]
          :-  %suffix
        :-  r+%primary
        tar+[(chas '*' '+' '?' ~) sp]
          :-  %primary
        :+  %or  [t+'(' sp r+%pattern t+')' sp]
        :+  %or  :^  %tag  %tag  [t+'{' t+':']
          [r+%nonterminal r+%pattern t+'}' sp]
        :+  %or  tag+mem+[t+'{' sp r+%pattern t+'}' sp]
        :+  %or  [t+'.' sp]
        :+  %or  r+%literal
        :+  %or  r+%charclass
        [r+%nonterminal not+arrow]
          :-  %literal
        [soq [%tar not+soq dot] soq sp]
          :-  %char
        :+  %or
          [t+'\\' (chas 't' 'n' ~)]
        dot
          :-  %charclass
        :*  t+'['
            :*  %tar
                not+t+']'
                [%or [dot t+'-' dot] r+%char]
            ==
            t+']'
            sp
        ==
          :-  %nonterminal
        [run+[%a %z] [%tar %or t+'-' run+[%a %z]] sp]
          :-  %sp
        tar+(chas ' ' 9 10 ~)
      ==
    =/  usr
      $@  ~
      $%  [%nt name=@tas]
          [%plan p=plan]
          [%gram g=gram]
      ==
    =/  pep  (hpeg @ta ,[@ud @ud @ta] usr ,~ ,~)
    =/  ascii-cords=toke.pep
      :-  |=(@ta `@`+<)
      =/  c  %*(. cut a 3, c 1)
      |=  [i=@ud len=@ud txt=@ta]
      ?:  (gte i len)  ~
      :_  [+(i) +<+]
      (c(b i, d txt))
    =/  expect-seq
      |=  t=tree.pep
      ~|  [%expect-seq t]
      ?>  ?=(^ -.t)
      t
    =/  expect-tar
      |=  t=tree.pep
      ~|  [%expect-tar t]
      ?>  ?=(%i -.t)
      l.t
    =/  expect-lus
      |=  t=tree.pep
      ~|  [%expect-lus t]
      ?>  ?=(%e -.t)
      l.t
    =/  expect-token
      ::  this should work for any tree, so it feels like it should
      ::  be a wet gate and outside of pep. or inside pep, moist.
      ::  or both.
      |=  t=tree.pep
      ~|  [%expect-token t]
      ?>  ?=(%t -.t)
      tok.t
    =/  seq-tail
      |=  [n=@ t=tree.pep]
      ~|  [%seq-tail n t]
      ?:  =(0 n)  t
      $(n (dec n), t q:(expect-seq t))
    =/  seq-head
      |=  t=tree.pep
      ~|  [%seq-head t]
      p:(expect-seq t)
    =/  seq-nth
      |=  [n=@ t=tree.pep]
      ~|  [%seq-nth n t]
      (seq-head (seq-tail n t))
    =/  expect-plan
      |=  t=tree.pep
      ~|  [%expect-plan t]
      ?>  ?=([%u %plan *] t)
      p.t
    =/  expect-nonterminal
      |=  t=tree.pep
      ~|  [%expect-nonterminal t]
      ?>  ?=([%u %nt *] t)
      name.t
    =/  plan-actions=mean.pep
      %-  sam.pep
      :~  :-  %grammar
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  item
          |=  t=tree.pep
          ^-  (pair @tas plan)
          =/  top  (expect-seq t)
          :-  (expect-nonterminal p.top)
          (expect-plan (seq-tail 2 q.top))
        =/  l      (expect-lus t)
        =/  first  (item i.l)
        =/  rest   (turn t.l item)
        [%u %gram (meg first rest)]
          :-  %pattern
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :+  %u  %plan
        =/  top  (expect-seq t)
        =/  mor=(lest tree.pep)
          :-  p.top
          %+  turn  (expect-tar q.top)
          |=  t=tree.pep
          (seq-tail 2 t)
        |-  ^-  plan
        =/  p=plan  (expect-plan i.mor)
        ?~  t.mor  p
        [%or p $(mor t.mor)]
          :-  %alternative
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :+  %u  %plan
        =/  item
          |=  t=tree.pep
          ~|  [%item t]
          ^-  plan
          =/  top  (expect-seq t)
          =/  suf  (expect-plan (seq-tail 1 q.top))
          ?+  p.top  !!
            [%o ~]        suf
            [%o %t %'!']  [%not suf]
            [%o %t %'&']  [%and suf]
          ==
        =/  kids  (expect-lus t)
        |-  ^-  plan
        =/  p  (item i.kids)
        ?~  t.kids  p
        [p $(kids t.kids)]
          :-  %suffix
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  top  (expect-seq t)
        =/  nut  (expect-plan p.top)
        =/  suf  (expect-tar q.top)
        :+  %u  %plan
        |-  ^-  plan
        ?~  suf  nut
        %=  $
          suf  t.suf
          nut  ?+  (expect-token (seq-head i.suf))  !!
                 %'*'  [%tar nut]
                 %'?'  [%wut nut]
                 %'+'  [%lus nut]
        ==     ==
          :-  %primary
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        ?+  t  (seq-nth 2 t)
          [%u %plan *]   t
          [%u %nt *]     u+plan+r+name.t
          [[%t %'.'] *]  u+plan+any+|
        ==
          :-  %tag
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  tel  (expect-seq (seq-tail 1 t))
        =<  u+plan+tag+.
        :-  (expect-nonterminal p.tel)
        (expect-plan (seq-head q.tel))
          :-  %mem
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        u+plan+mem+(expect-plan (seq-nth 2 t))
          :-  %literal
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  inside  (expect-tar (seq-nth 1 t))
        :+  %u  %plan
        ?~  inside  any+&
        |-  ^-  plan
        =/  tok=plan  t+(expect-token i.inside)
        ?~  t.inside  tok
        [tok $(inside t.inside)]
          :-  %char
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        ?:  ?=(%t -.t)  t
        :-  %t
        ?+  (expect-token (seq-tail 1 t))  !!
          %t  '\09'
          %n  '\0a'
        ==
          :-  %charclass
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  kids  (expect-tar (seq-nth 1 t))
        :+  %u  %plan
        ?~  kids  any+|
        =|  [s=(set @) r=(list (pair @ @))]
        |^  ^-  plan
            ?:  ?=(%t -.i.kids)
              next(s (~(put in s) tok.i.kids))
            =/  top  (expect-seq i.kids)
            =/  fo   (expect-token p.top)
            =/  to   (expect-token (seq-tail 1 q.top))
            next(r [[fo to] r])
        ++  aset
          ^-  plan
          ?:  ?=([* ~ ~] s)
            t+n.s
          set+s
        ++  next
          ^-  plan
          ?.  ?=(~ t.kids)
            $(kids t.kids)
          ?~  r  aset
          =/  runs
            |-  ^-  plan
            =/  one  [%run p.i.r q.i.r]
            ?~  t.r  one
            [%or one $(r t.r)]
          ?~  s  runs
          [%or aset runs]
        --
          :-  %nonterminal
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  top  (expect-seq t)
        :+  %u  %nt
        ^-  @tas
        %+  rep  3
        :-  (expect-token p.top)
        (turn (expect-tar p:(expect-seq q.top)) expect-token)
          :-  %sp
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        u+~
      ==
    =/  peg-compiled=exe.pep  (bake.pep peg-grammar plan-actions)
    =/  peg-parser
      |=  input=@ta
      =/  tos  [0 (met 3 input) input]
      =/  r    (pep ascii-cords no-memo peg-compiled tos ~ ~)
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
'''
grammar     <- (nonterminal '<-' sp pattern)+
pattern     <- alternative ('/' sp alternative)*
alternative <- ([!&]? sp suffix)+
suffix      <- primary ([*+?] sp)*
primary     <- '(' sp pattern ')' sp
             / {:tag '{:' nonterminal pattern '}' sp }
             / {:mem '{' sp pattern '}' sp }
             / '.' sp
             / literal
             / charclass
             / nonterminal !'<-'
literal     <- ['] (!['] .)* ['] sp
char        <- '\' [tn] / .
charclass   <- '[' ( !']' ( . '-' . / char ) )* ']' sp
nonterminal <- [a-z] [-a-z]* sp
sp          <- [ \t\n]*
'''
    %ok
=>
|%
::  as far as grammars are concerned, tokens are atoms.
::  if you have lexer, it must come paired with a function that
::  reduces (puffs) tokens to atoms for t, run, and set.
+$  gram  (pair @tas (map @tas plan))
+$  plan
  $~  [%any &]                    ::  empty
  $^  [p=plan q=plan]             ::  sequence
  $%  [%any e=?]                  ::  0 or 1 tokens
      [%t puf=@]                  ::  reduced token
      [%r name=@tas]              ::  rule reference
      [%or p=plan q=plan]         ::  ordered choice
      [%not p=plan]               ::  negative lookahead
      [%and p=plan]               ::  positive lookahead
      [%wut p=plan]               ::  ? optional
      [%tar p=plan]               ::  * zero or more
      [%lus p=plan]               ::  + one or more
      [%rep n=@ p=plan]           ::  p exactly n times
      [%run from=@ to=@]          ::  range [a-z]
      [%set set=(set @)]          ::  class [abd]
      [%tag name=@tas p=plan]     ::  labelled
      [%mem p=plan]               ::  memoize
  ==
++  wag                           ::  fragment for autocons types
  |*  [a=axis p=*]
  |-  ^+  p
  ?:  =(1 a)  p
  ?>  ?=(^ -.p)
  %=  $
    a  (mas a)
    p  ?:(=(2 (cap a)) -.p +.p)
  ==
++  dox
  ::  call dox on a (map key autocons-type)),
  ::  axis=(~(get by inx) key) at compile time,
  ::  (wag axis bat) at runtime.
  |*  m=(map)
  =+  ::  count and flatten
    =|  [n=@ kev=_~(tap by m)]
    |-  ^+  +<
    ?~  m   +<
    =/  l   $(m l.m, n +(n))
    $(m r.m, n n.l, kev kev.l)
  ?<  =(0 n)
  =*  p  ?~(m !! n.m)
  =|  [axe=_1 inx=(map _p.p axis)]
  =<  +
  |-  ^+  [kev=kev inx=inx bat=q.p]
  ?:  =(1 n)  
    ?~  kev  !!
    [t.kev (~(put by inx) p.i.kev axe) q.i.kev]
  =/  mid  (rsh 0 n)
  =/  l    $(n mid, axe (peg axe 2))
  =/  r    $(n (sub n mid), axe (peg axe 3), kev kev.l, inx inx.l)
  [kev.r inx.r bat.l bat.r]
++  meg
  |=  rs=(list (pair @tas plan))
  ?~  rs  !!
  [p.i.rs (~(gas by *(map @tas plan)) rs)]
++  chas
  |=  cs=(list @)
  ^-  plan
  set+(~(gas in *(set @)) cs)
--
|%
++  hpeg                          ::  types
  =|  $:  tom=mold                ::  token
          tos=mold                ::  tokenizer state
          usr=mold                ::  user trees
          sus=mold                ::  semantic state
          mem=mold                ::  memoize state
      ==
  |@
  ::  it is recommended to tokenize eagerly in order to efficiently
  ::  support backtracking: in that case the tokenizer state
  ::  usually contains the list of remaining tokens.
  ::  alternatively, the tokenizer can lazily read tokens as requested
  ::  from an input string. This trades space for time, as redundant
  ::  work will be done when backtracking, but this makes sense for
  ::  some tokenizers (ascii cords for example).
  ::
  ::  puff maps tokens to atoms.
  ::  when tokens are characters, it should be the id function.
  ::  also common is for tokens to have a "type" (%num, %name, etc)
  ::  in addition to the text matched and/or position information. 
  ::  that "type" is then what puff returns. The returned atoms are also
  ::  used for token classes and ranges. If token ranges are desired,
  ::  it is recommended that the types be numbered (%0, %1, etc.)
  ::  so that the ranges make sense.
  +$  puff  $-(tom @)
  +$  pass  $-(tos $@(~ [tok=tom sat=tos]))
  +$  toke  [puf=puff pas=pass]
  ::
  ::  the %u case enables (tagged) typed semantic actions.
  +$  tree
    $~  [%u *usr]
    $^  [p=tree q=tree]
    $%  [%u usr]                 :: user
        [%t tok=tom]             :: token(s)
        [%l name=@tas p=tree]    :: labelled
        [%o u=$@(~ tree)]        :: ?
        [%i l=(list tree)]       :: *
        [%e l=(lest tree)]       :: +
    ==
  ::  semantic actions have state(sus) and access to the position
  ::  at the beginning and end of a match. they can transform
  ::  what match trees look like, but not the language matched
  ::  by the grammar. state is discarded across backtracking.
  ::  for semantics "inside a rule", e.g. `!:`, use something like
  ::  [tag+zpcl-start+[t+'!' t+':'] r+%gap tag+zpcl-end+r+%tall]
  ::  and have the zpcl-start and zpcl-end actions turn the debug flag
  ::  on and off in the state.
  +$  act
    $-  [tree sus tos tos]
    [=tree =sus]
  ++  sam
    |=  (list (pair @tas act))
    (~(gas by *mean) +<)
  +$  mean  (map @tas act)
  ::
  ::  a compiled plan: symbolic rule references have been dox'd,
  ::  semantic actions have been embedded into the tree,
  ::  and operators have been numbered for maximal directness.
  +$  code
    $~  [%0 &]
    $^  [p=code q=code]
    $%  [%0 e=?]                             ::  any
        [%1 puf=@]                           ::  t
        [%2 =axis]                           ::  r
        [%3 p=code q=code]                   ::  or
        [%4 p=code]                          ::  not
        [%5 p=code]                          ::  wut
        [%6 p=code]                          ::  tar
        [%7 p=code]                          ::  lus
        [%8 from=@ to=@]                     ::  run
        [%9 set=(set @)]                     ::  set
        [%10 p=code sem=$@(@tas act)]        ::  tag
        [%11 p=code]                         ::  mem
    ==
  ::
  ::  gram -> woke as plan -> code
  +$  exe  [bat=code main=code]
  ++  bake
    |=  [=gram =mean]
    ^-  exe
    =+  (dox q.gram)
    =-  =/  axe  (~(got by inx) p.gram)
        =/  bat  $
        [bat (take p.gram (wag axe bat))]
    |%
    ++  take
      |=  [name=@tas cod=code]
      =/  sem  (test name cod)
      ?~(sem cod sem)
    ++  test
      |=  [name=@tas cod=code]
      ^-  $@(~ code)
      =/  sem  (~(get by mean) name)
      ?~  sem  ~
      [%10 cod u.sem]
    ++  $  ^-  code
    ?-  -.bat
      ^     [$(bat p.bat) $(bat q.bat)]
      %any  0+e.bat
      %t    1+puf.bat
      %r    (take name.bat [%2 (~(got by inx) name.bat)])
      %or   3+[$(bat p.bat) $(bat q.bat)]
      %not  4+$(bat p.bat)
      %and  4+4+$(bat p.bat)
      %wut  5+$(bat p.bat)
      %tar  6+$(bat p.bat)
      %lus  7+$(bat p.bat)
      %rep  ?:  =(0 n.bat)  0+&
            =/  one=code  $(bat p.bat)
            =/  out=code  one
            |-  ^+  out
            ?:  =(0 n.bat)  out
            $(n.bat (dec n.bat), out [one out])
      %run  8+[from.bat to.bat]
      %set  9+set.bat
      %tag  =/  cod  $(bat p.bat)
            =/  sem  (test name.bat cod)
            ?~  sem  [%10 cod name.bat]
            sem
      %mem  11+$(bat p.bat)
    ==  --
  ++  pre   |$(r $@(? [r=r =tos =sus]))    ::  parsing result
  +$  pro   (pre tree)                     ::  typical pre
  ++  gest  |$(r [pro=(pre r) mem=mem])    ::  product and memory
  +$  gast  (gest tree)                    ::  pro gest
  +$  look  $-([mem tos code] (unit pro))  ::  cache get
  +$  duct  $-([mem [tos code] pro] mem)   ::  cache put
  +$  hall  [=look =duct]                  ::  a way to remember pros
  ++  $
  |_  [toke hall exe =tos =sus =mem]
  ++  $  p:fin
  ++  fin
    ::   p=& means assert-success. unlikely.
    ::   p=| means match failure (no explanation).
    ::   otherwise, p is a match tree.
    ::   q can be reused for incremental parsing.
    =>  run
    [p=?@(pro pro r.pro) q=mem]
  ++  tope  ::  apply predicate to next token
    |=  f=$-(@ ?)
    ^-  gast
    :_  mem
    =/  t  (pas tos)
    ?~  t  |
    ?.  (f (puf tok.t))  |
    [[%t tok.t] sat.t sus]
  ++  loop  ::  apply match till it fails, list results
    =|  out=(list tree)
    |-  ^-  (gest (list tree))
    =/  r   run
    ?@  pro.r
      ?:  pro.r  r
      :_  mem.r
      [(flop out) tos sus]
    $(out [r.pro.r out], mem mem.r, tos tos.pro.r, sus sus.pro.r)
  ++  run
    |-  ^-  gast
    ?-  -.main
      ^
    =/  p  $(main p.main)
    ?@  pro.p  ?.(pro.p p $(main q.main, mem mem.p))
    =/  q  $(main q.main, mem mem.p, tos tos.pro.p, sus sus.pro.p)
    ?@  pro.q  ?.(pro.q q [pro.p mem.q])
    q(r.pro [r.pro.p r.pro.q])
      %0  ::  any
    :_  mem
    ?:  e.main  &  :: epsilon
    =/  t  (pas tos)
    ?~  t  |
    [[%t tok.t] sat.t sus]
      %1  ::  t
    (tope |=(a=@ =(puf.main a)))
      %2  ::  r
    $(main (wag axis.main bat))
      %3  ::  or
    =/  r  $(main p.main)
    ?.  ?=(%| pro.r)  r
    $(main q.main, mem mem.r)
      %4  ::  not
    =/  r  $(main p.main)
    :_  mem.r
    ?=(%| pro.r)
      %5  ::  wut
    =/  r  $(main p.main)
    :_  mem.r
    ?@  pro.r
      ?:  pro.r  &
      [[%o ~] tos sus]
    [[%o r.pro.r] tos.pro.r sus.pro.r]
      %6  ::  tar
    =/  r  loop(main p.main)
    ?@  pro.r  r
    r(r.pro i+r.pro.r)
      %7  ::  lus
    =/  p      $(main p.main)
    ?@  pro.p  p
    =/  q      loop(main p.main, tos tos.pro.p, sus sus.pro.p, mem mem.p)
    ?@  pro.q  q
    q(r.pro e+[r.pro.p r.pro.q])
      %8  ::  run
    (tope |=(a=@ &((gte a from.main) (lte a to.main))))
      %9  ::  set
    (tope |=(a=@ (~(has in set.main) a)))
      %10  ::  tag
    =/  r  $(main p.main)
    ?@  pro.r  r
    ?@  sem.main
      r(r.pro [%l sem.main r.pro.r])
    =/  ser  (sem.main r.pro.r sus.pro.r tos tos.pro.r)
    :_  mem.r
    [tree.ser tos.pro.r sus.ser]
      %11  ::  mem
    =/  key  [tos p.main]
    =/  got  (look mem key)
    ?^  got  [u.got mem]
    =/  r    $(main p.main)
    r(mem (duct mem key pro.r))
    ==
  --
  --
--
