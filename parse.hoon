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
        :+  %or  [t+'.' sp]
        :+  %or  r+%literal
        :+  %or  r+%charclass
        [r+%nonterminal not+arrow]
          :-  %literal
        [soq [%tar not+soq dot] soq sp]
          :-  %charclass
        :*  t+'['
            :*  %tar
                not+t+']'
                [%or [dot t+'-' dot] dot]
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
      ?>  ?=(%r -.t)
      kids.t
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
      (seq-head (seq-tail (dec n) t))
    =/  expect-plan
      |=  t=tree.pep
      ~|  [%expect-plan t]
      ?>  ?=([%u %plan *] t)
      p.t
    =/  plan-actions=mean.pep
      %-  sam.pep
      :~  :-  %grammar
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  item
          |=  t=tree.pep
          ^-  (pair @tas plan)
          =/  top  (expect-seq t)
          ?>  ?=([%u %nt *] p.top)
          :-  name.p.top
          (expect-plan (seq-tail 2 q.top))
        =/  top  (expect-seq t)
        =/  first  (item p.top)
        =/  rest   (turn (expect-tar q.top) item)
        [%u %gram (meg first rest)]
          :-  %pattern
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  top  (expect-seq t)
        =/  nut  (expect-plan p.top)
        =/  mor  (expect-tar q.top)
        :+  %u  %plan
        |-  ^-  plan
        ?~  mor  nut
        $(mor t.mor, nut [%or nut (expect-plan (seq-tail 2 i.mor))])
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
        =/  top   (expect-seq t)
        =/  hed   (item p.top)
        =/  kids  (expect-tar q.top)
        ?~  kids  hed
        :-  hed
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
        ?+  t  (seq-nth 3 t)
          [%u %plan *]   t
          [%u %nt *]     u+plan+r+name.t
          [[%t %'.'] *]  u+plan+any+|
        ==
          :-  %literal
        |=  [t=tree.pep *]
        ~|  [%expect-literal t]
        :_  ~  ^-  tree.pep
        =/  inside  (expect-tar (seq-nth 2 t))
        :+  %u  %plan
        ?~  inside  any+&
        |-  ^-  plan
        =/  tok=plan  t+(expect-token i.inside)
        ?~  t.inside  tok
        [tok $(inside t.inside)]
          :-  %charclass
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  kids  (expect-tar (seq-nth 2 t))
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
    %-  peg-parser
'''
grammar <- (nonterminal '<-' sp pattern)+
pattern <- alternative ('/' sp alternative)*
alternative <- ([!&]? sp suffix)+
suffix <- primary ([*+?] sp)*
primary <- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<-'
literal <- ['] (!['] .)* ['] sp
charclass <- '[' (!']' (. '-' . / .))* ']' sp
nonterminal <- [a-z] [-a-z]* sp
sp <- [ \t\n]*
'''
    ::%ok
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
      [%tar p=plan]               ::  * zero or more
      [%lus p=plan]               ::  + one or more
      [%rep n=@ p=plan]           ::  p exactly n times
      [%wut p=plan]               ::  ? optional
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
  ::  some tokenizers (ascii for example).
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
        [%l name=@tas kid=tree]  :: labelled
        [%o kid=$@(~ tree)]      :: ? 
        [%r kids=(list tree)]    :: *
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
        [%5 p=code]                          ::  tar
        [%6 p=code]                          ::  wut
        [%7 from=@ to=@]                     ::  run
        [%8 set=(set @)]                     ::  set
        [%9 p=code sem=$@(@tas act)]         ::  tag
        [%10 p=code]                         ::  mem
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
      [%9 cod u.sem]
    ++  $  ^-  code
    ?-  -.bat
      ^     [$(bat p.bat) $(bat q.bat)]
      %any  0+e.bat
      %t    1+puf.bat
      %r    (take name.bat [%2 (~(got by inx) name.bat)])
      %or   3+[$(bat p.bat) $(bat q.bat)]
      %not  4+$(bat p.bat)
      %and  4+4+$(bat p.bat)
      %tar  5+$(bat p.bat)
      %wut  6+$(bat p.bat)
      %lus  =/  c  $(bat p.bat)
            [c %5 c]
      %rep  ?:  =(0 n.bat)  0+&
            =/  one=code  $(bat p.bat)
            =/  out=code  one
            |-  ^+  out
            ?:  =(0 n.bat)  out
            $(n.bat (dec n.bat), out [one out])
      %run  7+[from.bat to.bat]
      %set  8+set.bat
      %tag  =/  cod  $(bat p.bat)
            =/  sem  (test name.bat cod)
            ?~  sem  [%9 cod name.bat]
            sem
      %mem  10+$(bat p.bat)
    ==  --
  +$  pro   $@(? [=tree =tos =sus])        ::  internal parse product
  +$  gast  [=pro =mem]                    ::  product and memory ghost
  +$  look  $-([mem tos code] (unit pro))  ::  cache get
  +$  duct  $-([mem [tos code] pro] mem)   ::  cache put
  +$  hall  [=look =duct]                  ::  a way to remember pros
  ++  $
  |_  [toke hall exe =tos =sus =mem]
  ++  $  p:run
  ++  tope  ::  apply predicate to next token
    |=  f=$-(@ ?)
    ^-  gast
    :_  mem
    =/  t  (pas tos)
    ?~  t  |
    ?.  (f (puf tok.t))  |
    [[%t tok.t] sat.t sus]
  ++  run
    ::   p=& means assert-success. unlikely.
    ::   p=| means match failure (no explanation).
    ::   otherwise, p is a match tree.
    ::   q can be reused for incremental parsing.
    =<  [p=?@(pro pro tree.pro) q=mem]
    |-  ^-  gast
    ?-  -.main
      ^
    =/  p  $(main p.main)
    ?@  pro.p  ?.(pro.p p $(main q.main, mem mem.p))
    =/  q  $(main q.main, mem mem.p, tos tos.pro.p, sus sus.pro.p)
    ?@  pro.q  ?.(pro.q q [pro.p mem.q])
    q(tree.pro [tree.pro.p tree.pro.q])
      %0
    :_  mem
    ?:  e.main  &  :: epsilon
    =/  t  (pas tos)
    ?~  t  |
    [[%t tok.t] sat.t sus]
      %1
    (tope |=(a=@ =(puf.main a)))
      %2
    $(main (wag axis.main bat))
      %3
    =/  r  $(main p.main)
    ?.  ?=(%| pro.r)  r
    $(main q.main, mem mem.r)
      %4
    =/  r  $(main p.main)
    :_  mem.r
    ?=(%| pro.r)
      %5
    =|  out=(list tree)
    |-  ^-  gast
    =/  r   ^$(main p.main)
    ?@  pro.r
      ?:  pro.r  r
      :_  mem.r
      [[%r (flop out)] tos sus]
    $(out [tree.pro.r out], mem mem.r, tos tos.pro.r, sus sus.pro.r)
      %6
    =/  r  $(main p.main)
    :_  mem.r
    ?@  pro.r
      ?:  pro.r  &
      [[%o ~] tos sus]
    [[%o tree.pro.r] tos.pro.r sus.pro.r]
      %7
    (tope |=(a=@ &((gte a from.main) (lte a to.main))))
      %8
    (tope |=(a=@ (~(has in set.main) a)))
      %9
    =/  r  $(main p.main)
    ?@  pro.r  r
    ?@  sem.main
      r(tree.pro [%l sem.main tree.pro.r])
    =/  ser  (sem.main tree.pro.r sus.pro.r tos tos.pro.r)
    :_  mem.r
    [tree.ser tos.pro.r sus.ser]
      %10
    =/  key  [tos p.main]
    =/  got  (look mem key)
    ?^  got  [u.got mem]
    =/  r    $(main p.main)
    r(mem (duct mem key pro.r))
    ==
  --
  --
--
