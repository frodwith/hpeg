::  at some point a visualizer tool would be cool,
::  but we probably do that in javascript. or with mark's
::  new ncurses stuff?  following guido's visualiser?
!:
:-  %say
|=  *
:-  %noun
=<  =/  hel  (hpeg @tD tape ,~ ,~ ,~)
    =/  tapes=toke.hel
      :-  |=(@tD `@`+<)
      |=(tape +<)
    =/  no-memo=hall.hel  
      =/  nil  |=(* ~)
      [nil nil]
    =/  gel=gram
      :-  %$  :_  [~ ~]  :-  %$
      [t+%h t+%e t+%l t+%l t+%o t+',']
    =/  compiled=exe.hel  (bake.hel gel ~)
    ?>  .=  [t+%h t+%e t+%l t+%l t+%o t+',']
        %-  hel  
        :*  tapes
            no-memo
            compiled
            "hello, anything!"
            [~ ~]
        ==
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
        [%2 =axis act=$@(~ act)]             ::  r
        [%3 p=code q=code]                   ::  or
        [%4 p=code]                          ::  not
        [%5 p=code]                          ::  rep
        [%6 p=code]                          ::  opt
        [%7 from=@ to=@]                     ::  run
        [%8 set=(set @)]                     ::  set
        [%9 p=code sem=$@(@tas act)]         ::  tag
        [%10 p=code]                         ::  mem
    ==
  ::
  ::  gram -> woke as plan -> code
  +$  exe  [main=code bat=code]
  ++  bake
    |=  [=gram =mean]
    ^-  exe
    =+  (dox q.gram)
    =-  :_  -
        (wag (~(got by inx) p.gram) -)
    |-  ^-  code
    ?-  -.bat
      ^     [$(bat p.bat) $(bat q.bat)]
      %any  0+e.bat
      %t    1+puf.bat
      %r    :+  %2  (~(got by inx) name.bat)
            =/  got  (~(get by mean) name.bat)
            ?~  got  ~  u.got
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
      %tag  :+  %9  $(bat p.bat)
            =/  act  (~(get by mean) name.bat)
            ?~  act  name.bat  u.act
      %mem  10+$(bat p.bat)
    ==
  +$  pro   $@(? [=tree =tos =sus])        ::  internal parse product
  +$  gast  [=pro =mem]                    ::  product and memory ghost
  +$  look  $-([mem tos code] (unit pro))  ::  cache get
  +$  duct  $-([mem [tos code] pro] mem)   ::  cache put
  +$  hall  [=look =duct]                  ::  a way to remember pros
  ++  $
  |_  [toke hall exe =tos =sus =mem]
  ++  $  p:run
  ++  take  ::  apply a semantic action
    |=  [a=act r=gast]
    ^-  gast
    ?@  pro.r  r
    =/  ser  (a tree.pro.r sus.pro.r tos tos.pro.r)
    :_  mem.r
    [tree.ser tos.pro.r sus.ser]
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
    =/  tgt  (wag axis.main bat)
    =*  cal  $(main tgt)
    ?~  act.main  cal
    (take act.main cal)
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
    (tope |=(a=@ &((lte a from.main) (gte a to.main))))
      %8
    (tope |=(a=@ (~(has in set.main) a)))
      %9
    =/  r  $(main p.main)
    ?^  sem.main
      (take sem.main r)
    ?@  pro.r  r
    r(tree.pro [%l sem.main tree.pro.r])
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
