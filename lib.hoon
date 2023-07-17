/-  *hpeg
|%
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
    =|  [n=@ kev=(list _?~(m !! n.m))]
    |-  ^+  +<
    ?~  m   +<
    =/  l   $(m l.m, n +(n), kev [n.m kev])
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
++  lib
=|  poly
|@  
++  sur  ~(. ^sur +<)
++  tool  !.
  |%
  ++  seq
    |=  t=tree:sur
    ?^  -.t  t
    ~|([%seq t] !!)
  ++  tar
    |=  t=tree:sur
    ^-  (list tree:sur)
    ?:  ?=(%r -.t)  +.t
    ~|([%tar t] !!)
  ++  lus
    |=  t=tree:sur
    ^-  (lest tree:sur)
    =/  r  (tar t)
    ?^  r  r
    ~|([%lus t] !!)
  ++  wut
    |=  t=tree:sur
    ^-  $@(~ tree:sur)
    =/  r  (tar t)
    ?~  r    ~
    ?~  t.r  i.r
    ~|([%wut t] !!)
  ++  token
    |=  t=tree:sur
    ?:  ?=(%t -.t)  tok.t
    ~|([%token t] !!)
  ++  drop
    |=  [n=@ t=tree:sur]
    ?:  =(0 n)  t
    $(n (dec n), t q:(seq t))
  ++  head  |=(t=tree:sur p:(seq t))
  ++  nth
    |=  [n=@ t=tree:sur]
    ~|  [%nth n t]
    (head (drop n t))
  --
++  sam
  |=  (list (pair @tas act:sur))
  (~(gas by *mean:sur) +<)
++  bake
  |=  [=gram m=mean:sur]
  ^-  exe:sur
  =+  %-  dox
      =+  m=q.gram  ::  decorate named plans with tags for actions
      |-  ^+  m
      ?~  m  ~
      :+  [p.n.m %tag p.n.m q.n.m]
        $(m l.m)
      $(m r.m)
  =-  :-  -
      (wag (~(got by inx) p.gram) -)
  =<  $
  |%
  ++  tag
    |=  [yel=? name=@tas p=plan]
    ^-  code:sur
    =/  cod  ^$(bat p)
    =/  sem  (~(get by m) name)
    ?~  sem
      ?.  yel  cod
      [%8 cod name]
    [%8 cod u.sem]
  ++  $  ^-  code:sur
  ?-  -.bat
    ^     [$(bat p.bat) $(bat q.bat)]
    %any  0+e.bat
    %t    1+puf.bat
    %r    2+(~(got by inx) name.bat)
    %or   3+[$(bat p.bat) $(bat q.bat)]
    %not  4+$(bat p.bat)
    %and  4+4+$(bat p.bat)
    %wut  5+[$(bat p.bat) 2]
    %tar  5+[$(bat p.bat) 0]
    %lus  5+[$(bat p.bat) 1]
    %min  5+[$(bat p.bat) n.bat 0]
    %max  5+[$(bat p.bat) 0 n.bat]
    %mid  5+[$(bat p.bat) lo.bat hi.bat]
    %rep  =-  5+[$(bat p.bat) -]
          ?+  n.bat  +(n.bat)
            %0  0     ::  [0 0] means *
            %1  [1 1] ::  weird but allowed
          ==
    %run  6+[from.bat to.bat]
    %set  7+set.bat
    %tag  (tag | name.bat p.bat)
    %yel  (tag & name.bat p.bat)
    %mem  9+$(bat p.bat)
  ==  --
++  exec
|_  [toke:sur hall:sur exe:sur =tos =sus =mem]
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
  ^-  gast:sur
  :_  mem
  =/  t  (pas tos)
  ?~  t  |
  ?.  (f (puf tok.t))  |
  [[%t tok.t] sat.t sus]
++  run
  |-  ^-  gast:sur
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
    %5  ::  loop
  =/  [inf=? min=@ max=@]
    ?+  n.main  =+(n=(dec n.main) [| n n])
      ^   [=(0 hi.n.main) lo.n.main hi.n.main]
      %0  [& 0 0]
      %1  [& 1 0]
      %2  [| 0 1]
    ==
  =>  .(main p.main)
  =|  [n=@ out=(list tree:sur)]
  |^  ^-  gast:sur
  =/  r  ^$
  =.  mem  mem.r
  ?@  pro.r
    ?:  pro.r  &+mem
    ?:  (lth n min)  |+mem
    ret
  =.  n  +(n)
  ?.  |(inf (lte n max))  ret
  $(out [r.pro.r out], tos tos.pro.r, sus sus.pro.r)
  ++  ret  [[[%r (flop out)] tos sus] mem]
  --
    %6  ::  run
  (tope |=(a=@ &((gte a lo.main) (lte a hi.main))))
    %7  ::  set
  (tope |=(a=@ (~(has in set.main) a)))
    %8  ::  tag
  =/  r  $(main p.main)
  ?@  pro.r  r
  ?@  sem.main
    r(r.pro [%l sem.main r.pro.r])
  =/  ser  (sem.main r.pro.r sus.pro.r tos tos.pro.r)
  :_  mem.r
  [tree.ser tos.pro.r sus.ser]
    %9  ::  mem
  =/  key  [tos p.main]
  =/  got  (look mem key)
  ?^  got  [u.got mem]
  =/  r    $(main p.main)
  r(mem (duct mem key pro.r))
  ==
--
--
--
