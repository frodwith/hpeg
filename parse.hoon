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
        lus+tag+def+[r+%nonterminal arrow sp r+%pattern]
          :-  %pattern
        :-  r+%alternative
        tar+tag+pat+[t+'/' sp r+%alternative]
          :-  %alternative
        lus+tag+alt+[wut+(chas '!' '&' ~) sp r+%suffix]
          :-  %count
        lus+[%run '0' '9']
          :-  %suffix
        :-  r+%primary  :_  sp  :-  %wut
        :+  %or  (chas '*' '+' '?' ~)
        :-  t+'{'  :_  t+'}'
        :+  %or  [t+',' r+%count]
        [r+%count %wut t+',' wut+r+%count]
          :-  %primary
        :-  %or  :_
          :+  %or  r+%literal
          :+  %or  r+%charclass
          [r+%nonterminal not+arrow]
        :+  %tag  %head
        :_  sp
        :-  %or  :_  t+'.'
        :^  %tag  %group  t+'('  :_  [sp r+%pattern t+')']
        [%wut %tag %type %or t+'#' t+':' r+%identifier wut+t+'!']
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
                [%or tag+range+[dot t+'-' dot] r+%char]
            ==
            t+']'
            sp
        ==
          :-  %identifier
        [run+[%a %z] [%tar %or run+[%a %z] t+'-']]
          :-  %nonterminal
        tag+head+[r+%identifier sp]
          :-  %sp
        tar+(chas ' ' 9 10 ~)
      ==
    =/  usr
      $@  ?(%sp %memo)
      $%  [%tag name=@tas yel=?]
          [%nt name=@tas]
          [%plan p=plan]
          [%gram g=gram]
          [%num n=@]
          [%run from=@ to=@]
          [%def name=@tas p=plan]
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
      %-  sam.pep
      :~  :-  %def
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :+  %u  %def
        =/  top  (expect-seq t)
        :-  (expect-nonterminal p.top)
        (expect-plan (seq-drop 2 q.top))
          :-  %grammar
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  l      ts:(expect-lus t)
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
          :-  %pattern
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :+  %u  %plan
        =/  top  (expect-seq t)
        =/  mor=(lest tree.pep)
          :-  p.top
          ts:(expect-tar q.top)
        |-  ^-  plan
        =/  p=plan  (expect-plan i.mor)
        ?~  t.mor  p
        [%or p $(mor t.mor)]
          :-  %alt
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
        $(n (add n (mul pac (sub (expect-token i.dig) '0'))))
          :-  %suffix
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :+  %u  %plan  ^-  plan
        =/  top  (expect-seq t)
        =/  nut  (expect-plan p.top)
        =/  suf  (expect-wut (seq-head q.top))
        ?~  suf  nut
        ?@  -.suf
          ?+  (expect-token suf)  !!
            %'*'  [%tar nut]
            %'?'  [%wut nut]
            %'+'  [%lus nut]
          ==
        =/  qua  (expect-seq p:(expect-seq q.suf))
        ?:  ?=([%t %','] p.qua)
          [%max (expect-count q.qua) nut]
        =/  min  (expect-count p.qua)
        =/  two  (expect-wut q.qua)
        ?~  two  [%rep min nut]
        =/  max  (expect-wut (seq-drop 1 two))
        ?~  max  [%min min nut]
        [%mid min (expect-count max) nut]
          :-  %type
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        :-  %u
        ?:  ?=(%t -.t)  %memo
        =/  tel  (expect-seq (seq-drop 1 t))
        =/  nam  (expect-nonterminal p.tel)
        =/  zap  (expect-wut q.tel)
        [%tag (expect-nonterminal p.tel) !?=(~ zap)]
          :-  %head
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        (seq-head t)
          :-  %group
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  tel  (expect-seq (seq-drop 1 t))
        =/  typ  (expect-wut p.tel)
        =/  pin  (seq-head (seq-drop 1 q.tel))
        ?~  typ  pin
        :+  %u  %plan  ^-  plan
        ?>  ?=(%u -.typ)
        =/  lan  (expect-plan pin)
        ?+  +.typ  !!
          %memo     [%mem lan]
          [%tag *]  =/  nap  [name.typ lan]
                    ?:(yel.typ yel+nap tag+nap)
        ==
          :-  %primary
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        ?+  t  t
          [%u %nt *]  u+plan+r+name.t
          [%t %'.']   u+plan+any+|
        ==
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
          %t  '\09'
          %n  '\0a'
        ==
          :-  %range
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  top   (expect-seq t)
        =/  fo=@  (expect-token p.top)
        =/  to=@  (expect-token (seq-drop 1 q.top))
        [%u %run fo to]
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
          |-  ^-  plan
          =/  i  (el i.out)
          ?~  t.out  i
          [%or $(out t.out) i]
        --
          :-  %identifier
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        =/  top  (expect-seq t)
        :+  %u  %nt
        ^-  @tas
        %+  rep  3
        :-  (expect-token p.top)
        (turn ts:(expect-tar q.top) expect-token)
          :-  %sp
        |=  [t=tree.pep *]
        :_  ~  ^-  tree.pep
        u+%sp
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
::    added {1,2}-style suffixes
'''
grammar     <- (:def nonterminal '<-' sp pattern)+
pattern     <- alternative (:pat '/' sp alternative)*
alternative <- (:alt [!&]? sp suffix)+
count       <- [0-9]+
suffix      <- primary (
                 [*+?]
                 / '{' (
                   ',' count
                   / count (',' count?)?
                 ) '}'
               )? sp
primary     <- (:head ((:group '('
                  (:type '#' / (':' identifier '!'? ))?
                  sp pattern ')')
                / '.' ) sp)
               / literal
               / charclass
               / nonterminal !'<-'
literal     <- ['] (!['] .)* ['] sp
char        <- '\' [tn] / .
charclass   <- '[' ( !']' ( (:range . '-' .) / char ) )* ']' sp
identifier  <- [a-z] [a-z-]*
nonterminal <- (:head identifier sp)
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
  $~  [%any &]                       ::  empty
  $^  [p=plan q=plan]                ::  sequence
  $%  [%any e=?]                     ::  0 or 1 tokens
      [%t puf=@]                     ::  reduced token
      [%r name=@tas]                 ::  rule reference
      [%or p=plan q=plan]            ::  ordered choice
      [%not p=plan]                  ::  negative lookahead
      [%and p=plan]                  ::  positive lookahead
      [%wut p=plan]                  ::  ? optional
      [%tar p=plan]                  ::  * 0 or more
      [%lus p=plan]                  ::  + 1 or more
      [%min n=@ p=plan]              ::  at least n ps
      [%max n=@ p=plan]              ::  at most n ps
      [%mid from=@ to=@ p=plan]      ::  between from and to ps
      [%rep n=@ p=plan]              ::  exactly n ps
      [%run from=@ to=@]             ::  range [a-z]
      [%set set=(set @)]             ::  class [abd]
      [%tag name=@tas p=plan]        ::  group for semantic actions
      [%yel name=@tas p=plan]        ::  tag with forced %l in output
      [%mem p=plan]                  ::  memoize
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
--
::  TODO: separate tokenizer state into a reader type, a 'transient'
::        state type (reset across backtracking), and a 'persistent'
::        state type that gets threaded around with the memo table.
::        this allows us to have lazy token buffers, bounded or not,
::        at the caller's pleasure, in order to avoid repeating
::        tokenization work.
::  also: after doing the above and verifying by print that it solves
::        the repeated token reads, adjust the nonterminal rule so that
::        it tries a-z before -. then run with an unbuffered tokenizer
::        to verify that gets rid of a lot of the double token reads.
::        this probably means forcibly respecting the order in the
::        charclass rule and only making sets when you have adjacent
::        (non-range-interspersed) characters in the set.
::  also: clean up semantic actions more in the example grammar.
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
        [%r n=@ l=(list tree)]   :: repetition
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
        [%5 n=@ p=code]                      ::  min
        [%6 n=@ p=code]                      ::  max
        [%7 from=@ to=@ p=code]              ::  mid
        [%8 n=@ p=code]                      ::  rep
        [%9 from=@ to=@]                     ::  run
        [%10 set=(set @)]                    ::  set
        [%11 p=code sem=$@(@tas act)]        ::  tag
        [%12 p=code]                         ::  mem
    ==
  ::
  ::  gram -> exe as plan -> code
  +$  exe  [bat=code main=code]
  ++  bake
    |=  [=gram =mean]
    ^-  exe
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
      ^-  code
      =/  cod  ^$(bat p)
      =/  sem  (~(get by mean) name)
      ?~  sem
        ?.  yel  cod
        [%11 cod name]
      [%11 cod u.sem]
    ++  $  ^-  code
    ?-  -.bat
      ^     [$(bat p.bat) $(bat q.bat)]
      %any  0+e.bat
      %t    1+puf.bat
      %r    2+(~(got by inx) name.bat)
      %or   3+[$(bat p.bat) $(bat q.bat)]
      %not  4+$(bat p.bat)
      %and  4+4+$(bat p.bat)
      %wut  7+[0 1 $(bat p.bat)]
      %tar  5+[0 $(bat p.bat)]
      %lus  5+[1 $(bat p.bat)]
      %min  5+[n.bat $(bat p.bat)]
      %max  6+[n.bat $(bat p.bat)]
      %mid  7+[from.bat to.bat $(bat p.bat)]
      %rep  8+[n.bat $(bat p.bat)]
      %run  9+[from.bat to.bat]
      %set  10+set.bat
      %tag  (tag | name.bat p.bat)
      %yel  (tag & name.bat p.bat)
      %mem  12+$(bat p.bat)
    ==  --
  +$  pro   $@(? [r=tree =tos =sus])          ::  parsing result
  +$  gast  [=pro =mem]                    ::  product and memory
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
  ++  loop
    |=  [min=@ max=@]
    =|  [n=@ out=(list tree)]
    |^  ^-  gast
    =/  r  run
    =.  mem  mem.r
    ?@  pro.r
      ?:  pro.r  &+mem
      ?:  (lth n min)  |+mem
      ret
    =.  n  +(n)
    ?.  |(=(0 max) (lte n max))  ret
    $(out [r.pro.r out], tos tos.pro.r, sus sus.pro.r)
    ++  ret  [[[%r n (flop out)] tos sus] mem]
    --
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
      %5  ::  min
    (loop(main p.main) n.main 0)
      %6  ::  max
    (loop(main p.main) 0 n.main)
      %7  ::  mid
    (loop(main p.main) from.main to.main)
      %8  ::  rep
    (loop(main p.main) n.main n.main)
      %9  ::  run
    (tope |=(a=@ &((gte a from.main) (lte a to.main))))
      %10  ::  set
    (tope |=(a=@ (~(has in set.main) a)))
      %11  ::  tag
    =/  r  $(main p.main)
    ?@  pro.r  r
    ?@  sem.main
      r(r.pro [%l sem.main r.pro.r])
    =/  ser  (sem.main r.pro.r sus.pro.r tos tos.pro.r)
    :_  mem.r
    [tree.ser tos.pro.r sus.ser]
      %12  ::  mem
    =/  key  [tos p.main]
    =/  got  (look mem key)
    ?^  got  [u.got mem]
    =/  r    $(main p.main)
    r(mem (duct mem key pro.r))
    ==
  --
  --
--
