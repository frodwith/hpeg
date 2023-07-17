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
      [%mid lo=@ hi=@ p=plan]        ::  between lo and hi ps
      [%rep n=@ p=plan]              ::  exactly n ps
      [%run from=@ to=@]             ::  range [a-z]
      [%set set=(set @)]             ::  class [abd]
      [%tag name=@tas p=plan]        ::  group for semantic actions
      [%yel name=@tas p=plan]        ::  tag with forced %l
      [%mem p=plan]                  ::  memoize
  ==
+$  poly                      ::  types hpeg is polymorphic over
  $:  tom=mold                ::  token
      tos=mold                ::  tokenizer state
      usr=mold                ::  user trees
      sus=mold                ::  semantic state
      mem=mold                ::  memoize state
  ==
--
|%
++  sur
  =|  poly
  |@
  ::  The tokenizer doesn't have persistent state across
  ::  backtracking, and so can't avoid repeating work when 
  ::  backtracking happens. A lot of thought convinced us this 
  ::  was correct: you can't usefully avoid repeating work without
  ::  getting tokens "pushed back" in the correct order during
  ::  backtracking, and that's too expensive to be worth the 
  ::  savings you'd get by not repeating tokenizer work. It is 
  ::  therefore recommended to tokenize eagerly in order to
  ::  support backtracking: in that case the tokenizer state
  ::  usually contains the list of remaining tokens.
  ::  alternatively, the tokenizer can lazily read tokens as
  ::  requested from an input string. This trades space for time,
  ::  as redundant work will be done when backtracking, but this
  ::  makes sense for some tokenizers (ascii cords for example).
  ::
  ::  puff maps tokens to atoms.
  ::  when tokens are characters, it should be the id function.
  ::  also common is for tokens to have a "type" (%num, %name, etc)
  ::  in addition to the text matched and/or position information. 
  ::  that "type" is then what puff returns. The returned atoms
  ::  are also used for token classes and ranges. If token ranges
  ::  are desired, it is recommended that the types be numbered 
  ::  (%0, %1, etc.) so that the ranges make sense.
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
  ::  and have the zpcl-start and zpcl-end actions turn the debug
  ::  flag on and off in the state.
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
    $%  [%0 e=?]                          ::  any
        [%1 puf=@]                        ::  t
        [%2 =axis]                        ::  r
        [%3 p=code q=code]                ::  or
        [%4 p=code]                       ::  not
        ::  5's n can be shortened to one atom for compactness:
        ::  0->[0 0], 1->[1 0], 2->[0 1], n->[n-1 n-1]
        [%5 p=code n=$@(@ [lo=@ hi=@])]   ::  min,max,mid,rep,+,*,?
        [%6 lo=@ hi=@]                    ::  run
        [%7 set=(set @)]                  ::  set
        [%8 p=code sem=$@(@tas act)]      ::  tag
        [%9 p=code]                       ::  mem
    ==
  ::
  ::  gram -> exe as plan -> code
  +$  exe  [bat=code main=code]
  ::
  +$  pro   $@(? [r=tree =tos =sus])       ::  parsing result
  +$  gast  [=pro =mem]                    ::  product and memory
  +$  look  $-([mem tos code] (unit pro))  ::  cache get
  +$  duct  $-([mem [tos code] pro] mem)   ::  cache put
  +$  hall  [=look =duct]                  ::  a way to remember pros
--
--
