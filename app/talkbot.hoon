::
::  /app/talkbot/hoon
::
::TODO's and ideas:
::  -  try finding more metadata for %webpage assists. description?
::  -  give descriptions for runes and stdlib functions when asked.
::  -  keep track of last-msg-time, scnd-last-msg-time, and if the current
::      message is a "hello?" or similar, send an automated welcoming message if
::      nobody else replies within two minutes.
::  -  watch urbit repos for prs and issues, and post about them in urbit-meta.
::  -  have it tip someone (random 13 25) dogecoin whenever they "praise ~zod",
::      effectively paying them to turn us into a cult. -- 2018 business model
::  -  have it say "praise ~zod" every time something good happens (ie someone
::      says "hurray" or "problem solved!"). to make this work best, include a
::      hella long list of specific triggers, and vary between using ! and .
::      for punctuation.
::  -  "talkbot, listen", followed by a statement makes talkbot remember it.
::      ie "urbit is fun" makes it remember that phrase for urbit definitions.
::  -  actually port !chopra to hoon
::  -  quote generators (like !chopra) for !moldbug, !chomsky, !plato
::  -  keep map of birthdays, congratulating that person the first time they
::      send a message on that date.
::  -  (map cord @ud)  "+1 for ..." increments ... in map.
::  -  if message includes "(but) don't quote me on (this|that)", add to quotes
::      and quote immediately
::
/-  hall, gh
/+  hall
!:
::
[. hall]
=>  ::>  ||
    ::>  ||  %arch
    ::>  ||
    ::
    |%
    ++  state                                           ::<  full app state
      $:  streams/(map circle stream)                   ::<  what we follow
          known/(map serial (pair circle @ud))          ::<  all known messages
          ignoring/(set ship)                           ::<  shy people
          latest/(map term *)                           ::<  for polling
          simples/(map term tape)                       ::<  static replies
      ==                                                ::
    ++  stream                                          ::<  stream state
      $:  grams/(list telegram)                         ::<  words
          count/@ud                                     ::<  (lent grams)
          ::  [3 2 1 ~] , 3
          ::  (sub count +(i))
          people/(map ship person)                      ::<  people + msgs
          behavior/role                                 ::<  how to act
      ==                                                ::
    ++  person                                          ::<  per-person data
      $:  messages/(list @ud)                           ::<  stream msgs
          ::  in order to prevent abuse, we give more   ::
          ::  active triggerers longer cooldowns.       ::
          ::  for every message we react to, a ship's   ::
          ::  heat gets doubled. for every five seconds ::
          ::  that pass, one heat point is lost.        ::
          ::  when a message is sent, we round down in  ::
          ::  our calculation of remaining heat.        ::
          heat/@ud                                      ::<  activity score
          last/@da                                      ::<  time of last msg
      ==                                                ::
    ::                                                  ::
    ++  role  ?($full $polite)                          ::<  behavior
    ::                                                  ::
    ++  control                                         ::<  user control
      $%  {$join cir/circle rol/role}                   ::
          {$leave cir/circle}                           ::
          {$test $~}                                    ::
      ==                                                ::
    ::                                                  ::
    ++  move  (pair bone card)                          ::
    ++  card                                            ::
      $%  {$peer wire dock path}                        ::
          {$pull wire dock $~}                          ::
          {$poke wire dock pear}                        ::
          $:  $hiss                                     ::
              wire                                      ::
              (unit user:eyre)                          ::
              mark                                      ::
              {$purl purl:eyre}                         ::
          ==                                            ::
      ==                                                ::
    ++  pear                                            ::
      $%  {$hall-action action}                         ::
          {$helm-hi cord}                               ::
      ==                                                ::
    ::                                                  ::
    ++  delta                                           ::
      $%  ::  state changes
          {$join cir/circle rol/role}
          {$learn cir/circle gam/telegram}
          {$ignore who/ship ign/?}
          ::  side-effects
          {$sub sub/? cir/circle}
          {$say cir/circle gam/telegram res/(list reply)}
      ==
    ++  reply  ?(speech-reply move-reply)
    ++  speech-reply
      $%  {$simple wat/term}
          {$speech sep/speech}
          {$ignore ign/?}
          {$milestone num/@ud}
          {$hey $~}
          {$bye $~}
          {$bark $~}
          {$urbit $~}
          {$source $~}
          {$oracle $~}
          {$uifail $~}
          [$quote $~]
          {$meme $~}
      ==
    ++  move-reply
      $%  {$ping $~}
          {$chopra $~}
          {$github own/cord rep/cord wat/request-github}
          {$youtube wat/cord}
          {$pastebin wat/cord}
          {$webpage ret/?}
      ==
    ++  request-github
      $%  {$repo $~}
          {$issue num/cord}
          {$commit has/cord}
      ==
    ++  weir
      $%  {$ping uid/serial wen/@da}
          {$github uid/serial own/cord rep/cord}
          {$other uid/serial}
      ==
    --
::
::>  ||
::>  ||  %work
::>  ||
::>    functional cores and arms
::
|_  {bol/bowl:gall state}
::
++  prep
  ::>  adapts state.
  ::
  |=  old/(unit state)
  ^-  (quip move _..prep)
  :-  ~
  ?~  old
    ..prep(simples get-simples)
  ..prep(+<+ u.old(simples get-simples))
::
++  poke-noun
  |=  con/control
  ^-  (quip move _+>)
  ?-  -.con
    $join   %-  pre-bake
            :~  [%join cir.con rol.con]
                [%sub & cir.con]
            ==
    $leave  (bake [%sub | cir.con])
    $test   !!
  ==
::
++  diff-hall-prize
  |=  {wir/wire piz/prize}
  ~&  %ignoring-prize
  [~ +>]
::
++  diff-hall-rumor
  |=  {wir/wire rum/rumor}
  ^-  (quip move _+>)
  ?>  ?=({$circle $gram *} rum)
  %-  pre-bake  =<  ta-done
  (ta-open:ta (wire-source wir) nev.rum.rum)
::
++  coup-ping
  |=  {wir/wire tan/(unit tang)}
  ^-  (quip move _+>)
  =+  wer=(unwire (welp /ping wir))
  ?>  ?=($ping -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  (ta-coup-ping:(taa uid.wer) wen.wer)
::
++  sigh-tang
  |=  {wir/wire tan/tang}
  ~&  tan
  [~ +>]
::
++  sigh-httr
  |=  {wir/wire cod/@ud hed/mess:eyre bod/(unit octs)}
  ^-  (quip move _+>)
  ?.  |(?=({@ta @ta $~} wir) ?=({$redirect @ta @ta $~} wir))
    ~&([%invalid-httr-wir wir] [~ +>.$])
  ?:  &(?=(~ bod) (gte cod 300) (lth cod 400) !?=({$redirect *} wir))
    ::  turn headers into a map, make keys lowercase
    =/  hes
      %-  ~(gas by *(map tape @t))
      %+  turn  hed
      |=((pair @t @t) [(cass (trip p)) q])
    ?.  (~(has by hes) "location")  [~ +>.$]
    =/  gam
      %*  .  *telegram
          uid
        ?>  ?=({@ta @ta $~} wir)
        (slav %uv i.t.wir)
          sep
        :-  %url
        :_  ~
        %-  need
        (de-purl:html (~(got by hes) "location"))
      ==
    da-done:(~(da-do-reply da ~ *circle gam ~) %webpage &)
  ?~  bod  [~ +>]
  =?  wir  ?=({$redirect *} wir)  t.wir
  =+  wer=(unwire wir)
  ?>  ?=($other -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  (ta-sigh-httr:(taa uid.wer) [wir u.bod])
::
++  sigh-json
  |=  {wir/wire jon/json}
  ^-  (quip move _+>)
  ?>  ?=({$youtube @ $~} wir)
  =/  res
    ^-  (unit {items/(list {snippet/{title/tape}})})
    %.  jon
    =>  dejs-soft:format
    (ot items+(ar (ot snippet+(ot title+sa ~) ~)) ~)
  ?~  res  ~&(%json-parse-failed [~ +>.$])
  ?:  =(0 (lent items.u.res))  [~ +>.$]
  =+  wer=(unwire wir)
  ?>  ?=($other -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  %-  ta-sigh-youtube:(taa uid.wer)
  title.snippet:(snag 0 items.u.res)
::
++  sigh-gh-repository
  |=  {wir/wire rep/(unit repository:gh)}
  ^-  (quip move _+>)
  ?~  rep  [~ +>]
  =+  wer=(unwire wir)
  ?>  ?=($github -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  %-  ta-sigh-gh-repository:(taa uid.wer)
  [own.wer rep.wer u.rep]
::
++  sigh-gh-commit
  |=  {wir/wire com/(unit commit:gh)}
  ^-  (quip move _+>)
  ?~  com  [~ +>]
  =+  wer=(unwire wir)
  ?>  ?=($github -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  %-  ta-sigh-gh-commit:(taa uid.wer)
  [own.wer rep.wer u.com]
::
++  sigh-gh-issue
  |=  {wir/wire iss/(unit issue:gh)}
  ^-  (quip move _+>)
  ?~  iss  [~ +>]
  =+  wer=(unwire wir)
  ?>  ?=($github -.wer)
  ?.  (~(has by known) uid.wer)
    ~&([%unknown-uid uid.wer] [~ +>.$])
  %-  pre-bake  =<  ta-done
  %-  ta-sigh-gh-issue:(taa uid.wer)
  [own.wer rep.wer u.iss]
::
++  taa
  |=  uid/serial
  =+  kon=(~(got by known) uid)
  =+  sem=(~(got by streams) p.kon)
  =/  gam/telegram
    =-  (snag - grams.sem)
    (sub (dec count.sem) q.kon)
  ~(. ta ~ ~ p.kon gam sem)
::
++  unwire
  |=  wir/wire
  ^-  weir
  ?+  wir  !!
      {$ping @ @ $~}
    [%ping (slav %uv i.t.wir) (slav %da i.t.t.wir)]
    ::
      {$github @ @ @ $~}
    [%github (slav %uv i.t.t.t.wir) i.t.wir i.t.t.wir]
    ::
      {@ @ $~}
    [%other (slav %uv i.t.wir)]
  ==
::
++  bake
  |=  det/delta
  ^-  (quip move _+>)
  da-done:(da-apply:da det)
::
++  pre-bake
  |=  des/(list delta)
  ^-  (quip move _+>)
  %+  roll  des
  |=  {d/delta m/(list move) _+>.$}
  =^  mos  +>.^$  (bake d)
  [:(welp m mos) +>.^$]
::
++  ta                                                  ::>  per transaction
  |_  $:  deltas/(list delta)
          reps/(list reply)
          circ/circle
          gram/telegram
          stam/stream
      ==
  ::
  ++  ta-done
    %-  flop  ^-  (list delta)
    [[%say circ gram (flop reps)] deltas]
  ::
  ++  ta-delta
    |=  a/delta
    %_(+> deltas [a deltas])
  ::
  ++  ta-reply
    |=  a/reply
    %_(+> reps [a reps])
  ::
  ++  ta-reply-lin
    |=  a/tape
    ^+  +>
    (ta-reply %speech %lin | (crip ":: {a}"))
  ::
  ++  ta-open
    |=  {cir/circle num/@ud gam/telegram}
    ^+  +>
    =?  +>  =(0 (mod num 1.000))
      (ta-reply %milestone num)
    =.  +>
      (ta-delta %learn cir gam)
    ta-read(circ cir, gram gam, stam (~(got by streams) cir))
  ::
  ++  ta-read
    ^+  .
    ?:  ta-ignore  .
    =+  sep=sep.gram
    |-
    ?+  -.sep  ..ta-read
      $lin  (ta-read-lin +.sep)
      $url  (ta-read-url +.sep)
      $ire  $(sep sep.sep)
      $fat  $(sep sep.sep)
      $app  ?.  =(%blockio app.sep)  ..ta-read
            $(sep sep.sep)
    ==
  ::
  ++  ta-read-lin
    |=  {pat/? msg/cord}
    ^+  +>
    =+  msg=(cass (trip msg))
    =+  cmd=(ta-command msg)
    ::
    ::  process a command
    ?^  cmd
      ?+  p.u.cmd   +>.$
        $ignoreme   %-  ta-delta:(ta-reply %ignore &)
                    [%ignore aut.gram &]
        $noticeme   %-  ta-delta:(ta-reply %ignore |)
                    [%ignore aut.gram |]
        $ping       (ta-reply %ping ~)
        $chopra     ?.  ?=($full behavior.stam)  +>.$
                    ?.  (chance 5)
                      (ta-reply %chopra ~)
                    %-  ta-reply-lin
                    "we are slaves to our code. we cannot be saved."
        $quote      ?.  ?=($full behavior.stam)  +>.$
                    (ta-reply %quote ~)
        $meme       ?.  ?=($full behavior.stam)  +>.$
                    (ta-reply %meme ~)
      ==
    ::
    ::  process a textual action.
    ?:  pat
      ?:  =(msg "jumps over the lazy dogs")
        (ta-reply %bark ~)
      +>.$
    ::
    =+  men=(ta-has msg "talkbot")
    =+  man=(ta-has msg (scag 7 (scow %p our.bol)))
    =+  qes=(ta-question msg)
    ::
    ::  we got mentioned, respond accordingly.
    =?  +>.$  |(men man)
      ?:  &(man !men)
        (ta-reply %simple %name)
      ::
      ?:  (ta-has msg "cute")
        (ta-reply %simple %cute)
      ::
      ?:  (ta-has msg "shut up")
        (ta-reply %simple %ignore)
      ::
      ?:  %+  ta-has-any  msg
          ~["thank" "good job" "gj" "good bot"]
        (ta-reply %simple %welcome)
      ::
      ?:  %+  ta-has-all  msg
          ~["Confirmed: " " to {(cite:title our.bol)}."]
        (ta-reply %simple %thank)
      ::
      ?:  %+  ta-has-any  msg
          ~["hi " "yo " "hey" "hello" "greetings" "salve"]
        (ta-reply %hey ~)
      ::
      ?:  (ta-has-any msg ~["good night" "bye" "adios"])
        (ta-reply %bye ~)
      ::
      ?:  &(qes (ta-has-any msg ~["source" " code" " repo"]))
        (ta-reply %source ~)
      ::
      =+  pos=(need (find "talkbot" msg))
      ?:  &(qes (lte pos 1))
        (ta-reply %oracle ~)
      +>.$
    ::
    ::  process a "regular" message.
    ?:  &((ta-has msg "ping") (lte (lent msg) 5))
      (ta-reply %simple %pong)
    ::
    ?:  &((ta-has msg "beep") (lte (lent msg) 5))
      (ta-reply %simple %boop)
    ::
    ?:  |(=(msg "test") =(msg "testing"))
      (ta-reply %simple %test)
    ::
    ?:  ?&  (ta-has-all msg ~["hello" "world"])
            (lte (lent msg) 13)
        ==
      (ta-reply %simple %hello)
    ::
    ?:  =(msg "talkbot and i are really close, we even finish each other's")
      (ta-reply %simple %finish)
    ::
    ?:  =(msg "+code")
      (ta-reply %uifail ~)
    ::
    ?:  ?&  qes
            ?|  (ta-has msg "what is urbit")
                (ta-has msg "what's urbit")
                (ta-has msg "what is this")
                (ta-has msg "what's this")
            ==
        ==
      (ta-reply %urbit ~)
    ::
    ?:  (ta-has msg "dy-edit-busy")
      (ta-reply %simple %dy-edit-busy)
    +>.$
  ::
  ++  ta-read-url
    |=  url/purf:eyre
    ^+  +>
    =*  hos  r.p.p.url
    =*  pax  q.q.p.url
    =*  qer    r.p.url
    =*  typ  p.q.p.url
    ?:  ?=(%| -.hos)  +>.$
    ::
    ::  make a github api request for more data.
    ?:  =(`0 (find ~['com' 'github'] p.hos))
      ::  issue or pr
      ?:  ?&  =((lent pax) 4)
              (ta-has-any pax ~[~['pull'] ~['issues']])
          ==
        %+  ta-reply  %github
        [(snag 0 pax) (snag 1 pax) issue+(snag 3 pax)]
      ::  specific commit
      ?:  (ta-has pax ~['commit'])
        %+  ta-reply  %github
        [(snag 0 pax) (snag 1 pax) commit+(snag 3 pax)]
      ::  repository
      ?:  (gte (lent pax) 2)
        %+  ta-reply  %github
        [(snag 0 pax) (snag 1 pax) repo+~]
      +>.$
    ::
    ::  make a youtube api request for more data.
    ?:  ?&  =(`0 (find ~['com' 'youtube'] p.hos))
            (ta-has ~['watch'] pax)
        ==
      %+  ta-reply  %youtube
      (~(got by (~(gas by *(map @t @t)) qer)) 'v')
    ?:  =(`0 (find ~['be' 'youtu'] p.hos))
      (ta-reply %youtube (snag 0 pax))
    ::
    ::  make a pastebin api request for more data.
    ?:  =(`0 (find ~['com' 'pastebin'] p.hos))
      %+  ta-reply  %pastebin
      ?:  =((lent pax) 1)  (snag 0 pax)
      (snag 1 pax)  ::  assume raw
    ::
    ::  make a generic webpage request.
    =+  typ=(fall typ %html)
    ?:  ?=(?($html $php) typ)
      (ta-reply %webpage |)
    +>.$
  ::
  ++  ta-ignore
    ?|  =(aut.gram our.bol)
      ::
        ?.  (~(has in ignoring) aut.gram)  |
        ?.  ?=($lin -.sep.gram)  |
        !=((trip msg.sep.gram) "!noticeme")
      ::
        (gth (sub wen.gram ~m1) now.bol)
      ::
        ?&  ?=($lin -.sep.gram)
            =((scag 2 (trip msg.sep.gram)) "::")
        ==
      ::
        =+  per=(fall (~(get by people.stam) aut.gram) *person)
        ?:  =(last.per *@da)  |
        ?:  (gth last.per wen.gram)  &
        =+  col=(div (sub wen.gram last.per) ~s5)
        &((gth heat.per 2) (gth heat.per +(col)))
    ==
  ::
  ++  ta-command
    |=  m/tape
    ^-  (unit (pair term (list tape)))
    ?.  =((snag 0 m) '!')  ~
    =.  m  (slag 1 m)
    =/  res/(list tape)
      (rust m (more ace (star ;~(less ace next))))
    `[(crip (snag 1 res)) (slag 2 res)]
  ::
  ++  ta-mentioned
    |=  m/tape
    %+  ta-has-any  m
    :~  "talkbot"
        (firstname our.bol)
    ==
  ::
  ++  ta-question
    |=  m/tape
    =((snag 0 (flop m)) '?')
  ::
  ++  ta-has
    |=  {m/(list) n/(list)}
    ?=(^ (find n m))
  ::
  ++  ta-has-any
    |=  {m/(list) n/(list (list))}
    ?~  n  |
    |((ta-has m i.n) $(n t.n))
  ::
  ++  ta-has-all
    |=  {m/(list) n/(list (list))}
    ?~  n  &
    &((ta-has m i.n) $(n t.n))
  ::
  ++  ta-coup-ping
    |=  wen/@da
    =+  tim=(div (mul 1.000 (sub now.bol wen)) ~s1)
    %-  ta-reply-lin
    %+  weld  "{(scow %ud tim)} ms "
    "(round trip from me to {(cite:title aut.gram)})"
  ::
  ++  ta-sigh-httr
    |=  {wir/wire bod/octs}
    ^+  +>
    ?+  -.wir  ~&([%strange-httr-wir wir] +>)
        $chopra
      (ta-reply-lin "{(trip q.bod)}")
      ::
        $pastebin
      %+  ta-reply  %speech
      =-  [%fat - %lin | ':: Pastebin contents:']
      [%text (to-wain:format q.bod)]
      ::
        $webpage
      =+  bod=(trip q.bod)
      =+  hed=(find "<title>" bod)
      =+  tal=(find "</title>" bod)
      ?.  &(?=(^ hed) ?=(^ tal))  +>.$
      =.  u.hed  (add u.hed 7)
      %-  ta-reply-lin
      "{(swag [u.hed (sub u.tal u.hed)] bod)}"
    ==
  ::
  ++  ta-sigh-youtube
    |=  tit/tape
    ^+  +>
    (ta-reply-lin tit)
  ::
  ++  ta-sigh-gh-repository
    |=  {own/cord rep/cord rop/repository:gh}
    ^+  +>
    %-  ta-reply-lin
    "{(trip own)}/{(trip rep)}: {(trip description.rop)}"
  ::
  ++  ta-sigh-gh-commit
    |=  {own/cord rep/cord com/commit:gh}
    ^+  +>
    %-  ta-reply-lin
    %+  weld
      "{(trip own)}/{(trip rep)}: "
    =+  mes=(trip message.com)
    =+  end=(find "\0a" mes)
    ?~  end  mes
    (scag u.end mes)
  ::
  ++  ta-sigh-gh-issue
    |=  {own/cord rep/cord iss/issue:gh}
    ^+  +>
    %-  ta-reply-lin
    ;:  weld
      "{(trip own)}/{(trip rep)}"
      "#{(scow %ud number.iss)}: "
      (trip title.iss)
    ==
  --
::
++  da
  ::>
  ::
  |_  $:  moves/(list move)
          circ/circle
          gram/telegram
          seps/(list speech)
      ==
  ::
  ++  da-done
    :_  +>
    :_  (flop moves)
    :*  ost.bol
        %poke
        /said
        [our.bol %hall]
        %hall-action
        =-  [%phrase [circ ~ ~] -]
        %+  turn  (flop seps)
        |=(s/speech [%ire uid.gram s])
    ==
  ::
  ++  da-move
    |=  a/move
    %_(+> moves [a moves])
  ::
  ++  da-say
    |=  sep/speech
    %_(+> seps [sep seps])
  ::
  ++  da-say-lin
    |=  a/tape
    (da-say %lin | (crip (weld ":: " a)))
  ::
  ++  da-say-act
    |=  a/tape
    (da-say %lin & (crip a))
  ::
  ++  da-say-url
    |=  a/tape
    (da-say %url (need (de-purl:html (crip a))) ~)
  ::
  ++  da-apply
    |=  det/delta
    ^+  +>
    ?-  -.det
      $join     (da-apply-join +.det)
      $learn    (da-apply-learn +.det)
      $ignore   (da-apply-ignore +.det)
      ::
      $sub      (da-apply-sub +.det)
      $say      (da-apply-say +.det)
    ==
  ::
  ++  da-apply-join
    |=  {cir/circle rol/role}
    ^+  +>
    =+  sem=(fall (~(get by streams) cir) *stream)
    +>.$(streams (~(put by streams) cir sem(behavior rol)))
  ::
  ++  da-apply-learn
    |=  {cir/circle gam/telegram}
    ^+  +>
    =+  sem=(fall (~(get by streams) cir) *stream)
    =+  per=(fall (~(get by people.sem) aut.gam) *person)
    =.  known  (~(put by known) uid.gam cir count.sem)
    =.  messages.per  [count.sem messages.per]
    =.  grams.sem  [gam grams.sem]
    =.  count.sem  +(count.sem)
    =.  people.sem  (~(put by people.sem) aut.gam per)
    +>.$(streams (~(put by streams) cir sem))
  ::
  ++  da-apply-ignore
    |=  {who/ship ign/?}
    ^+  +>
    ?:  ign
      +>(ignoring (~(put in ignoring) who))
    +>(ignoring (~(del in ignoring) who))
  ::
  ++  da-apply-sub
    |=  {sub/? cir/circle}
    ^+  +>
    %-  da-move
    ?.  sub
      :*  ost.bol
          %pull
          /stream/(scot %p hos.cir)/[nom.cir]
          [hos.cir %hall]
          ~
      ==
    :*  ost.bol
        %peer
        /stream/(scot %p hos.cir)/[nom.cir]
        [hos.cir %hall]
        /circle/[nom.cir]/grams/(scot %da now.bol)
    ==
  ::
  ++  da-apply-say
    |=  {cir/circle gam/telegram res/(list reply)}
    ^+  +>
    ?:  =(0 (lent res))  +>
    =.  circ  cir
    =.  gram  gam
    =+  sem=(fall (~(get by streams) cir) *stream)
    =+  per=(fall (~(get by people.sem) aut.gam) *person)
    =.  heat.per
      %+  mul  2
      %+  max  1
      %+  sub  heat.per
      %+  min  heat.per
      (div (sub wen.gam (min wen.gam last.per)) ~s5)
    =.  last.per  now.bol
    =.  people.sem  (~(put by people.sem) aut.gam per)
    =.  streams  (~(put by streams) cir sem)
    |-  ^+  +>.^$
    ?~  res  +>.^$
    =.  +>.^$  (da-reply i.res)
    $(res t.res)
  ::
  ++  da-reply
    |=  rep/reply
    ^+  +>
    ?:  ?=(move-reply rep)
      (da-do-reply rep)
    (da-speak-reply rep)
  ::
  ++  da-speak-reply
    |=  rep/speech-reply
    ^+  +>
    ?-  -.rep
      $hey      (da-say-lin "Hello {(firstname aut.gram)}!")
      $bye      (da-say-lin "Bye, {(firstname aut.gram)}!")
      $bark     (da-say-act "barks")
      $source   (da-say-url "https://github.com/Fang-/talkbot")
      ::
        $simple
      ?:  ?&  (chance 10)
              (~(has in simples) (cat 3 wat.rep '2'))
          ==
        $(wat.rep (cat 3 wat.rep '2'))
      (da-say-lin (~(got by simples) wat.rep))
      ::
        $speech
      (da-say sep.rep)
      ::
        $ignore
      %-  da-say-lin
      ?.  ign.rep
        "No longer ignoring {(cite:title aut.gram)}."
      "Now ignoring {(cite:title aut.gram)}. !noticeme to undo."
      ::
        $milestone
      (da-say-lin "That was the {(scow %ud num.rep)}th message in this circle!")
      ::
        $urbit
      %-  da-say-lin
      %-  pick-random
      ^-  (list tape)
      :~  "Urbit is a P2P network of personal servers."
          "Urbit is an OS with shared global state."
          "An urbit is a cryptographic personal identity."
          "Urbit is a self-proclaimed virtual city in the cloud."
          "Urbit is to real estate as Bitcoin is to currency."
          "Urbit is the future."
          "Urbit is definitely not a scamcoin."
          "Urbit: project, function, server, network, possibility."
          "Urbit is in active development."
          "What is Urbit not?"
          "You tell me."
          "Crazy."
      ==
      ::
        $oracle
      %-  da-say-lin
      %-  pick-random
      ^-  (list tape)
      :~  "It is certain."
          "It is decidedly so."
          "Without a doubt."
          "Yes, definitely."
          "You may rely on it."
          "As I see it, yes."
          "Most likely."
          "Outlook good."
          "Yes."
          "Signs point to yes."
          "Reply hazy try again."
          "Ask again later."
          "Better not tell you now."
          "Cannot predict now."
          "Concentrate and ask again."
          "Don't count on it."
          "My reply is no."
          "My sources say no."
          "Outlook not so good."
          "Very doubtful."
      ==
      ::
        $uifail
      %-  da-say-lin
      ?.  (chance 10)
        "There is no user failure, only UI failure."
      (scow %p (random 281.474.976.710.656 18.446.744.073.709.551.615))
      ::
        $quote
      %-  da-say-lin
      =-  :(weld q " -- " (cite:title p))
      %-  pick-random
      ^-  (list (pair @p tape))
      :~  :-  ~tomrex-sampel-sampel-sampel--sampel-sampel-sampel-magpes
          "hello this is fbi u r all under a rest"
        ::
          :-  ~rilwyn-sanpyl
          "curtis yarvin is my spirit animal"
        ::
          :-  ~pittyp-datfyn
          "(slav %da (kraut %nein now)) #justurbitthings"
        ::
          :-  ~master-morzod
          "hoon runes were originally defended as metalhead syntax"
        ::
          :-  ~milrex-mithec
          "this is my urbit, there are many like it but this one is mine"
      ==
      ::
        $meme
      %-  pick-random
      ^-  (list _+>)
      :~  %-  da-say-lin
          "Submit Urbit memes to talkbot@sssoft.io"
          ::
          %-  da-say-lin
          ;:  weld
            "Urbit must secure the existence of our "
            "shitposts and a future for dank memes."
          ==
          ::
          %-  da-say-lin
          ;:  weld
            "I'd just like to interject for a moment. "
            "What you're referring to as Arvo, is in "
            "fact, Nock/Arvo, or as I've recently taken "
            "to calling it, Nock plus Arvo."
          ==
          ::
          %-  da-say-lin
          ;:  weld
            "What the fuck did you just say about me, "
            "you little comet? I'll have you know I "
            "graduated top of my clan in the ~binzod "
            "fleet, and I've been involved in numerous "
            "secret raids on centralized software, and "
            "I have over 300 merged pull requests."
          ==
          ::
          (da-say-act "chugs a gallon of whole milk.")
          ::
          (da-say-url "http://i.imgur.com/kXeGKfp.png")
          ::
          (da-say-url "http://i.imgur.com/7gWmwVM.png")
          ::
          (da-say-url "http://i.imgur.com/juUPnDI.jpg")
          ::
          (da-say-url "http://i.imgur.com/54YK0A1.png")
          ::
          (da-say-url "http://i.imgur.com/YYUkyMQ.png")
      ==
    ==
  ::
  ++  da-do-reply
    |=  rep/move-reply
    ^+  +>
    %-  da-move
    :-  ost.bol
    =+  uid=(scot %uv uid.gram)
    ?-  -.rep
        $ping
      :*  %poke
          /ping/[uid]/(scot %da now.bol)
          [aut.gram %hood]
          [%helm-hi 'talkbot ping']
      ==
      ::
        $chopra
      :*  %hiss
          /chopra/[uid]
          ~
          %httr
          %purl
          (need (de-purl:html 'https://fang.io/chopra.php'))
      ==
      ::
        $github
      =/  url/tape
        %+  weld  "https://api.github.com/repos/"
        ?-  -.wat.rep
            $repo
          (weld (trip own.rep) '/'^(trip rep.rep))
          ::
            $issue
          ;:  weld
            (trip own.rep)
            '/'^(trip rep.rep)
            "/issues"
            '/'^(trip num.wat.rep)
          ==
          ::
            $commit
          ;:  weld
            (trip own.rep)
            '/'^(trip rep.rep)
            "/git/commits"
            '/'^(trip has.wat.rep)
          ==
        ==
      :*  %hiss
          /github/[own.rep]/[rep.rep]/[uid]
          ~
          ?-  -.wat.rep
            $repo     %gh-repository
            $issue    %gh-issue
            $commit   %gh-commit
          ==
          %purl
          (need (de-purl:html (crip url)))
      ==
      ::
        $youtube
      =/  url/tape
        ;:  weld
          "https://www.googleapis.com/youtube/v3/"
          "videos?part=snippet&key="
          "AIzaSyBTzehz6Fst7XC-YReqE5JqLwHczltS65Y&id="
          (trip wat.rep)
        ==
      :*  %hiss
          /youtube/[uid]
          ~
          %json  ::TODO  implement proper marks for yt
          %purl
          (need (de-purl:html (crip url)))
      ==
      ::
        $pastebin
      :*  %hiss
          /pastebin/[uid]
          ~
          %httr
          %purl
          =-  (need (de-purl:html (crip -)))
          (weld "https://pastebin.com/raw/" (trip wat.rep))
      ==
      ::
        $webpage
      ?>  ?=($url -.sep.gram)
      :*  %hiss
          (weld ?:(ret.rep /redirect ~) /webpage/[uid])
          ~
          %httr
          %purl
          p.url.sep.gram
      ==
    ==
  --
::
++  get-simples
  ^-  (map term tape)
  %-  ~(gas by *(map term tape))
  :~  :-  %pong     "Pong."
      :-  %pong2    "[ping-pong intensifies]"
      :-  %beep     "Boop."
      :-  %beep2    "[robot noises]"
      :-  %test     "Test successful!"
      :-  %name     "Call me ~talkbot, beep boop!"
      :-  %name2    "Yes, hello fellow human."
      :-  %welcome  "You're welcome!"
      :-  %thank    "Thank you!"
      :-  %ignore   "Want me to ignore you? Say ~ignoreme"
      :-  %finish   "sentences."
      :-  %cute     "no u ;)"
    ::
      :-  %dy-edit-busy
      "Press backspace in an empty dojo prompt to cancel %dy-edit-busy!"
  ==
::
++  wire-source
  |=  wir/wire
  ^-  circle
  ?>  ?=({$stream @ta @ta *} wir)
  [(slav %p i.t.wir) i.t.t.wir]
::
++  firstname
  |=  who/ship
  ^-  tape
  (scag 7 (scow %p who))
::
++  random
  |=  {min/@ud max/@ud}
  ^-  @ud
  =+  rng=~(. og eny.bol)
  =^  r  rng  (rads:rng max)
  (add min r)
::
++  chance
  |=  perc/@
  ^-  ?
  (lth (random 0 101) perc)
::
++  pick-random
  |*  a/(list)
  ^+  ?>(?=(^ a) i.a)
  (snag (random 0 (lent a)) a)
--
