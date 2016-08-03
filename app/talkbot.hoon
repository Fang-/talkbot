::  Bot for urbit talk.
::  Responds when talked to, provides titles of GitHub issues and PRs when linked.
::  To begin, start and :talkbot [%join ~ship ~.channel]

::TODO  Robustly support multiple messages from different channels at the same
::      time by putting received messages into a queue that is constantly processed.
::      (As it is now, tmpstation can be overwritten if we receive messages too swiftly.)
::TODO  Give descriptions for rnues and stdlib functions when asked.
::TODO  Use ;: and %+ etc for cleaner-looking code.

/-  talk
/+  talk
!:

|%
++  move  {bone card}
++  card
  $%  {$peer wire {@p term} path}
      {$pull wire {@p term} $~}
      {$poke wire {@p term} *}
      {$hiss wire $~ $httr {$purl p/purl}}
  ==
++  address  {(pair @p @ta)}
++  action
  $%  {$join a/address}
      {$leave a/address}
      {$leaveall $~}
      {$joined $~}
      {$ignoring $~}
  ==
++  update
  $%  {$tmpstation s/station:talk}
      {$ignore p/@p}
      {$unignore p/@p}
  ==
--

|_  {bowl joined/(list address) ignoring/(list @p) tmpstation/station:talk}

++  poke-noun
  ::TODO  Should probably check if %peers and %pulls succeed (using reap) before
  ::      adding/removing stations to/from the joined list.
  |=  act/action
  ^-  {(list move) _+>.$}
  ?-  act
    {$join *}
      ?~  (find [p.a.act q.a.act]~ joined)
        ~&  [%joining p.a.act q.a.act]
        :-  [[ost %peer /talkbot/listen/(scot %p p.a.act)/[q.a.act] [p.a.act %talk] /afx/[q.a.act]/(scot %da now)] ~]
        +>.$(joined [[p.a.act q.a.act] joined])
      ~&  [%already-joined p.a.act q.a.act]
      [~ +>.$]
    {$leave *}
      =+  i=(find [p.a.act q.a.act]~ joined)
      ?~  i
        ~&  [%already-left p.a.act q.a.act]
        [~ +>.$]
      ~&  [%leaving p.a.act q.a.act]
      :-  [[ost %pull /talkbot/listen/(scot %p p.a.act)/[q.a.act] [p.a.act %talk] ~] ~]
      +>.$(joined (weld (scag u.i joined) (slag +(u.i) joined)))
    {$leaveall $~}
      ~&  [%leaving-all]
      :_  +>.$(joined ~)
      %+  turn  joined
        |=  a/address
        [ost %pull /talkbot/listen/(scot %p p.a)/[q.a] [p.a %talk] ~]
    {$joined $~}
      ~&  [%currently-joined joined]
      [~ +>.$]
    {$ignoring $~}
      ~&  [%ignoring ignoring]
      [~ +>.$]
  ==

++  diff-talk-report
  |=  {wir/wire rep/report:talk}
  ^-  {(list move) _+>.$}
  ?:  ?=({$grams *} rep)  ::  Message list.
    =+  i=(lent q.rep)
    =|  moves/(list move)
    |-  ^-  {(list move) _+>.^$}
    ?:  (gth i 0)
      =.  i  (sub i 1)
      =+  gram=(snag i q.rep)
      =+  res=(read-telegram gram)  ::  (pair (unit move) (unit update))
      =.  moves  ?~(p.res moves [u.p.res moves])
      =+  upd=(fall q.res ~)
      =+  ^=  updres  ^-  (pair (unit move) (unit {s/station:talk i/(list @p)}))
        ?-  upd
          {$tmpstation *}
            [~ [~ [s=s.upd i=ignoring]]]
          {$ignore *}
            ?^  (find [p.upd]~ ignoring)  [~ ~]
            :_  [~ [s=tmpstation i=[p.upd ignoring]]]
            [~ (send (get-audience-station-naive q.q.gram) :(weld "Now ignoring " (ship-shortname p.upd) ", use ~unignoreme to undo."))]
          {$unignore *}
            =+  i=(find [p.upd]~ ignoring)
            ?~  i  [~ ~]
            =+  nign=(weld (scag u.i ignoring) (slag +(u.i) ignoring))
            :_  [~ [s=tmpstation i=nign]]
            [~ (send (get-audience-station-naive q.q.gram) (weld "No longer ignoring " (ship-shortname p.upd)))]
          {$~}  [~ ~]
        ==
      =.  moves  ?~(p.updres moves [u.p.updres moves])  ::  If we got a move, add it.
      ?:  =(i 0)
        ?~  q.updres
          [moves +>.^$]
        [moves +>.^$(tmpstation s.u.q.updres, ignoring i.u.q.updres)]
      ?~  q.updres
        $
      $(tmpstation s.u.q.updres, ignoring i.u.q.updres)
    [moves +>.^$]

  ?:  ?=({$group *} rep)  ::  Users in channel.
    ::TODO  Store channel member count.
    ~&  [%got-group p.rep]
    [~ +>.$]
  
  ?:  ?=({$cabal *} rep)  ::  Channel info.
    ~&  [%got-cabal rep]
    [~ +>.$]

  ~&  [%report rep]
  [~ +>.$]

++  read-telegram
  |=  gram/telegram:talk
  ^-  (pair (unit move) (unit update))
  =*  msg  r.r.q.gram
  =+  aud=(get-audience-station-naive q.q.gram)
  ?:  =(p.gram our)  ::  Ignore ourselves.
    [~ ~]
  ?^  (find [p.gram]~ ignoring)  ::  If we're ignoring a user, only acknowledge ~unignorme/~noticeme.
    ?:  &(?=({$lin *} msg) |(=((find "~unignoreme" (trip q.msg)) [~ 0]) =((find "~noticeme" (trip q.msg)) [~ 0])))
      [~ [~ [%unignore p.gram]]]
    [~ ~]
  ?:  ?=({$lin *} msg)  ::  Regular message.
    =+  tmsg=(trip q.msg)
    ::  React when we are talked about.
    ?^  (find "talkbot" tmsg)
      ::  If someone greets us, greet them back by name.
      =+  ^=  greeted
        ::TODO  Matches on things like "the talkbot they built"
        ?^  (find "hi " tmsg)  &  :: We don't want it to match on "something".
        ?^  (find "hey" tmsg)  &
        ?^  (find "hello" tmsg)  &
        ?^  (find "greetings" tmsg)  &
        |
      ?:  greeted
        [[~ (send aud :(weld "Hello " (ship-firstname p.gram) "!"))] ~]
      ::  If we're thanked, respond.
      ?^  (find "thank" tmsg)
        [[~ (send aud "You're welcome!")] ~]
      ::  If we're told to shut up, tell them about ~ignoreme.
      [~ ~]
    ::  If our ship name is mentioned, inform that we are a bot.
    ?^  (find (swag [0 7] (scow %p our)) tmsg)
      [[~ (send aud "Call me ~talkbot, beep boop!")] ~]
    ?:  =((find "~ignoreme" tmsg) [~ 0])
      [~ [~ [%ignore p.gram]]]
    ?:  =((find "~chopra" tmsg) [~ 0])
      [[~ [ost %hiss /chopra ~ %httr %purl (need (epur 'https://fang.io/chopra.php'))]] [~ [%tmpstation aud]]]
    [~ ~]

  ?:  ?=({$url *} msg)  ::  Parsed URL.
    =+  turl=(earf p.msg)
    =+  slashes=(fand "/" turl)
    ?:  =((find "https://github.com/" turl) [~ 0])
      =+  owner=(swag [19 (sub (snag 3 slashes) 19)] turl)
      =+  repo=(swag [(add (snag 3 slashes) 1) (sub ?:((gth (lent slashes) 4) (snag 4 slashes) (lent turl)) (add (snag 3 slashes) 1))] turl)
      =|  api/cord
      =+  apibase="https://api.github.com/repos/"
      =+  ^=  api  ^-  cord
        ?^  (find "/issues/" turl)
          (crip (weld apibase (swag [19 (lent turl)] turl)))
        ?^  (find "/pull/" turl)
          (crip :(weld apibase (swag [19 (sub (snag 4 slashes) 19)] turl) "/issues" (swag [(snag 5 slashes) 6] turl)))
        (crip (weld apibase (swag [19 (sub (lent turl) 19)] turl)))  ::  Just make a generic api call.
      =+  url=(epur api)
      ?~  url
        ~&  [%failed-epur-for api]
        [~ ~]
      [[~ [ost %hiss /gh/(crip owner)/(crip repo) ~ %httr %purl u.url]] [~ [%tmpstation aud]]]
    ?:  =((find "http://pastebin.com/" turl) [~ 0])
      :: Pastebin doesn't provide API access to paste data (ie title), so just get the page.
      =+  url=(epur (crip turl))
      ?~  url
        ~&  [%failed-epur-for turl]
        [~ ~]
      [[~ [ost %hiss /pb ~ %httr %purl u.url]] [~ [%tmpstation aud]]]
    [~ ~]
  [~ ~]

++  sigh-httr
  |=  {wir/wire code/@ud headers/mess body/(unit octs)}
  ^-  {(list move) _+>.$}
  ?:  &((gte code 200) (lth code 300))
    ?~  body
      [~ +>.$]
    ?:  ?=({@tas *} wir)
      ?:  =(i.wir %gh)  ::  GitHub
        =+  json=(poja q.u.body)
        ?~  json
          [~ +>.$]
        ?:  ?=({$o *} u.json)
          =+  ^=  info  ^-  tape
            =+  desc=(fall (~(get by p.u.json) 'description') ~)
            =+  title=(fall (~(get by p.u.json) 'title') ~)
            ?:  ?=({$s *} desc)  (trip p.desc)
            ?:  ?=({$s *} title)  (trip p.title)
            ~
          ?~  info  ::  If we can't say anything informative, just don't speak.
            [~ +>.$]
          =+  ^=  repo  ^-  tape
            =+  fullname=(fall (~(get by p.u.json) 'full_name') ~)
            =+  repourl=(fall (~(get by p.u.json) 'repository_url') ~)
            ?:  ?=({$s *} fullname)  (trip p.fullname)
            ?:  ?=({$s *} repourl)  (swag [29 20] (trip p.repourl))
            "GitHub"
          [[(send tmpstation :(weld repo ": " info)) ~] +>.$]
        ~&  [%no-title]
        [~ +>.$]
      ?:  =(i.wir %pb)
        =+  tbody=(trip q.u.body)
        =+  openi=(fall (find "<h1>" tbody) ~)
        =+  closei=(fall (find "</h1>" tbody) ~)
        ?:  !|(=(openi ~) =(closei ~))
          =.  openi  (add openi 4)
          =+  title=(swag [openi (sub closei openi)] tbody)
          ?:  =(title "Untitled")
            [~ +>.$]
          [[(send tmpstation title) ~] +>.$]
        [~ +>.$]
      ?:  =(i.wir %chopra)
        =+  tbody=(trip q.u.body)
        [[(send tmpstation tbody) ~] +>.$]
      ~&  [%unknown-service]
      [~ +>.$]
    ~&  [%invalid-wire]
    [~ +>.$]
  ~&  [%we-have-a-problem code]
  ~&  [%headers headers]
  ~&  [%body body]
  [~ +>.$]

++  get-audience-station-naive
  |=  aud/audience:talk
  ^-  station:talk
  ?.  ?=({^ $~ $~} aud)                 ::  test if aud is a singleton map
    ~|  %not-singleton-map  !!          ::  fail when it's not
  ?-  p.n.aud                           ::  we know that p.n.aud exists thanks to the ?=
    {$& station:talk}  p.p.n.aud        ::  produce the value
    {$| *}        ~|  %not-station  !!  ::  fail
  ==

++  send
  |=  {cuz/station:talk ?(mess/tape mess/@t)}
  ^-  move
  =+  mes=?@(mess (trip mess) mess)
  [ost %poke /repeat/(scot %ud 1)/(scot %p p.cuz)/[q.cuz] [our %talk] (said our cuz %talk now eny [%leaf (weld ":: " mes)]~)]

++  said  ::  Modified from lib/talk.hoon.
  |=  {our/@p cuz/station:talk dap/term now/@da eny/@uvI mes/(list tank)}
  :-  %talk-command
  ^-  command:talk
  :-  %publish
  |-  ^-  (list thought:talk)
  ?~  mes  ~
  :_  $(mes t.mes, eny (sham eny mes))
  ^-  thought:talk
  :+  (shaf %thot eny)
    [[[%& cuz] [*envelope:talk %pending]] ~ ~]
  [now *bouquet:talk [%lin & (crip ~(ram re i.mes))]]

++  reap
  |=  {wir/wire error/(unit tang)}
  ^-  {(list move) _+>.$}
  ?~  error
    [~ +>.$]
  ~&  [%subscription-failed error]
  [~ +>.$]

++  ship-firstname
  |=  ship/@p
  ^-  tape
  =+  name=(scow %p ship)
  =+  part=?:(=((clan ship) %earl) [15 6] [1 6])
  (weld "~" (swag part name))

++  ship-shortname
  |=  ship/@p
  ^-  tape
  =+  kind=(clan ship)
  =+  name=(scow %p ship)
  ?:  =(%earl kind)
    :(weld "~" (swag [15 6] name) "^" (swag [22 6] name))
  ?:  =(%pawn kind)
    :(weld (swag [0 7] name) "_" (swag [51 6] name))
  name

--
