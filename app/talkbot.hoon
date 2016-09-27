::  Bot for urbit talk.
::  Responds when talked to, provides titles of GitHub issues and PRs when linked.
::  To begin, start and :talkbot [%join ~ship ~.channel]

/-  talk
/+  talk, gh-parse
!:

|%
++  move  {bone card}
++  card
  $%  {$peer wire {@p term} path}
      {$pull wire {@p term} $~}
      {$poke wire {@p term} *}
      {$hiss wire $~ $httr {$purl p/purl}}
  ==
++  action
  $%  {$join s/station:talk}
      {$leave s/station:talk}
      {$joinfaves $~}
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

|_  {bowl joined/(map station:talk atlas:talk) ignoring/(list @p) tmpstation/station:talk}

++  poke-noun
  ::TODO  Should probably check if %peers and %pulls succeed (using reap) before
  ::      adding/removing stations to/from the joined list.
  |=  act/action
  ^-  {(list move) _+>.$}
  ?-  act
  {$join *}
    ?:  (~(has by joined) s.act)
      ~&  [%already-joined s.act]
      [~ +>.$]
    ~&  [%joining s.act]
    :-  [[ost %peer /(scot %p p.s.act)/[q.s.act] [p.s.act %talk] /afx/[q.s.act]/(scot %da now)] ~]
    +>.$
  {$leave *}
    ?.  (~(has by joined) s.act)
      ~&  [%already-left s.act]
      [~ +>.$]
    ~&  [%leaving s.act]
    :-  [[ost %pull /(scot %p p.s.act)/[q.s.act] [p.s.act %talk] ~] ~]
    +>.$(joined (~(del by joined) s.act))
  {$joinfaves $~}
    =+  ^=  favs  ^-  (list station:talk)  :~
      [~palfun-foslup ~.sandbox]
      [~binzod ~.urbit-meta]
    ==
    :_  +>.$
    %+  murn  favs
      |=  f/station:talk
      ^-  (unit move)
      ::  Just poke our app for the %join call.
      [~ [ost %poke /poking [our dap] %noun [%join f]]]
  {$leaveall $~}
    ~&  [%leaving-all]
    :_  +>.$(joined ~)
    %+  turn  (~(tap by joined))
      |=  j/(pair station:talk *)
      [ost %pull /(scot %p p.p.j)/[q.p.j] [p.p.j %talk] ~]
  {$joined $~}
    ~&  :-  %currently-joined
      %+  turn  (~(tap by joined))
        |=  a/(pair station:talk *)
        p.a
    [~ +>.$]
  {$ignoring $~}
    ~&  [%ignoring ignoring]
    [~ +>.$]
  ==

++  diff-talk-report
  |=  {wir/wire rep/report:talk}
  ^-  {(list move) _+>.$}
  ?+  rep
    ~&  [%report rep]
    [~ +>.$]
  {$grams *}  ::  Message list.
    =+  i=(lent q.rep)
    =|  moves/(list move)
    |-  ^-  {(list move) _+>.^$}
    ?.  (gth i 0)
      [moves +>.^$]
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

  {$group *}  ::  Users in channel.
    ::  Since $group reports don't contain the station it came from, we have to
    ::  deduce it from the wire.
    =+  stat=(fall (station-from-wire wir) ~)
    ::  If we can't parse the ship from the wire, jump out.
    ?~  stat
      ~&  [%unparsable-wire-address wir]
      [~ +>.$]
    ::  Verify we know the station we deduced from the wire.
    ?.  (~(has by joined) stat)
      ~&  [%unknown-wire-address wir]
      ~&  [%leaving stat]
      [[[ost %pull wir [p.stat %talk] ~] ~] +>.$]
    =+  oldmems=(fall (~(get by joined) stat) ~)
    :_  +>.$(joined (~(put by joined) stat p.rep))
    ::  To avoid greet-bombing, only continue when a single new ship joined.
    ?.  =((dec ~(wyt by p.rep)) ~(wyt by oldmems))
      ~
    =+  diff=(~(dif in p.rep) oldmems)
    ?.  =(~(wyt by diff) 1)
      ~&  [%weird-mem-diff-num ~(wyt by diff)]
      ~
    ::  This feels kind of naive, but it works.
    =+  newmem=(head (~(tap by diff)))
    ?:  =(p.newmem our)
      ~
    ::  Finally, greet the newly joined ship.
    [(send stat :(weld "Welcome, " (ship-firstname p.newmem) "!")) ~]

  {$cabal *}  ::  Channel info.
    ~&  [%got-cabal rep]
    [~ +>.$]

  ==

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
  ?+  msg
    [~ ~]
  {$lin *}  ::  Regular message.
    =+  tmsg=(trip q.msg)
    ::  React when we are talked about.
    ?^  (find "talkbot" tmsg)
      =+  ^=  source
        ?^  (find "code" tmsg)  &
        ?^  (find "repo" tmsg)  &
        ?^  (find "source" tmsg)  &
        |
      ?^  (find "?" tmsg)  ::  where, there
        ?:  source
          [[~ (send aud "https://github.com/Fang-/talkbot")] ~]
        [~ ~]
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
      ?^  (find "shut up" tmsg)
        [[~ (send aud "Want me to ignore you? Send `~ignoreme`.")] ~]
      [~ ~]
    ::  If our ship name is mentioned, inform that we are a bot.
    ?^  (find (swag [0 7] (scow %p our)) tmsg)
      [[~ (send aud "Call me ~talkbot, beep boop!")] ~]
    ?:  =("ping" tmsg)
      [[~ (send aud "Pong.")] ~]
    ?:  =((find "~whocount" tmsg) [~ 0])
      =+  memlist=(fall (~(get by joined) aud) ~)
      =+  statnom=:(weld (ship-shortname p.aud) "/" (scow %tas q.aud))
      ?:  (gth ~(wyt by memlist) 0)
        [[~ (send aud :(weld "There are currently " (scow %u ~(wyt by memlist)) " ships in " statnom "."))] ~]
      [[~ (send aud :(weld "I don't have member data for " statnom " yet, sorry!"))] ~]
    ?:  =((find "~ignoreme" tmsg) [~ 0])
      [~ [~ [%ignore p.gram]]]
    ?:  =((find "~chopra" tmsg) [~ 0])
      [[~ [ost %hiss /chopra ~ %httr %purl (need (epur 'https://fang.io/chopra.php'))]] [~ [%tmpstation aud]]]
    [~ ~]

  {$url *}  ::  Parsed URL.
    =+  turl=(earf p.msg)
    =+  slashes=(fand "/" turl)
    ?:  =((find "https://github.com/" turl) [~ 0])
      =+  apibase="https://api.github.com/repos/"
      ::  We want to know what we're requesting (issue, repo, etc.) so we can put it in the wire.
      ::TODO  Ideally you want to set this below, when you also define the api url.
      =+  ^=  kind
        ?:  (gth (lent slashes) 4)
          %issue
        %repo
      =+  ^=  api  ^-  cord
        ?^  (find "/issues/" turl)
          (crip (weld apibase (swag [19 (lent turl)] turl)))
        ?^  (find "/pull/" turl)
          (crip :(weld apibase (swag [19 (sub (snag 4 slashes) 19)] turl) "/issues" (swag [(snag 5 slashes) 6] turl)))
        ::  Just make a generic api call for the repo if we don't know what else to do.
        =+  ^=  endi
          ?:  (gth (lent slashes) 4)
            (snag 5 slashes)
          (lent turl)
        (crip (weld apibase (swag [19 (sub endi 19)] turl)))
      =+  url=(epur api)
      ?~  url
        ~&  [%failed-epur-for api]
        [~ ~]
      [[~ [ost %hiss /gh/[kind] ~ %httr %purl u.url]] [~ [%tmpstation aud]]]
    ?:  =((find "http://pastebin.com/" turl) [~ 0])
      :: Pastebin doesn't provide API access to paste data (ie title), so just get the page.
      =+  url=(epur (crip turl))
      ?~  url
        ~&  [%failed-epur-for turl]
        [~ ~]
      [[~ [ost %hiss /pb ~ %httr %purl u.url]] [~ [%tmpstation aud]]]
    [~ ~]
  ==

++  sigh-httr
  |=  {wir/wire code/@ud headers/mess body/(unit octs)}
  ^-  {(list move) _+>.$}
  ?.  &((gte code 200) (lth code 300))
    ~&  [%we-have-a-problem code]
    ~&  [%headers headers]
    ~&  [%body body]
    [~ +>.$]
  ?~  body
    [~ +>.$]
  ?.  ?=({@tas *} wir)
    ~&  [%invalid-wire]
    [~ +>.$]
  ?:  =(i.wir %gh)  ::  GitHub
    =+  jon=(fall (poja q.u.body) ~)
    ?~  t.wir
      ~&  [%gh-no-wire wir]
      [~ +>.$]
    ?:  =(i.t.wir %issue)
      =+  iss=(fall (issue:gh-parse jon) ~)
      ?~  iss
        ~&  [%gh-issue-parse-failed jon]
        [~ +>.$]
      =+  slashes=(fand "/" (trip url.iss))
      =+  si=(add (snag 3 slashes) 1)
      =+  repo=(swag [si (sub (snag 5 slashes) si)] (trip url.iss))
      [[(send tmpstation :(weld repo ": " (trip title.iss))) ~] +>.$]
    ?:  =(i.t.wir %repo)
      =+  rep=(fall (repository:gh-parse jon) ~)
      ?~  rep
        ~&  [%gh-repo-parse-failed jon]
        [~ +>.$]
      [[(send tmpstation :(weld (trip full-name.rep) ": " (trip description.rep))) ~] +>.$]
    ~&  [%gh-unknown-wire wir]
    [~ +>.$]
      [~ +>.$]
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
  ?:  =(i.wir %pb)
    =+  tbody=(trip q.u.body)
    =+  openi=(fall (find "<h1>" tbody) ~)
    =+  closei=(fall (find "</h1>" tbody) ~)
    ?.  !|(=(openi ~) =(closei ~))
      [~ +>.$]
    =.  openi  (add openi 4)
    =+  title=(swag [openi (sub closei openi)] tbody)
    ?:  =(title "Untitled")
      [~ +>.$]
    [[(send tmpstation title) ~] +>.$]
  ?:  =(i.wir %chopra)
    =+  tbody=(trip q.u.body)
    [[(send tmpstation tbody) ~] +>.$]
  ~&  [%unknown-service wir]
  [~ +>.$]

++  get-audience-station-naive
  |=  aud/audience:talk
  ^-  station:talk
  ?.  ?=({^ $~ $~} aud)                 ::  test if aud is a singleton map
    ::TODO  This is a shitty tmp fix. Do better "complex audience" handling here.
    ?.  %-  ~(any in aud)
            |=  a/(pair partner:talk *)
            ?.  ?=({$& station:talk} p.a)  |
            ?.  =(q.p.p.a ~.urbit-meta)  |
            ?.  |(=(p.p.p.a ~binzod) =(p.p.p.a ~marzod) =(p.p.p.a ~samzod) =(p.p.p.a ~wanzod))  |
            &
      ~|  %complex-audience  !!          ::  fail when it's not
    [~binzod ~.urbit-meta]  ::  Just naively post to ~binzod, it doesn't really matter.
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
  |=  {our/@p cuz/station:talk dap/term now/@da eny/@uvJ mes/(list tank)}
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
  ?^  error
    ~&  [%subscription-failed error]
    [~ +>.$]
  =+  stat=(fall (station-from-wire wir) ~)
  ?~  stat
    ~&  [%unexpected-reap wir]
    [~ +>.$]
  ~&  [%joined stat]
  [~ +>.$(joined (~(put by joined) stat *atlas:talk))]

++  station-from-wire
  |=  wir/wire
  ^-  (unit station:talk)
  ?.  ?=({@tas @tas *} wir)
    ~
  =+  ship=(fall `(unit @p)`(slaw %p i.wir) ~)
  ?~  ship
    ~&  [%unparsable-wire-station wir]
    ~
  =+  channel=(crip (slag 1 (spud t.wir)))
  [~ [ship channel]]

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
