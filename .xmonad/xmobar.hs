Config
  { template = "%StdinReader% }{ cpu %cpu% mem %memory%    %date%"
  , commands =
      [ Run StdinReader
      , Run Cpu
        [ "--template" , "<total>%"
        , "--Low"      , "20"
        , "--High"     , "50"
        , "--normal"   , "green"
        , "--high"     , "red"
        ] 10
      , Run Memory
        [ "--template" , "<usedratio>%"
        , "--Low"      , "20"
        , "--High"     , "50"
        , "--normal"   , "green"
        , "--high"     , "red"
        ] 10
      , Run Date "%a %b %_d %H:%M" "date" 10
      ]
  }
