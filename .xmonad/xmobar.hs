Config
  { template = "  %StdinReader% } ----==|i|==---- { cpu %cpu%   mem %memory%   %date%  "
  , commands =
      [ Run StdinReader
      , Run Cpu
        [ "--template" , "<total>%"
        , "--Low"      , "10"
        , "--High"     , "25"
        , "--normal"   , "green"
        , "--high"     , "red"
        ] 40
      , Run Memory
        [ "--template" , "<usedratio>%"
        , "--Low"      , "20"
        , "--High"     , "50"
        , "--normal"   , "green"
        , "--high"     , "red"
        ] 40
      , Run Date "%#Z %R   %#a %F" "date" 40
      ]
  }
