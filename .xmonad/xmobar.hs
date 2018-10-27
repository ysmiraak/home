Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 99
       , lowerOnStart = True
       , commands = [ Run Cpu [ "-t" , "cpu <total>%"
                              , "-L" , "3"
                              , "-H" , "50"
                              , "--normal" , "green"
                              , "--high" , "red"
                              ] 10
                    , Run Memory ["-t","mem <usedratio>%"] 10
                    , Run Date "%a %b %_d %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% %cpu% %memory% }{ %date%"
       }
