(ns arcane-lab.sets)

(def sets-that-work
  #{:LEB (keyword "3ED") (keyword "4ED") :ICE :MIR :VIS (keyword "5ED") :POR
    :WTH :TMP :STH :PO2 :EXO :USG :ULG (keyword "6ED") :UDS :S99 :MMQ :NMS :PCY
    :INV :PLS (keyword "7ED") :APC :ODY :TOR :JUD :ONS :LGN :SCG
    (keyword "8ED") :MRD :DST (keyword "5DN") :UNH :CHK :BOK :SOK
    (keyword "9ED") :RAV :GPT :DIS :CSP (keyword "10E") :LRW :MOR :SHM :EVE :ALA
    :CON :ARB :M10 :ZEN :WWK :ROE :M11 :SOM :MBS :NPH :M12 :ISD :DKA :AVR :M13
    :RTR :GTC :DGM :M14 :THS :BNG :JOU :M15 :KTK :FRF :DTK :ORI :BFZ :OGW :SOI
    :EMN :KLD :AER :AKH :HOU :XLN :RIX :DOM :M19})

(def sets-that-dont-work
  #{:EMA :MM2 :CNS :MMA :FUT :PLC :TSP :TSB :PTK :UGL :ALL :HML :CHR :FEM :DRK
    :LEG :ATQ :ARN})

(def sealed-formats
  "A map between set code and sealed format for every set with a multi-set
  sealed format (i.e. small sets). All other sets' sealed formats are just
  6 packs of that set."
  {:RIX "4RIX2XLN" ; the last entry for the foreseeable future (RIP small sets)
   :HOU "4HOU2AKH"
   :AER "4AER2KLD"
   :EMN "4EMN2SOI"
   :OGW "4OGW2BFZ"
   :DTK "4DTK2FRF"
   :FRF "3FRF3KTK"
   :JOU "2JOU2BNG2THS"
   :BNG "3BNG3THS"
   :DGM "2DGM2GTC2RTR"
   :DKA "3DKA3ISD"
   :NPH "2NPH2MBS2SOM"
   :MBS "3MBS3SOM"
   :WWK "3WWK3ZEN"
   :ARB "2ARB2CON2ALA"
   :CON "3CON3ALA"
   :EVE "3EVE3SHM"
   :MOR "3MOR3LRW"
   :FUT "2FUT2PLC2TSP"
   :PLC "3PLC3TSP"
   :DIS "2DIS2GPT2RAV"
   :GPT "3GPT3RAV"
   :SOK "2SOK2BOK2CHK"
   :BOK "3BOK3CHK"
   (keyword "5DN") "25DN2DST2MRD"
   :DST "3DST3MRD"
   :SCG "2SCG2LGN2ONS"
   :LGN "3LGN3ONS"
   :JUD "2JUD2TOR2ODY"
   :TOR "3TOR3ODY"
   :APC "2APC2PLS2INV"
   :PLS "3PLS3INV"
   :PCY "2PCY2NMS2MMQ"
   :NMS "3NMS3MMQ"
   :UDS "2UDS2ULG2USG"
   :ULG "3ULG3USG"
   :EXO "2EXO2STH2TMP"
   :STH "3STH3TMP"
   :WTH "2WTH2VIS2MIR"
   :VIS "3VIS3MIR"})
