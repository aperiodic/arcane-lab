(ns arcane-lab.sets)

(def sets-that-work
  #{:AER :KLD :EMN :SOI :OGW :BFZ :ORI :DTK :FRF :KTK :M15 :JOU :BNG :THS :M14 :DGM
    :GTC :RTR :M13 :AVR :DKA :ISD :M12 :NPH :MBS :SOM :M11 :ROE :WWK :ZEN :M10
    :ARB :CON :ALA :EVE :SHM :MOR :LRW (keyword "10E") :CSP :DIS :GPT :RAV
    (keyword "9ED") :SOK :BOK :CHK :UNH (keyword "5DN") :DST :MRD
    (keyword "8ED") :SCG :LGN :ONS :JUD :TOR :ODY :APC (keyword "7ED") :PLS :INV
    :PCY :NMS :MMQ :S99 :UDS (keyword "6ED") :ULG :USG :EXO :PO2 :STH :TMP :WTH
    :POR (keyword "5ED") :VIS :MIR :ICE (keyword "4ED") (keyword "3ED") :LEB})

(def sets-that-dont-work
  #{:EMA :MM2 :CNS :MMA :FUT :PLC :TSP :TSB :PTK :UGL :ALL :HML :CHR :FEM :DRK
    :LEG :ATQ :ARN})

(def sealed-formats
  "A map between set code and sealed format for every set with a multi-set
  sealed format (i.e. small sets). All other sets' sealed formats are just
  6 packs of that set."
  {:AER "4AER2KLD"
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
