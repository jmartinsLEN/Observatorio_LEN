#Per?odo I - de 1 de Janeiro a 31 de Marco
#Per?odo II - de 1 de Abril a 30 de Junho
#Per?odo III - de 1 de Julho a 30 de Setembro
#Per?odo IV - de 1 de Outubro a 31 de Dezembro

########################################################
#******************************************************#
#*****************Tarifas e Impostos*******************#
#******************************************************#
########################################################

###Tarifas ERSE (Acesso as redes) - BTE###

Ano <- 2013:2018
ponta_p1e4_BTE <- c(0.0354, 0.0489, 0.0619, 0.0663, 0.0685, 0.0721)
ponta_p2e3_BTE <- c(0.0354, 0.0489, 0.0619, 0.0663, 0.0685, 0.0713)

cheia_p1e4_BTE <- c(0.0311, 0.0424, 0.0534, 0.0573, 0.0592, 0.0612)
cheia_p2e3_BTE <- c(0.0311, 0.0424, 0.0534, 0.0573, 0.0592, 0.0608)

vnormal_p1e4_BTE <- c(0.0187, 0.0221, 0.0279, 0.0298, 0.0307, 0.0305)
vnormal_p2e3_BTE <- c(0.0187, 0.0221, 0.0279, 0.0298, 0.0307, 0.0300)

superv_p1e4_BTE <- c(0.0169, 0.0199, 0.0253, 0.0273, 0.0282, 0.0282)
superv_p2e3_BTE <- c(0.0169, 0.0199, 0.0253, 0.0273, 0.0282, 0.0283)

tar_ativa_BTE <- data.frame(P1e4=ponta_p1e4_BTE, C1e4=cheia_p1e4_BTE, 
                            VN1e4=vnormal_p1e4_BTE, SV1e4=superv_p1e4_BTE, 
                            P2e3=ponta_p2e3_BTE, C2e3=cheia_p2e3_BTE, 
                            VN2e3=vnormal_p2e3_BTE, SV2e3=superv_p2e3_BTE)
rownames(tar_ativa_BTE) <- Ano

###Tarifas ERSE (Acesso as redes) - MT###

ponta_p1e4_MT <- c(0.0268, 0.0338, 0.0432, 0.0473, 0.0463, 0.0490)
ponta_p2e3_MT <- c(0.0266, 0.0335, 0.0429, 0.0470, 0.0460, 0.0487)

cheia_p1e4_MT <- c(0.0236, 0.0290, 0.0374, 0.0409, 0.0401, 0.0413)
cheia_p2e3_MT <- c(0.0237, 0.0291, 0.0371, 0.0406, 0.0398, 0.0410)

vnormal_p1e4_MT <- c(0.0150, 0.0160, 0.0206, 0.0225, 0.0221, 0.0221)
vnormal_p2e3_MT <- c(0.0151, 0.0162, 0.0205, 0.0224, 0.0220, 0.0220)

superv_p1e4_MT <- c(0.0144, 0.0153, 0.0198, 0.0217, 0.0212, 0.0214)
superv_p2e3_MT <- c(0.0148, 0.0156, 0.0200, 0.0219, 0.0214, 0.0215)
tar_ativa_MT <- data.frame(P1e4=ponta_p1e4_MT, C1e4=cheia_p1e4_MT, 
                           VN1e4=vnormal_p1e4_MT, SV1e4=superv_p1e4_MT, 
                           P2e3=ponta_p2e3_MT, C2e3=cheia_p2e3_MT, 
                           VN2e3=vnormal_p2e3_MT, SV2e3=superv_p2e3_MT)
rownames(tar_ativa_MT) <- Ano

###Tarifas ERSE (Acesso as redes) - AT###

ponta_p1e4_AT <- c(0.0226, 0.0261, 0.306, 0.0330, 0.0329, 0.0339)
ponta_p2e3_AT <- c(0.0266, 0.0260, 0.0304, 0.0328, 0.0327, 0.0338)

cheia_p1e4_AT <- c(0.0202, 0.0226, 0.0265, 0.0285, 0.0285, 0.0279)
cheia_p2e3_AT <- c(0.0202, 0.0226, 0.0265, 0.0285, 0.0285, 0.0279)

vnormal_p1e4_AT <- c(0.0148, 0.0148, 0.0175, 0.0188, 0.0188, 0.0177)
vnormal_p2e3_AT <- c(0.0149, 0.0149, 0.0175, 0.0188, 0.0188, 0.0177)

superv_p1e4_AT <- c(0.0146, 0.0145, 0.0170, 0.0184, 0.0183, 0.0175)
superv_p2e3_AT <- c(0.0148, 0.0147, 0.0172, 0.0186, 0.0185, 0.0175)
tar_ativa_AT <- data.frame(P1e4=ponta_p1e4_AT, C1e4=cheia_p1e4_AT, 
                           VN1e4=vnormal_p1e4_AT, SV1e4=superv_p1e4_AT, 
                           P2e3=ponta_p2e3_AT, C2e3=cheia_p2e3_AT, 
                           VN2e3=vnormal_p2e3_AT, SV2e3=superv_p2e3_AT)
rownames(tar_ativa_AT) <- Ano

###Tarifas comercializador (BTE, MT ou BTN)###
tarifComerc <- c(0.0490, 0.0413, 0.0221, 0.0214)
names(tarifComerc) <- c("P", "C", "VN", "SV")

###Imposto sobre consumo###
impostoConsumo <- 0.0010

###Contribuicao Audio-Visuais###
value <-c(2.65, 2.65, 2.65, 2.85, 2.85, 2.85)
CAV <- data.frame(CAV = value)
rownames(CAV) <- Ano

###Tarifa Acesso a rede BTE- Reativa (2013-2018)###
Indutiva_BTE <- c(0.0268, 0.0293, 0.0313, 0.0331, 0.0346, 0.0331)
Capacitiva_BTE <- c(0.0204, 0.0223, 0.0239, 0.0252, 0.0264 ,0.0252)
tar_Reativa_BTE <- data.frame(Indutiva= Indutiva_BTE,
                              Capacitiva= Capacitiva_BTE)
rownames(tar_Reativa_BTE) <- Ano

###Tarifa Acesso a rede MT- Reativa (2013-2018)###
Indutiva_MT <- c(0.0234, 0.0246, 0.0263, 0.0277, 0.0290, 0.0278)
Capacitiva_MT <- c(0.0176, 0.0185, 0.0197, 0.0208, 0.0218, 0.0209)
tar_Reativa_MT <- data.frame(Indutiva= Indutiva_MT,
                             Capacitiva= Capacitiva_MT)
rownames(tar_Reativa_MT) <- Ano

###Tarifa Acesso a rede AT- Reativa (2013-2018)###
Indutiva_AT <- c(0.0215, 0.0226, 0.0241, 0.0255, 0.0267, 0.0255)
Capacitiva_AT <- c(0.0161, 0.0169, 0.0181, 0.0191, 0.0200, 0.0191)
tar_Reativa_AT <- data.frame(Indutiva= Indutiva_AT,
                             Capacitiva= Capacitiva_AT)
rownames(tar_Reativa_AT) <- Ano

###Preco Potencia contratada e hora de ponta BTE EUR/kW.mes (2013-2018)###
pContratada_BTE <- c(1.497, 1.118, 1.088, 1.172, 1.313, 1.312)
PHP_BTE <- c(19.789, 19.874, 17.289, 17.728, 18.593, 15.353)
custo_Potencia_BTE <- data.frame(Pot_Con = pContratada_BTE, PHP = PHP_BTE)
rownames(custo_Potencia_BTE) <- Ano

###Potencia contratada e hora de ponta MT EUR/kW.mes (2013-2018)###
pContratada_MT <- c(1.427, 1.058, 0.950, 0.977, 1.209, 1.063)
PHP_MT <- c(8.752, 8.958, 7.109, 6.902, 8.033, 6.531)
custo_Potencia_MT <- data.frame(Pot_Con = pContratada_MT, PHP = PHP_MT)
rownames(custo_Potencia_MT) <- Ano

###Potencia contratada e hora de ponta AT EUR/kW.mes (2013-2018)###
pContratada_AT <- c(0.843, 0.489, 0.477, 0.509, 0.691, 0.680)
PHP_AT <- c(4.536, 4.835, 3.694, 3.532, 4.283, 3.706)
custo_Potencia_AT <- data.frame(Pot_Con = pContratada_AT, PHP = PHP_AT)
rownames(custo_Potencia_AT) <- Ano


########################################################
#******************************************************#
#*Periodos horarios Inverno ciclo semanal(cs) - todos**#
#******************************************************#
########################################################

###Periodos horarios Inverno cs - todos###
csSegSexInv <- c("09:00:00-12:00:00/18:30:00-21:00:00",
                 "07:00:00-09:30:00/12:00:00-18:30:00/21:00:00-24:00:00",
                 "00:00:00-02:00:00/06:00:00-07:00:00", "02:00:00-06:00:00")
names(csSegSexInv) <- c("P", "C", "VN", "SV")
csSabInv <- c("09:30:00-13:00:00/18:30:00-22:00:00",
              "00:00:00-02:00:00/06:00:00-09:30:00/13:00:00-18:30:00/22:00:00-24:00:00",
              "02:00:00-06:00:00")
names(csSabInv) <- c("C","VN","SV")

csDomInv <- c("00:00:00-02:00:00/06:00:00-24:00:00",
              "02:00:00-06:00:00")
names(csDomInv) <- c("VN","SV")

###Periodos horarios Verao cs - todos###
csSegSexVer <- c("09:15:00-12:15:00",
                 "07:00:00-09:15:00/12:15:00-24:00:00",
                 "00:00:00-02:00:00/06:00:00-07:00:00", "02:00:00-06:00:00")
names(csSegSexVer) <- c("P", "C", "VN", "SV")

csSabVer <- c("09:00:00-14:00:00/20:00:00-22:00:00",
              "00:00:00-02:00:00/06:00:00-09:00:00/14:00:00-20:00:00/22:00:00-24:00:00",
              "02:00:00-06:00:00")
names(csSabVer) <- c("C","VN","SV")

csDomVer <- c("00:00:00-02:00:00/06:00:00-24:00:00",
              "02:00:00-06:00:00")
names(csDomVer) <- c("VN","SV")
########################################################################
#**********************************************************************#
#Ciclo semanal opcional(op) para MAT, AT e MT em Portugal Continental**#
#**********************************************************************#
########################################################################

###Periodos horarios Inverno op - MAT, AT e MT###
opSegSexInv <- c("17:00:00-22:00:00",
                 "00:00:00-00:30:00/07:30:00-17:00:00/22:00:00-24:00:00",
                 "00:30:00-02:00:00/06:00:00-07:30:00", 
                 "02:00:00-06:00:00")

names(opSegSexInv) <- c("P", "C", "VN", "SV")

opSabInv <- c("10:30:00-12:30:00/17:30:00-22:30:00",
              "00:00:00-03:00:00/07:00:00-10:30:00/12:30:00-17:30:00/22:30:00-24:00:00",
              "03:00:00-07:00:00")
names(opSabInv) <- c("C","VN","SV")

opDomInv <- c("00:00:00-04:00:00/08:00:00-24:00:00",
              "04:00:00-08:00:00")
names(opDomInv) <- c("VN","SV")

###Periodos horarios Verao op - MAT, AT e MT###
opSegSexVer <- c("14:00:00-17:00:00",
                 "00:00:00-00:30:00/07:30:00-14:00:00/17:00:00-24:00:00",
                 "00:30:00-02:00:00/06:00:00-07:30:00", 
                 "02:00:00-06:00:00")

names(opSegSexVer) <- c("P", "C", "VN", "SV")

opSabVer <- c("10:00:00-13:30:00/19:30:00-23:00:00",
              "00:00:00-03:30:00/07:30:00-10:00:00/13:30:00-19:30:00/23:00:00-24:00:00",
              "03:30:00-07:30:00")
names(opSabVer) <- c("C","VN","SV")

opDomVer <- c("00:00:00-04:00:00/08:00:00-24:00:00",
              "04:00:00-08:00:00")
names(opDomVer) <- c("VN","SV")


##################################################
#************************************************#
#*Ciclo di?rio para clientes (cd) - BTN e BTE ***#
#************************************************#
##################################################

###Periodos horarios Inverno cd - BTN e BTE###
cdSegDomInv <- c("09:00:00-10:30:00/18:00:00-20:30:00",
                 "08:00:00-09:00:00/10:30:00-18:00:00/20:30:00-22:00:00",
                 "06:00:00-08:00:00/22:00:00-24:00:00/00:00:00-02:00:00", 
                 "02:00:00-06:00:00")

names(cdSegDomInv) <- c("P", "C", "VN", "SV")

###Periodos horarios Verao cd - BTN e BTE###
cdSegDomVer <- c("10:30:00-13:00:00/19:30:00-21:00:00",
                 "08:00:00-10:30:00/13:00:00-19:30:00/21:00:00-22:00:00",
                 "06:00:00-08:00:00/22:00:00-24:00:00/00:00:00-02:00:00", 
                 "02:00:00-06:00:00")

names(cdSegDomVer) <- c("P", "C", "VN", "SV")
