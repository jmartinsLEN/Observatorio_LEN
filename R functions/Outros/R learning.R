
### ESTÃO AQUI SOLUÇÕES PARA O SUMIF

# # rowsum
# 
# Sum_month <- rowsum(RegInst_TS$Activa, format(RegInst_TS$timestamp, '%m-%Y'))
# 
# months(RegInst_TS$timestamp)
# 
# # tapply
# 
# Sum_month <- tapply(RegInst_TS$Activa, format(RegInst_TS$timestamp, '%m-%Y'), sum)
# 
# tapply(RegInst_TS$timestamp, format(RegInst_TS$timestamp, '%m-%Y'), mean)
#
# # aggregate

### Para definir um timestamp manualmente e obter o seu mês em Português

source("Meses_pt.R") #Atençao ao directório caso este script esteja numa pasta diferente

TS=as.POSIXlt("2018/12/01 01:00:00")
Meses_pt(TS)


### GGPLOT2


# ### [OLD] Para desenhar um gráfico de consumo mensal:
# 
# ggplot(data = RegInst_TS, aes(Meses, Consumo, fill = Consumo, label = Consumo)) +
#   stat_summary(fun.y = sum, geom = "bar") + # or "line"
#   labs(y="Consumo [kWh]") +
#   scale_x_date(breaks = as.Date(Meses$timestamp),
#                labels = Meses_labels)

# g <- ggplot(RegInst_toplot, 
#             # keep all aesthetics in one place
#             aes(x=Mes_pt,y=Consumo, fill = Consumo, label = Consumo)) +
#   # replacement of geom_bar(stat = "identity")
#   geom_col() +
#   # avoid overlap of text and bar to make text visible as bar and text have the same colour 
#   geom_text(nudge_y = max(RegInst_toplot$Consumo)/30) +
#   # alternatively, print text inside of bar in discriminable colour
#   # geom_text(nudge_y = -1, color = "black") + 
#   scale_x_discrete(limits=RegInst_toplot$Mes_pt,labels = abbreviate(RegInst_toplot$Mes_pt,3L,use.classes = 0)) +
#   ggtitle("Consumo mensal") + 
#   xlab("Mês") + ylab("Consumo [MWh]") +
#   #theme_dark() +
#   #theme_bw() + 
#   guides(fill=guide_legend(title="Consumo [MWh]")) +
#   #theme(legend.position = "left") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   # usar com o as.factor de fill:
#   # scale_fill_manual(values=wes_palette(n=12, name="Cavalcanti", type = c("continuous")))
#   # usar sem o as.factor de fill:
#   # scale_fill_gradient(low="blue", high="red")
#   scale_fill_gradientn(colours = wes_palette(name="Cavalcanti", type = c("continuous")))

###

### Quando preciso extrair mais que um objecto como output de uma função a notação
### é a seguinte:
# RegInst_agg_m_G1 = AGG[[1]]
# AUX1 = AGG[[2]]
# AUX2 = AGG[[3]]


### Último mês (o actual) num vector:
M = month(last(df[,1]))

#Última data de um vector:
#Dt = as.POSIXct(last(df[,1]), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
Dt = as.Date(last(df[,1]))

### Obter primeira e última data que figuram no df:
Dti = (df_agg_d[1,1])
Dtf = last(df_agg_d[,1])

