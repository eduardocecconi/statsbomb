#atualização do R (para MacOS)
#solução de dúvidas e/ou caminho para atualização em Windows:
# https://github.com/AndreaCirilloAC/updateR
# https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol/
install.packages("devtools")
devtools::install_github('andreacirilloac/updateR')
updateR()

#instalação pacote StatsBombR
#atenção ao console quando solicitar quais pacotes devem ser atualizados
install.packages("remotes")
remotes::install_version("SDMTools", "1.1-221")
devtools::install_github("statsbomb/StatsBombR")

#chamando as bibliotecas
library(StatsBombR)
library(tidyverse)
library(ggplot2)
library(grid)

#função para obter a lista de competições com dados gratuitos
Comp <- FreeCompetitions()

#função para obter a lista de jogos com dados gratuitos
Matches <- FreeMatches(Comp)

#função para obter um jogo identificado pelo seu ID 
#Arsenal 1 x 1 United - 28-03-2004
Jogo <- get.matchFree(Matches[963,])

#função recomendada pela StatsBomb para limpar os dados
Jogo <- allclean(Jogo)

#procedimento recomendado para ajustar as dimensões do campo aos dados
heatmap = Jogo %>% mutate(location.x = ifelse(location.x>120, 120, location.x),
                           location.y = ifelse(location.y>80, 80, location.y),
                           location.x = ifelse(location.x<0, 0, location.x),
                           location.y = ifelse(location.y<0, 0, location.y))

#procedimento para cortar o campo em quadrantes
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)

#agrupando os dados defensivos por time
Mapa_Defesa <- heatmap %>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" |
           type.name=="Foul Committed" | type.name=="Interception" |
           type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave)

#construção do campo com todos os detalhes para a plotagem sobre o mapa de calor
#tutorial: https://statsbomb.com/wp-content/uploads/2021/11/Working-with-R.pdf
ggplot(data= Mapa_Defesa, aes(x = location.x, y = location.y, fill = bin_DA, group = bin_DA)) +
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,
                                  family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  scale_y_reverse() + 
  scale_fill_viridis_b() +
  labs(title = "Mapa de Ações Defensivas", subtitle = "Arsenal 1x1 Manchester United, 28-03-2004") +
  coord_fixed(ratio = 95/100) +
  facet_wrap(~team.name) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) +
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE))