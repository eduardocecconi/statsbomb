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

#instalação do pacote SBpitch, para obter o desenho dos campos
#atenção ao console quando solicitar quais pacotes devem ser atualizados
devtools::install_github("FCrSTATS/SBpitch")

#instalação do pacote Soccermatics, para obter o desenho dos campos
devtools::install_github("jogall/soccermatics")

#chamando as bibliotecas
library(StatsBombR)
library(tidyverse)
library(SBpitch)
library(soccermatics)
library(ggplot2)

#função para obter a lista de competições com dados gratuitos
Comp <- FreeCompetitions()

#função para obter a lista de jogos com dados gratuitos
Matches <- FreeMatches(Comp)

#função para obter um jogo identificado pelo seu ID 
#Arsenal 1 x 1 United - 28-03-2004
Jogo <- get.matchFree(Matches[963,])

#função recomendada pela StatsBomb para limpar os dados
Jogo <- allclean(Jogo)

#seleção dos toques (Ball Receipt) do Thierry Henry no jogo
Toques_Henry <- Jogo %>%
  filter(type.name=="Ball Receipt*", team.name=="Arsenal", player.name=="Thierry Henry") %>%
  select(type.name, team.name, player.name, location.x, location.y)

#criação do campo com a função create_Pitch() do SBpitch
blank_pitch <- create_Pitch(
  goaltype = "box",
  grass_colour = "white", 
  line_colour =  "#797876", 
  background_colour = "white", 
  goal_colour = "#131313"
)

#mapa de toques na bola com pontos usando o SBpitch
blank_pitch +
  geom_point(data = Toques_Henry, aes(x=location.x, y=location.y), colour = "red", size = 4) +
  labs(title = "Henry - Toques na Bola", 
       subtitle = "Arsenal 1x1 Manchester United, 28-03-2004",
       caption = "StatsBomb Free Data & SBpitch")

#mapa de calor usando biblioteca Soccermatics
Jogo %>%
  filter(type.name == "Ball Receipt*", team.name == "Arsenal", player.name=="Thierry Henry") %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                xBins = 6, yBins = 3, arrow = "r",
                title = "Henry - Toques na Bola", 
                subtitle = "Arsenal 1x1 Manchester United, 28-03-2004 - StatsBomb Free Data & Soccermatics")

#rede de passes arsenal - Soccermatics
Jogo %>%
  soccerTransform(method='statsbomb') %>%
  filter(team.name == "Arsenal") %>%
  soccerPassmap(fill = "red", arrow = "r", labelSize = 4, 
                title = "Arsenal 1x1 Manchester United, 28-03-2004")