install.packages("tidyverse")
library(tidyverse)

tabela <- read.csv("C:\\Users\\Pichau\\Documents\\Data Analysis\\Case Study\\top5_leagues_player.csv")

#visualizar a estruta dos dados
glimpse(tabela)

#ver o nome das colunas
colnames(tabela)

#ciração da tabela nova, e exclusão do valores em branco
tabela_nova <- select(tabela, "X",  "name", "age", "height", "place_of_birth", "price", "max_price", "position", "league", "club", "foot", "nationality")
tabela_nova <- na.omit(tabela_nova)

#extrair apenas a primeira palavra da coluna nacionalidade
tabela_nova$nationality <- str_extract(tabela_nova$nationality, "\\w+")



#remover linhas com valores em branco
tabela_nova <- tabela_nova[rowSums(tabela_nova == "")==0, ]

#Contar a quantidade de jogadores por pé
qnt_foot <- tabela_nova %>%
  count(foot)


soma_mean_both <- tabela_nova %>%
  filter(foot == "both") %>%
  summarise(soma_max_price = sum(max_price, na.rm = TRUE),
            mean_max_price = mean(max_price, na.rm = TRUE))



soma_mean_right <- tabela_nova %>%
  filter(foot == "right") %>%
  summarise(soma_max_price = sum(max_price, na.rm = TRUE),
            mean_max_price = mean(max_price, na.rm = TRUE))


#criação da tabela de acordo com o esquerdo
soma_mean_left <- tabela_nova %>%
  filter(foot == "left") %>%
  summarise(soma_max_price = round(sum(max_price, na.rm = TRUE),2),
            mean_max_price = mean(max_price, na.rm = TRUE))

##criação da tabela combinada
tabelas_combinadas <- bind_rows(soma_mean_both %>% mutate(origem = "both"),
                                soma_mean_left %>% mutate(origem = "left"),
                                soma_mean_right %>% mutate(origem = "right"))


#arrendondamento da media de valores
tabelas_combinadas <- tabelas_combinadas%>%
  mutate(mean_max_price = round(mean_max_price, 2))


#calculos de soma e media de preços pela nacionalidade do jogador
soma_paises <- tabela_nova %>%
  group_by(nationality) %>%
  summarise(total_preco = sum(max_price, na.rm = TRUE),
            mean_preco = mean(max_price, na.rm = TRUE))


##criação da tabela com os 5 jogadores mais caros
top_five_player <- top_price_player_country%>%
  arrange(desc(max_price))%>%
  head(5)

##criação da tabela com a quantidade de paises
conta_paises <- tabela_nova%>%
  count(nationality)

##criação da tabela com os 10 paises com mais jogadores revelados
top10_country_player <- conta_paises%>%
  top_n(10, n)

##criação da tabeela com os 10 jogadires mais caro
top_price_player_country <- tabela_nova%>%
  top_n(10, max_price)%>%
  group_by(name)


# criação de gráfico media dos preços de acordo com pé dominante.
ggplot(tabelas_combinadas, aes(x = origem, y = round(mean_max_price, 2), fill = origem, label = mean_max_price)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Media de preço de acordo com pé",
       x = "Pé",
       y = "Preço") +
  theme_minimal()


#grafico com a quantidade de jogadores pelo pé de chute
ggplot(qnt_foot, aes(x = "", y = n, fill = foot, label = paste0(round((n/sum(n))*100), "%"))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Porcentagem de Jogadores por Pé",
       x = NULL,
       y = NULL) +
  coord_polar(theta = "y") +
  theme_minimal()


##criação do gráfico com os 10 paises com mais jogadores na liga
ggplot(top10_country_player, aes(x = nationality, y = n, fill = nationality, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Top 10 países por quantidade de jogador",
       x = "País",
       y = "Quantidade") +
  theme_minimal()+
  theme(axis.text.x = element_blank())


  
##criação de grafico com os cinco jogadores mais caros 
ggplot(top_five_player, aes(x = name, y = max_price, fill = name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = max_price), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Top 5 jogadores mais caros",
       x = "Nome",
       y = "Preço") +
  theme_minimal()
