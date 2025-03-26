library(lubridate)
library(ggplot2)
library(tidyverse)
library(viridis)

#Carregando os arquivo .csv de dados mensais
dados = read_csv("salesmonthly.csv")

#inspeciona as colunas
glimpse(dados)

#converte em formato long (Criando uma coluna para categorias ATC)
dados_long = dados %>%
  rename(Data = datum) %>%
  pivot_longer(
    cols = -Data, #mantem a coluna 'Data'
    names_to = "Classe_ATC",
    values_to = "Quantidade"
  )

#converte 'datum' para data
dados_long = dados_long %>%
  mutate(Data = ymd(Data)) #o formato YYYY-MM-DD


dados_long = dados_long %>%
  mutate(
    Estação = case_when(
      month(Data) %in% c(12, 1, 2) ~ "Verão",
      month(Data) %in% c(3, 4, 5) ~ "Outono",
      month(Data) %in% c(6, 7, 8) ~ "Inverno",
      month(Data) %in% c(9, 10, 11) ~ "Primavera"
    )
  )

#agrupa por categoria ATC e estação]
vendas_estacao = dados_long %>%
  group_by(Classe_ATC, Estação) %>%
  summarise(Total_Vendas = sum(Quantidade)) %>%
  ungroup()

#calcula a % de contribuição de cada estação para a categoria
vendas_estacao =  vendas_estacao %>%
  group_by(Classe_ATC) %>%
  mutate(
    Contribuicao_Perc = (Total_Vendas / sum(Total_Vendas)) * 100
  ) %>%
  ungroup()

#criação do primeiro gráfico, um heatmap de vendas (categorias x estação)
ggplot(vendas_estacao, aes(x = Estação, y = Classe_ATC, fill = Total_Vendas)) +
  geom_tile()+
  scale_fill_gradient(low = "#F0F8FF", high = "#006400") +
  labs(
    title = "Vendas por Categoria ATC e Estação",
    subtitle = "Categoria N02BE (Analgésicos e Antipiréticos) domina no inverno",
    x = "Estação",
    y = "Categoria ATC",
    fill = "Total de Vendas"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#criação do segundo gráfico, um gráfico em barras de top categorias por estação.
top_categorias_inverno = vendas_estacao %>%
  filter(Estação == "Inverno") %>%
  arrange(desc(Total_Vendas)) %>%
  slice_head(n = 3)

top_categorias_verao = vendas_estacao %>%
  filter(Estação == "Verão") %>%
  arrange(desc(Total_Vendas)) %>%
  slice_head(n = 3)

ggplot(top_categorias_inverno, aes(x = reorder(Classe_ATC, -Total_Vendas), y = Total_Vendas)) +
  geom_col(fill = "#4682B4") + 
  labs(
    title = "Top 3 Categorias no Inverno",
    x = "Categoria ATC",
    y = "Vendas"
  ) + 
  theme_minimal()

ggplot(top_categorias_verao, aes(x = reorder(Classe_ATC, -Total_Vendas), y = Total_Vendas)) +
  geom_col(fill = "#4682B4") + 
  labs(
    title = "Top 3 Categorias no Verão",
    x = "Categoria ATC",
    y = "Vendas"
  ) + 
  theme_minimal()


dados_long %>%
  mutate(Ano = year(Data)) %>%
  filter(Classe_ATC %in% c("N02BE", "N05B", "M01AB")) %>%
  group_by(Ano, Classe_ATC) %>%
  summarise(Vendas = sum(Quantidade)) %>%
  ggplot(aes(x = Ano, y = Vendas, color = Classe_ATC)) +
  geom_line(size = 1.5) +
  labs(title = "Crescimento Explosivo de N02BE (12% ao ano!)") 

ggplot(vendas_estacao, aes(x = Estação, y = Classe_ATC, fill = Total_Vendas)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Contribuicao_Perc, 1), "%")), color = "white", size = 4) +  # Anotações em %
  scale_fill_gradient(low = "#F0F8FF", high = "#228B22") +
  labs(title = "N02BE domina 45% das vendas no inverno!")  

ggplot(vendas_estacao, 
       aes(x = factor(Estação, levels = c("Primavera", "Verão", "Outono", "Inverno")),
           y = reorder(Classe_ATC, -Total_Vendas), 
           fill = Total_Vendas)) +
  geom_tile(color = "white", linewidth = 0.5) +  # Linhas brancas entre os tiles
  geom_text(aes(label = paste0(round(Contribuicao_Perc, 1), "%")), 
            color = "white", 
            size = 4, 
            fontface = "bold") +
  scale_fill_viridis(
    option = "C", 
    direction = -1, 
    labels = scales::comma,
    name = "Vendas Totais"
  ) +
  labs(
    title = "DOMÍNIO SAZONAL DAS CLASSIFICAÇÕES ATC",
    subtitle = "Cada célula mostra o % de contribuição da estação para a categoria",
    x = "Estação",
    y = "Classe ATC",
    caption = "Fonte: Pharma Sales Data (2014-2019)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  coord_fixed(ratio = 0.8) 