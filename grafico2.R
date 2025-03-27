library(tidyverse)
library(lubridate)
library(scales)

# Carrega os dados mensais (já ajustados para o formato long)
dados_long <- read_csv("salesmonthly.csv") %>%
  pivot_longer(cols = -datum, names_to = "Classe_ATC", values_to = "Quantidade") %>%
  rename(Data = datum) %>%
  mutate(Data = ymd(Data))

# Calcula o market share mensal de cada classe ATC
market_share <- dados_long %>%
  group_by(Data, Classe_ATC) %>%
  summarise(Quantidade = sum(Quantidade)) %>%
  group_by(Data) %>%
  mutate(Market_Share = Quantidade / sum(Quantidade)) %>%
  ungroup()

ggplot(market_share, aes(x = Data, y = Market_Share, fill = Classe_ATC)) +
  geom_area(alpha = 0.8, color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = percent_format()) +  # Formato de porcentagem
  scale_fill_viridis_d(option = "C") +  # Paleta de cores profissional
  labs(
    title = "DOMÍNIO DAS CLASSES ATC NO MERCADO (2014-2019)",
    subtitle = "Participação percentual mensal de cada categoria",
    x = "Ano",
    y = "Participação no Mercado",
    fill = "Classe ATC",
    caption = "Fonte: Pharma Sales Data | Análise: Seu Nome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Filtra 2018 e pega os top 3 meses de cada classe
top_2018 <- dados_long %>%
  mutate(Ano = year(Data), Mes = month(Data, label = TRUE)) %>%
  filter(Ano == 2018) %>%
  group_by(Classe_ATC, Mes) %>%
  summarise(Quantidade = sum(Quantidade)) %>%
  slice_max(Quantidade, n = 3)  # Top 3 meses por classe

ggplot(top_2018, aes(x = Mes, y = Quantidade, fill = Classe_ATC)) +
  geom_col(position = "dodge") +
  labs(
    title = "TOP MESES DE VENDA POR CLASSE ATC (2018)",
    subtitle = "R03 (respiratórios) dominou julho/agosto - surto de H1N1!",
    x = "Mês",
    y = "Vendas Totais"
  ) +
  scale_fill_brewer(palette = "Set2") +  # Cores amigáveis
  theme_minimal() +
  theme(legend.position = "bottom")

inverno_2018 <- dados_long %>%
  mutate(Ano = year(Data), Estação = case_when(
    month(Data) %in% c(6,7,8) ~ "Inverno",
    TRUE ~ "Outras estações"
  )) %>%
  filter(Ano == 2018, Estação == "Inverno") %>%
  group_by(Classe_ATC) %>%
  summarise(Quantidade = sum(Quantidade)) %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade))

ggplot(inverno_2018, aes(x = "", y = Porcentagem, fill = Classe_ATC)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = paste0(round(Porcentagem*100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y") +
  labs(
    title = "DISTRIBUIÇÃO DAS VENDAS NO INVERNO DE 2018",
    subtitle = "R03 (respiratórios) foi 28% do total!",
    fill = "Classe ATC"
  ) +
  scale_fill_viridis_d() +
  theme_void()  # Remove tudo (só mostra o gráfico)