# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)
library(lubridate)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

palestras <- read_csv("C:/Users/joser/Documents/data-analysis_with_R-201801/aula-05/data/ted_main.csv.gz")

palestras_df <-as.data.frame(palestras)


# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
palestras_df %>%
  select_if(.predicate = is.numeric) %>%
summary(palestras_df)


# Converta as seguintes variáveis utilizando o pacote Lubridate:

#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
palestras_df %>% 
  mutate(duration = duration(duration,"seconds")) -> palestras_df
summary(palestras_df)
#     * film_date, para data, com a função as_datetime.
palestras_df %>% 
  mutate(film_date = as_datetime(film_date,tz = "UTC")) -> palestras_df

summary(palestras_df)
#     * published_date, para data, com a função as_datetime..

palestras_df %>% 
  mutate(published_date = as_datetime(published_date,tz = "UTC")) -> palestras_df
summary(palestras_df)

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

palestras_df %>%
  mutate(event = factor(event)) %>%
  mutate(speaker_occupation = factor(speaker_occupation)) ->palestras_df

summary(palestras_df)


# Retire do dataframe a variável name

palestras_df %>% 
  select(-name) -> palestras_df



# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas

summary(palestras_df)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

palestras_df %>% 
  select(languages)%>%
  filter(languages>=1)%>%
  arrange(desc(languages))%>%
  tail(15)



# Verifique os 15 registros com menor data de filmagem. 

palestras_df %>% 
  select(film_date)%>%
  arrange(desc(film_date))%>%
  tail(15)



# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
palestras_df %>% 
  count(film_date) -> ano_quant

summary(ano_quant)
# Analise os 10 quantis da quantidade de apresentações por ano.
palestras_df %>%
count(palestras_df$film_date)
quantile(palestras_df$film_date,probs = seq(0, 1, 0.1), na.rm = FALSE, names = TRUE) -> quartis

print(quartis)

# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

palestras_df %>%
  count(palestras_df$film_date == as_date("2013-09-20 00:00:00 UTC")) -> quant_de_q4

print(quant_de_q4)

palestras_df %>%
  count(palestras_df$film_date)
  filter(palestras_df$film_date)


# Verifique novamente o resumo dos dados do dataframe




# Verifique os 10 registros com maior duração.


palestras_df %>%
select(duration)%>%
  arrange(desc(duration))
  head(10)
# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas


palestras_df %>%
  select(duration)%>%
  arrange(desc(duration))%>%
head(10)

print(duracao_pal)



# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil

palestras_df %>%
  count(palestras_df$duration)
quantile(palestras_df$duration) -> quatil_dur

print(quatil_dur)

IQR(palestras_df$duration)

# Visualize os 10 quantis da quantidade de visualizações

palestras_df %>%
  count(palestras_df$duration)
quantile(palestras_df$duration,probs = seq(0, 1, 0.1), na.rm = FALSE, names = TRUE)


# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




