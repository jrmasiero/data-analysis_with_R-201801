Sys.setlocale("LC_ALL", "pt_BR")
options(encoding = "UTF-8")

library(tidyverse)
library(lubridate)
salarios <- read_csv("aula-04/data/201802_dados_salarios_servidores.csv.gz")
## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.
##cotação dolar 28/02/2018 = 3,2421

salarios %>% 
  select(REMUNERACAO_REAIS,REMUNERACAO_DOLARES) %>%
  mutate( REMUNERACAO_DOLARES = REMUNERACAO_DOLARES * 3.2 ) %>%
  mutate(soma_real_dolar = REMUNERACAO_REAIS + REMUNERACAO_DOLARES ) %>%
  select(soma_real_dolar) %>%
  filter(soma_real_dolar > 900) %>%
  arrange(soma_real_dolar)

print(salarios)

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####
salarios %>% 
  select(DESCRICAO_CARGO) %>%
  count(DESCRICAO_CARGO) %>%
  filter(n >= 200) %>%
  arrange(n)%>%
  pull(DESCRICAO_CARGO) -> Acima_de_200
    

print(Acima_de_200)
  
salarios %>% 
  filter(DESCRICAO_CARGO %in% c(Acima_de_200)) -> nova_tabela_acima200

  ##cov(x = 2018 - year(DATA_INGRESSO_ORGAO ), y = 2018 - year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))

  nova_tabela_acima200%>%
    group_by(DESCRICAO_CARGO)%>%
    summarise(covalencia = abs(cov(x = 2018 - year(DATA_INGRESSO_ORGAO ), y = 2018 - year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))),
              direcao = if_else(covalencia<90.0,"colineares","colineares_opostos"))

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

