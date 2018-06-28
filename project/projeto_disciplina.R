# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
# bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")
library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)


departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos

##numero de produtos vendidos ==  numero de ocorrencia dos produtos na tabelaOrder_products
##nao tem segunda, dom, etc, tem numero 1,2,3,4,5

## Order Id é unico, OrderNumber não é

##


#1 # Quantos dos produtos do cadastro nunca foram comprados?
 
produtos_nao_comprados <- anti_join(products,insta_products, by = "product_id", copy = FALSE)

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

#Dica: Usar joints()

lista_prod_completa <- merge(products,departments,by="department_id")
merge(lista_prod_completa,aisles,by="aisle_id") -> lista_prod_completa


#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.
lista_prod_completa%>%
  group_by(aisle_id,department_id)%>%
  summarise(qtde = n()) %>%
  arrange(desc(qtde))%>%
  head(10) -> lista_10_mais
 
#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?
##Categoria do Departamento 

## RESPOSTA = 62.7 %

merge(lista_prod_completa,lista_10_mais,by=c("aisle_id","department_id"))->lista_10_mais_extendida
  
pedidos_nos_10_mais <- inner_join(insta_products, lista_10_mais_extendida, by = "product_id", copy = FALSE)

pedidos_nos_10_mais%>%
  group_by(order_id)%>%
  summarise(qtde = n())
  
  insta_products%>%
    group_by(order_id)%>%
    summarise(qtde = n())
  
  

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)
  pedidos_nos_10_mais%>%
    filter(department!= "missing" & aisle != "missing") -> pedidos_nos_10_mais_sem_missing

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   # Transforme as variáveis user_id, department e aisle em factor
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)

   # Este dataframe deverá ser utilizado em todas as atividades seguintes
  
## lista_prod_completa (products + aisles + department)
  
  lista_completa <- merge(lista_prod_completa,insta_products, by = "product_id")
  merge(lista_completa,insta_orders,by = "order_id") -> lista_completa
  
  lista_completa %<>%
    mutate( user_id = factor(user_id)
            , department = factor(department)
            , aisle = factor(aisle)
            ,order_hour_of_day = factor(order_hour_of_day,ordered = TRUE)) ->lista_completa_temp
  
  print(max(lista_completa$order_hour_of_day))
  
 
  
#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
  lista_completa_temp %>%
    group_by(order_hour_of_day) %>%
    summarise(qtde = n()) %>%
    arrange(desc(qtde)) %>%
    head(5)
    
    

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
  lista_completa_temp %>%
    group_by(order_hour_of_day) %>%
    summarise(qtde = n()) %>%
    arrange(desc(qtde)) %>%
    head(5) %>%
    pull(order_hour_of_day) -> horarios_top_5
  
  print(horarios_top_5)
  
  lista_completa_temp %>%
    select(order_hour_of_day, product_id) %>%
    filter(order_hour_of_day %in% c(horarios_top_5))  %>%
    count(product_id) %>%
    arrange(desc(n)) %>%
    head(15) %>%
 pull(product_id) -> quinze_mais_vendidos
  
  print(quinze_mais_vendidos)

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 
  
  lista_completa_temp %>%
    select(product_id, order_hour_of_day) %>%
    filter(product_id %in% c(quinze_mais_vendidos))  %>%
    group_by(product_id,order_hour_of_day) %>%
    summarise(qtde = n()) %>%
    arrange(desc(product_id)) %>%
    group_by(product_id)  %>%
    summarise(media_por_hora = mean(qtde)) -> media_produto_por_hora
  
  media_produto_por_hora %>%
  pull(product_id) -> etiquetas
  
  print(etiquetas)
  
  ggplot(media_produto_por_hora,aes(x=seq(1,15,1), weight=media_produto_por_hora$media_por_hora) ) + 
    scale_x_continuous(breaks = seq(1,15,1), labels =etiquetas)+
    labs(x="Produtos", y = "quantidade_por_hora")+
    geom_bar()
    
    
  
  max(media_produto_por_hora$media_por_hora)

#10 # Calcule as seguintes estatísticas descritivas sobre 
  #a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
  lista_completa_temp %>%
    select(product_id, order_hour_of_day, order_dow) %>%
    group_by(order_dow,order_hour_of_day) %>%
    summarise(qtde = n())%>%
    group_by(order_hour_of_day) %>%
    summarise(media_por_hora = mean(qtde),
              desvio_padrao = sd(qtde), 
              mediana = median(qtde ),
              desv_ab_med = median( abs(qtde - median(qtde ))),
              menor_venda = min(qtde),
              maior_venda = max(qtde))  -> dados_venda_horas_dias
  
  
    

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda
  dados_venda_horas_dias%>%
  ggplot( aes(x = order_hour_of_day, y = dados_venda_horas_dias$media_por_hora)) +
    geom_col() +
    theme_minimal()
  


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.
  lista_completa_temp %>%
    select(product_id, order_hour_of_day, order_dow) %>%
    group_by(order_dow,order_hour_of_day) %>%
    summarise(qtde = n()) %>%
  group_by(order_dow) %>%
    summarise(media_por_hora = mean(qtde)) ->media_pedidos_por_hora_cada_dia
  
  media_pedidos_por_hora_cada_dia %>%
  ggplot( aes( x = order_dow, y = media_por_hora, group = order_dow )) +
    geom_boxplot() +
  scale_x_continuous( breaks = 0:6 ) +
    scale_y_continuous( breaks = seq(from = 0, to = 15000, by = 1000 )) +
    theme_bw()
  
  

#13 # Identifique, por usuário, o tempo médio entre pedidos
  
  lista_completa_temp %>%
    select(user_id,days_since_prior_order)%>%
    group_by(user_id)%>%
    summarise(tm_entre_pedidos = mean(days_since_prior_order)) ->tempo_m_usuario


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado
tempo_m_usuario %>%
  group_by(tm_entre_pedidos) %>%
  summarise(qtde = n()) -> tempo_m_usuario_pgrafico

tempo_m_usuario_pgrafico%>%
  ggplot( aes(x = tm_entre_pedidos, y = qtde)) +
  geom_col() +
  theme_minimal()


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 
lista_completa_temp %>%
  select(user_id,days_since_prior_order)%>%
  group_by(days_since_prior_order)%>%
  summarise(qtde = n()) -> usuario_pordia_pgrafico

usuario_pordia_pgrafico%>%
  ggplot( aes(x = days_since_prior_order, y = qtde)) +
  geom_col() +
  theme_minimal()

#Resposta: Sim, são muito similares, quase iguais.


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?
lista_completa_temp %>%
  select(user_id,days_since_prior_order)%>%
  group_by(user_id)%>%
  summarise(tm_entre_pedidos = mean(days_since_prior_order)
                               ,qtde = n())%>%
  filter(qtde>=5) -> usuario_5mais_pedidos

usuario_5mais_pedidos %>%
  group_by(tm_entre_pedidos) %>%
  summarise(numero = n()) -> usuario_dia_pgrafico

usuario_dia_pgrafico%>%
  ggplot( aes(x = tm_entre_pedidos, y = numero)) +
  geom_col() +
  theme_minimal()

#Resposta: SIM, o padrão se mantem.


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
    bananas <- c(24852, 13176, 39276, 37067, 29259)
    
    lista_completa_temp %>%
      select(order_id,product_id)%>%
      filter(product_id %in% bananas)%>%
      group_by(order_id)%>%
      summarise(tipos_de_banana = n())%>%
      filter(tipos_de_banana>1) %>%
      pull(order_id) -> pedidos_duas_bananas
    

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências
    lista_completa_temp %>%
      select(order_id,product_id)%>%
      filter(product_id %in% bananas)%>%
      filter(order_id %in% pedidos_duas_bananas)%>%
      group_by(product_id)%>%
      summarise(tipo_banana_ocorrencia = n())%>%
      arrange(desc(tipo_banana_ocorrencia))%>%
      head(3) %>%
      pull(product_id)-> bananas_mais_vendidas
      
      

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

    lista_completa_temp %>%
      select(product_id,order_dow,order_hour_of_day)%>%
      filter(product_id %in% bananas_mais_vendidas)%>%
      group_by(order_dow,order_hour_of_day)%>%
      summarise(qnte = n())%>%
      group_by(order_dow)%>%
      summarise(venda_por_hora = mean(qnte)) -> media_venda_dia
    

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora
    
    lista_completa_temp %>%
      select(product_id,order_dow,order_hour_of_day)%>%
      filter(product_id %in% bananas_mais_vendidas)%>%
      group_by(order_dow,order_hour_of_day)%>%
      summarise(qnte = n()) -> dia_hora_qnt

    
    dia_hora_qnt%>%
      ggplot( aes(x = order_dow, y = order_hour_of_day, size = dia_hora_qnt$qnte)) +
      scale_size(range = c(0, 5)) +
      geom_point(color = "blue") +
      theme_minimal()
    
  

#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

