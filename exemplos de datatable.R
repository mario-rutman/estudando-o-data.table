# O objetivo deste projeto é conhecer e aprender a usar o pacote data.table. 
# É uma forma de trabalhar os dados muito rápida, boa para grandes quantidades de dados.
# A primeira impressão que tive foi a simplicidade e elegância da sintaxe.

# General form of data.table syntax
# DT[i, j, by]
# |  |  |
#    |   |  --> grouped by what?
#    |  -----> what to do?
#   --------> on which rows?

# Pega o DT, escolhe as linhas (i), faz um cálculo (j) por agrupamento (by).

library(data.table)
library(bikeshare14) # para obter o data.frame batrips.
# library(tidyverse)

# Create the data.table X 
X <- data.table(id = c("a", "b", "c"), value = c(0.5, 1.0, 1.5))

# View X
X

# Agente sabe que é um data.table quando o número das 
# linhas vem seguido de dois pontos.

# Qual a classe de batrips? R.dataframe.
class(batrips)

# Mas quero que seja também data.table! 
batrips <- as.data.table(batrips)
class(batrips)


head(batrips)

head(batrips, 4)

tail(batrips, 3)

str(batrips)

ncol(batrips)

nrow(batrips)

table(batrips$subscription_type)

batrips[3] #Terceira linha.

batrips[3,] #Terceira linha.

batrips[1:4] #Linhas de 1 a 4

batrips[1:4,] #Linhas de 1 a 4

batrips[12:15] #Linhas de 12 a 15 

batrips[-(12:15)] #Todas as linhas menos as de 12 a 15.

batrips[!(12:15)] #Todas as linhas menos as de 12 a 15.

batrips[c(1,6,10)] #Linhas 6, 7 e 10.

batrips[!c(1:5, 10:15)] #Todas linhas menos as de 1 a 5 e 10 a 15.

batrips[326339] #Última linha da batrips.

batrips[.N] #Última linmha da batrips.

batrips[!c(1, .N)] #todas nemos a 1ª e a última.

batrips[1:(.N-10)] #Todas as linhas até faltarem 10 para a última.

#Todas as linhas cuja coluna subscription_type é "Subscriber",
# poderia ser também "customer".
batrips[subscription_type == "Subscriber"] 

#Todas as linhas onde start_terminal é 58 e end_terminal não é 65.
batrips[start_terminal == 58 & end_terminal != 65]

batrips[start_station == "MLK Library" & duration > 1600] # Já está ficando repetitivo, não precisa explicar mais.

batrips[subscription_type != "Subscriber"]

batrips[start_station == "Ryland Park" & subscription_type != "Customer"]

# %like%
vvr <- c("aaba", "aaba", "baca")

# Localiza "aa" em qualquer string
vvr %like% "aa"

# Localiza "ba" ao final de qualquer string
vvr %like% "ba$"

# Todas as linhas em que a coluna start_station começa com San Francisco.
# Tem San Francisco City Hall, San Francisco Caltrain 2, San Francisco Caltrain etc. 
batrips[start_station %like% "^San Francisco"]

# Todas linhas com duração entre 2000 and 3000. Incluí 2000 e 3000.
# SÓ SERVE PARA COLUNAS NUMÉRICAS!
batrips[duration %between% c(2000, 3000)]

# Filtra na coluna trip_id alguns valores.
batrips[trip_id %in% c(588841, 139560, 139562)]

# Todas linhas que contenham os characters "Japantown", "Mezes Park", "MLK Library".
# SÓ SERVE PARA COLUNAS CHARACTER!
batrips[start_station %chin% c("Japantown", "Mezes Park", "MLK Library")]

# Filtra todas as end_station em que a palavra Market aparece ao final.
# Beale at Market, Steuart at Market etc.   
batrips[end_station %like% "Market$"]

# Selecionando a coluna trip_id.
# O resultado é um data.table, se fosse data.frame o resultado seria um vetor. 
ans <- batrips[, "trip_id"]
str(ans)
head(ans, 2)

# Podemos selecionar as colunas por número, mas não é recomendável.
# Pois a numeração das colunas muda na limpeza dos dados.
ans <- batrips[, c(2, 4)]
head(ans, 2)

# Selecionando colunas por nomes.
batrips[, c("duration", "start_station")]

# Podemos também selecionar por exclusão.
# Select all cols *except* those shown below
ans <- batrips[, -c("start_date", "end_date", "end_station")]
ans <- batrips[, !c("start_date", "end_date", "end_station")]
head(ans, 1)

# Aqui faz 2 coisas: seleciona as colunas trip_id e duration; e
# e troca o nome (rename) da duration por dur.
# NÃO PRECISA DE ASPAS!
# O resultado será um data.table
ans <- batrips[, list(trip_id, dur = duration)]

# O ponto aqui substituí a palavra list.
ans <- batrips[, .(trip_id, dur = duration)]

# Se fizermos sem o list, o resultado será um vetor.
ans <- batrips[, .(trip_id)]
head(ans, 4)

ans <- batrips[, (trip_id)]
head(ans, 4)

# Podemos fazer os cálculos diretamente nas colunas.
ans <- batrips[, mean(duration)]
ans <- batrips[, round(mean(duration),2)]

# Podemos calcular as média das duration só para Japantown. 
ans <- batrips[start_station == "Japantown", round(mean(duration),2)]

# Filtro todas as linhas cuja start_station foi Japantown
# e conto quantas linhas (.N).
# How many trips started from "Japantown"?
batrips[start_station == "Japantown", .N] # Aqui o resultado é um vetor.
batrips[start_station == "Japantown", .(.N)] # aqui é outro data.table.

# Aqui buscamos a mediana da duration nas linhas em que
# a coluna end_station é "Market at 10th" e a subscription_type é "Subscriber".
batrips[end_station == "Market at 10th" & subscription_type == "Subscriber", median(duration)]

# Diferença entre datas.
date1 <- "2018-12-20 11:30:00 EST"
date2 <- "2018-12-20 11:20:00 EST"
difftime(date1, date2, units = "min")
difftime(date1, date2, units = "hour")

date3 <- "2018-10-25"
date4 <- "2020-12-20"
difftime(date4, date3, units = "weeks")
difftime(date4, date3, units = "auto")

# Calculando estatísticas. 
# Aqui o resultado foi um data.table de uma linha.
batrips[, .(mn_dur = mean(duration), med_dur = median(duration))]

# Filtrando na coluna start_station o que é Japantown,
# depois, sobre isso, calculando a média e mediana da duration. 
batrips[start_station == "Japantown", .(mn_dur = mean(duration), 
                                        med_dur = median(duration))]

# Calculando a mínima e a máxima duration.
# O resultado é um data.table e as colunas novas
# foram automaticamente nomeadas de V1 e V2.
batrips[, .(min(duration), max(duration))]

# Aqui calculou a media e máxima duration e ainda 
# nomeou as colunas do data.table resultante. 
batrips[, .(mean_duration = mean(duration), 
            last_ride = max(end_date))]


# Ao invéz de fazer uma conta podemos fazer um gráfico.
batrips[, hist(duration)]

# O gráfico ainda pode ser feito a partir de uma filtragem.
batrips[start_station == "Townsend at 7th" & duration < 500, hist(duration)]

# Aqui começa a sofisticar.
# O by argument é o group_by do dplyr

# Quantas viagens foram feitas a partir de cada start_station?
ans <- batrips[, .N, by = "start_station"] # Estas duas formas equivalem.
ans <- batrips[, .N, by = .(start_station)]
head(ans, 10)
ans

# Além de calcular as colunas podemos nomeá-las.
ans <- batrips[, .(num_d_viag = .N), by = .(inicio = start_station)]
head(ans, 10)

# Aqui contamos (.N) a quantidade de strat_station por mês.
# Usamos a função month() do data.table, aplicado ao start_date.
ans <- batrips[ , .N, by = .(start_station, mon = month(start_date))]
ans
nrow(ans)

# Básico: calculando a duration média por start_station.
# Dei uma guaribada para o resultado ficar natural para brasileiros.
batrips[, .(mean_duration = format(round(mean(duration), 2), decimal.mark = ",", big.mark = ".")),
        by = start_station]

# Calculando a média da duration por (group_by) start_station e end_station.
batrips[, .(mean_duration = mean(duration)), by = .(start_station, end_station)]

# Calculando a média da duration e o número total de viagens agrupados por
# start_station e end_station
aggregate_mean_trips <- batrips[, .(mean_duration = mean(duration), 
                                    total_trips = .N), 
                                by = .(start_station, end_station)]

# Calculate the total number of unique bike ids for every month
# Observe que nomeou automaticamente a coluna.
ans <- batrips[, uniqueN(bike_id), by = month(start_date)]
head(ans, 12)

# Mas podemos chamá-la de unic_bici.
ans <- batrips[, .(unic_bici = uniqueN(bike_id)), by = month(start_date)]
head(ans, 12)

# Contando as bicicletas em todas combinações de start_station e end_station e 
# encadeando (%>%) com a operação de ordenamento decrescente. 
batrips[, .N, by = .(start_station, 
                     end_station)][order(-N)]

# Encadeando %>%.
# Contamos o número de chegadas por end_station e então
# ordenamos decrescentmente e então
# filtramos as 10 primeiras linhas.
top_10 <- batrips[, .N, by = end_station][order(-N)][1:10]
top_10

# Aqui parece meio complicado.
# Ordena crescentemente o start_data, depois
# agrupa por start_station, depois
# cria a coluna start_date que é formada pela
# 1ª e última start_date de cada start_station. 
batrips[order(start_date), 
        .(start_date = start_date[c(1, .N)]), 
        by = start_station]

# Usando o .SD.
# Por parecer confuso vou usar um exemplo.
# Criando a data.table DT.
DT = data.table(
  nome = c("alice","alice","alice","anita","anita","maria"),
  nota = 1:6,
  peso = 7:12,
  data = 13:18
)

# Usando o .SD (subset data, é igual aos conjuntos e subcojuntos do colégio)
# Pegou o DT e fez 3 data.table agrupados por nome.
# Um da Alice, um da Anita e 1 da Maria.
DT[, print(.SD), by = nome]

# Podemos também fazer um cálculo por nome, média por exemplo.
# Dá como resultado a média de cada uma nos quesitos nota, peso e data.
# Aqui o resultado é um só data.table.
DT[, lapply(.SD, mean), by = nome]

# O .SD considera todas as colunas, mas podemos escolher quais queremos.
# .SDcols
DT[, lapply(.SD, mean), by = nome, .SDcols = c("nota", "data")]
DT[, lapply(.SD, mean), by = nome, .SDcols = c("peso")]


# Aqui pega a 1ª linha de cada start_station,
# e mantém todas as colunas.
batrips[, .SD[1], by = start_station]

# Aqui pega a 1ª linha de cada start_station,
# mas só fica com as colunas tirp_id e duration.
batrips[, .SD[1], by = start_station, .SDcols = c("trip_id", "duration")]


# Operador := (colon equal). Atualiza uma data.table por referência.
# Acho que é o equivalente ao mutate do dplyr.
#

# Aqui adiciona 2 colunas ao batrips.
batrips[, c("is_dur_gt_1hour", "week_day") := list(duration > 3600, 
                                                   wday(start_date))]

# Quando adiciona uma só coluna a forma é simplificada.
batrips[, is_dur_gt_1hour := duration > 3600]

# Pode usar também o formato função := 
# Aqui a coluna is_dur_gt_1hour é retirada e a start_station 
# fica em letras maiúsculas.
batrips[, `:=`(is_dur_gt_1hour = NULL,                  
               start_station = toupper(start_station))][]

# Aqui acrescenta a coluna duration_hour, que é
# a durantion expressa em horas.
batrips[, duration_hour := duration/3600][]

# Foi criada a coluna duration_mean, é a média da duration 
# para cada combinação de start_station e end_station.
batrips[, duration_mean := mean(duration), by = .(start_station, end_station)][]

# Foi criada a coluna trips_N, para contar (.N)
# as viagens por start_station.
batrips[, trips_N := .N, by = start_station][]

# Criação da coluna mean_dur, a média da duration depois de 
# retirados os NA, agrupadas por mês. 
batrips[, mean_dur := mean(duration, na.rm = TRUE), 
            by = month(start_date)][]

# Criação da coluna mean_dur, a média da duration depois de 
# retirados os NA, agrupadas por mês. 
# Depois faz: se a duration é NA troca por mean_dur.
# No final descarta a coluna mean_dur.
batrips_new[, mean_dur := mean(duration, na.rm = TRUE), 
            by = month(start_date)][is.na(duration), 
                                    duration := mean_dur][, mean_dur := NULL]

# Aqui deu uma sofisticada.
# Foi criada a coluna trip_category. 
# 1º criou-se o objeto med_dur (a média da duration).
# Depois fez-se um ifelse para criar as categorias short, medioum e long.
# Tudo agrupado por start_station e end_station.
batrips[, trip_category := {
  med_dur = median(duration, na.rm = TRUE)
  if (med_dur < 600) "short"
  else if (med_dur >= 600 & med_dur <= 1800) "medium"
  else "long"
},
by = .(start_station, end_station)
][, .(start_station, end_station, trip_category)]

batrips[1:3]


# Aqui foram criadas duas colunas por start_station.
# Deu erro!
batrips[, c("mean_duration", "median_duration") := .(mean(duration), 
                                                     median(duration)),
        by = start_station]

# O mesmo que acima porém usando a função `:=``(). 
# Deu erro!
batrips[, `:=`(mean_duration = mean(duration),
               median_duration = median(duration)),
        by = start_station]

# Aqui filtrei a duration, cireia coluna mean_duration agrupada por 
# start_station e end_station
batrips[duration > 600, mean_duration := mean(duration),
        by = .(start_station, end_station)]

# Importando com fread.

# Pie chart.
mydata = sample(LETTERS[1:5],16,replace = TRUE)
mydata.count= table(mydata)
pie(mydata.count, col=rainbow(10))

# Função para gerar de senha.
password.generator <- function(len, n){
  dummydt=data.frame(matrix(ncol=0,nrow=n))
  num <- 1:9
  spcl <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "/", ":",
            ";", "<", "=", ">", "?", "@", "[", "^", "_", "{", "|", "}", "~")
  comb <- c(num, spcl, letters, LETTERS)
  p <- c(rep(0.035, 9), rep(0.015, 25), rep(0.025, 52))
  password<-replicate(nrow(dummydt),paste0(sample(comb, len, TRUE, prob = p), collapse = ""))
  dummydt$password<-password
  return(dummydt)
}

password.generator(8, 3)
