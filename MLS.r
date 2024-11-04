library(readxl)
library(ggplot2)

localpath <- "C:\Users\..."

MLS_C <- read.csv(localpath)

head(MLS_C)


summary(MLS_C)

# Verificacao de Assistencia em comparacao ao valor InStat
ggplot(aes(x = InStat.Index, y = Assistências),data = MLS_C) + 
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'black') + 
  xlim(200,300) + geom_line(stat = 'summary', fun.y = 'mean', vjust = -1, color ='green')

# Pode se observar que atletas com muitas assistencias tem uma tendência a estarem mais a direita, ou seja, estão acima da média
# Verificar se há possível correlação

cor(MLS_C$InStat.Index,MLS_C$Assistências)

# Identificando os jogadores com mais assistências e acima da média em ambos fatores
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Assistências >= 6))


# Correlação de 33%, é um fator de relevância média

# Queremos saber se jogadores com criação de jogadas tem influência na assistência para essa liga

ggplot(aes(x = InStat.Index, y = Passes.chave),data = MLS_C) +
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'blue') +
  xlim(200,300) 


cor(MLS_C$InStat.Index,MLS_C$Passes.chave)

# Cerca de quarenta por cento, parece ser um fator a ser levado em conta


ggplot(aes(x = Assistências, y = Gols),data = MLS_C) + geom_tile()

# Verificando overlays
qplot(x = InStat.Index, data = MLS_C) + scale_x_continuous(breaks = seq(180, 300, 10), limits = c(180, 290) )

# Nome dos jogadores que mais criam jogadas pelo clube, e tem passes muito acima da média
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Passes.chave > 40))

# Agora sera feito a análise dos que mais fazem gols
ggplot(aes(x = InStat.Index, y = Gols),data = MLS_C) + 
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'blue') + 
  xlim(200,300) + geom_line(stat = 'summary', fun.y = 'mean', vjust = -1, color ='green') + 
  geom_smooth(method = 'lm',color = 'red')

# Identificando os overlays
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Gols >= 9))

# Correlação de Gols com valor Instat
cor(MLS_C$InStat.Index,MLS_C$Gols)


# A criação de Boxplot dos valores, de gols, assistências, passes chaves e valor Instat seriam interessantes

boxplot(MLS_C$InStat.Index, outline = FALSE, main = 'Boxplot do valor InStat', col = 'red')
boxplot(MLS_C$Gols, main = 'Boxplot dos Gols', col = 'brown')
boxplot(MLS_C$Assistências, main = 'Boxplot das Assistências', col = 'gray')
boxplot(MLS_C$Passes.chave, main = 'Boxplot de Passes Chaves', col = 'yellow')
