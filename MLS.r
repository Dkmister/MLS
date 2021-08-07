library(readxl)
library(ggplot2)

MLS_C <- read.csv('C:/Users/Vilmar/Documents/MLS/MLS.csv')

head(MLS_C)


summary(MLS_C)

# Verificacao de Assistencia em comparacao ao valor InStat
ggplot(aes(x = InStat.Index, y = Assist�ncias),data = MLS_C) + 
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'black') + 
  xlim(200,300) + geom_line(stat = 'summary', fun.y = 'mean', vjust = -1, color ='green')

# Pode se observar que atletas com muitas assistencias tem uma tend�ncia a estarem mais a direita, ou seja, est�o acima da m�dia
# Verificar se h� poss�vel correla��o

cor(MLS_C$InStat.Index,MLS_C$Assist�ncias)

# Identificando os jogadores com mais assist�ncias e acima da m�dia em ambos fatores
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Assist�ncias >= 6))


# Correla��o de 33%, � um fator de relev�ncia m�dia

# Queremos saber se jogadores com cria��o de jogadas tem influ�ncia na assist�ncia para essa liga

ggplot(aes(x = InStat.Index, y = Passes.chave),data = MLS_C) +
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'blue') +
  xlim(200,300) 


cor(MLS_C$InStat.Index,MLS_C$Passes.chave)

# Cerca de quarenta por cento, parece ser um fator a ser levado em conta


ggplot(aes(x = Assist�ncias, y = Gols),data = MLS_C) + geom_tile()

# Verificando overlays
qplot(x = InStat.Index, data = MLS_C) + scale_x_continuous(breaks = seq(180, 300, 10), limits = c(180, 290) )

# Nome dos jogadores que mais criam jogadas pelo clube, e tem passes muito acima da m�dia
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Passes.chave > 40))

# Agora sera feito a an�lise dos que mais fazem gols
ggplot(aes(x = InStat.Index, y = Gols),data = MLS_C) + 
  geom_point(alpha = 1/20,position = position_jitter(h = 0),color = 'blue') + 
  xlim(200,300) + geom_line(stat = 'summary', fun.y = 'mean', vjust = -1, color ='green') + 
  geom_smooth(method = 'lm',color = 'red')

# Identificando os overlays
qplot(x = Nome, data = subset(MLS_C, InStat.Index > 275 & Gols >= 9))

# Correla��o de Gols com valor Instat
cor(MLS_C$InStat.Index,MLS_C$Gols)


# A cria��o de Boxplot dos valores, de gols, assist�ncias, passes chaves e valor Instat seriam interessantes

boxplot(MLS_C$InStat.Index, outline = FALSE, main = 'Boxplot do valor InStat', col = 'red')
boxplot(MLS_C$Gols, main = 'Boxplot dos Gols', col = 'brown')
boxplot(MLS_C$Assist�ncias, main = 'Boxplot das Assist�ncias', col = 'gray')
boxplot(MLS_C$Passes.chave, main = 'Boxplot de Passes Chaves', col = 'yellow')
