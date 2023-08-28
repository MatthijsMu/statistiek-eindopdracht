# Orienterende plots van de financiele variabelen tegen elkaar
# En de logtransformaties:
logsales = log(sales)
logassets = log(assets)
logmarketval = log(marketval)

PLOT_SCATTER <- function(x,y){
  plot(  x=x
       , y=y
       , main = c(deparse(substitute(y)),"tegen",deparse(substitute(x)))
       , xlab = deparse(substitute(x))
       , ylab = deparse(substitute(y))
       , type = 'p'
       , pch = 19
       , col = palette.colors(4)[sector]
       )
  abline(lm(y[which(sector == "Energy")] ~ x[which(sector == "Energy")]), col = palette.colors(4)[1])
  abline(lm(y[which(sector == "Finance")] ~ x[which(sector == "Finance")]), col = palette.colors(4)[2])
  abline(lm(y[which(sector == "Manufacturing")] ~ x[which(sector == "Manufacturing")]), col = palette.colors(4)[3])
  abline(lm(y[which(sector == "Retail")] ~ x[which(sector == "Retail")]), col = palette.colors(4)[4])
  legend("topleft"
         , legend = levels(sector)
         , col = palette.colors(4)
         , pch = c(19,19,19,19)
         )
}

PLOT_SCATTER(logassets, logsales)
PLOT_SCATTER(logmarketval, logsales)
PLOT_SCATTER(assets, sales)
PLOT_SCATTER(marketval, sales)

# Een zeer fancy 3d-plot van de datapunten:

PLOT_3D_SCATTER <- function(x,y,z, scaled){
  plot3D::scatter3D( x
                   , y
                   , z
                   , main = c(deparse(substitute(z)),"tegen",deparse(substitute(x)),"en",deparse(substitute(y)))
                   , xlab = deparse(substitute(x))
                   , ylab = deparse(substitute(y))
                   , zlab = deparse(substitute(z))
                   , type = 'p'
                   , pch = 19
                   , col = palette.colors(4)[sector]
                   , scale = scaled
                   , colkey = FALSE
  )
  legend("topleft"
         , legend = levels(sector)
         , col = palette.colors(4)
         , pch = c(19,19,19,19)
  )
}

PLOT_3D_SCATTER(sales, assets, marketval, FALSE)
PLOT_3D_SCATTER(sales, assets, marketval, TRUE)
PLOT_3D_SCATTER(logsales, logassets, logmarketval, FALSE)
PLOT_3D_SCATTER(logsales, logassets, logmarketval, TRUE)

# Kolgomorov-Smirnovtoets voor normale verdeling van logfinanciele variabelen

# Simulatiefunctie voor een simulatie van N samples uit de nullverdeling
# van "extended" KS-toets (lilliefors-toets?) bij samplegrootte n
NullExtendedKS <- function(N, n){
  D = numeric(N)
  for (i in 1:N){
    X <- rnorm(n)
    D[i] <- ks.test((X - mean(X))/sd(X), pnorm)$statistic
  }
  # Simuleer een steekproef van N realisaties van T* onder de nulhypothese
  # bij een steekproefgrootte van n uit een normale verdeling
  return(D)
}


N <- 100000
n <- 52

Tlogsales <- ks.test((logsales - mean(logsales))/sd(logsales), pnorm)$statistic
Tlogassets <- ks.test((logassets - mean(logassets))/sd(logassets), pnorm)$statistic
Tlogmarketval <- ks.test((logmarketval - mean(logmarketval))/sd(logmarketval), pnorm)$statistic

Tsales <- ks.test((sales - mean(sales))/sd(sales), pnorm)$statistic
Tassets <- ks.test((assets - mean(assets))/sd(assets), pnorm)$statistic
Tmarketval <- ks.test((marketval - mean(marketval))/sd(marketval), pnorm)$statistic

D = NullExtendedKS(N,n) #Simuleer verdeling

# Mooie plots van de nullverdeling
hist(D, probability = TRUE, breaks = 100, main = "Histogram van de nulverdeling van T*", xlab = "T*", ylab = "dichtheid")

lines(c(Tlogsales,Tlogsales), c(0,N), lwd = 2, col = "darkorange")
lines(c(Tlogassets,Tlogassets), c(0,N), lwd = 2, col = "chartreuse")
lines(c(Tlogmarketval,Tlogmarketval), lwd = 2, c(0,N), col = "blue")

lines(c(Tsales,Tsales), c(0,N), lwd = 2, col = "purple")
lines(c(Tassets,Tassets), c(0,N), lwd = 2, col = "magenta")
lines(c(Tmarketval,Tmarketval), c(0,N), lwd = 2, col = "darkred")

# Manier om de p-waarde voor de extended KS-toets te berekenen uit de nullverdeling:
pvalue <- function(t)
{
  return (sum(D>t)/N)
}

pvalue(Tlogsales)
pvalue(Tlogassets)
pvalue(Tlogmarketval)

pvalue(Tsales)
pvalue(Tassets)
pvalue(Tmarketval)



# De gefitte Lineaire modellen:

fit31 = lm(formula = logsales ~ logassets + logmarketval + sector) #Basale model

# F-test tegen model met alleen intercept (intercept-model)
fit0 = lm(formula = logsales ~1)
p0 = 1
p = 6
n = 52
f = (sum(fit0$residuals^2) - sum(fit31$residuals^2))/(sum(fit31$residuals^2))*(n-p)/(p-p0) # F-statistic

pval = 1 - pf(f, p-p0,n-p)
f 
pval

# F-test tegen model met alleen logassets en logmarketval als verklarende variabelen
# dus model zonder categorieen.
fit1 = lm(formula = logsales ~ logmarketval + logassets)
p0 = 3
p = 6
n = 52
f = (sum(fit1$residuals^2) - sum(fit31$residuals^2))/(sum(fit31$residuals^2))*(n-p)/(p-p0) # F-statistic

pval = 1 - pf(f, p-p0,n-p)
f 
pval

# Uitbreiding 3.2: interactie met logassets
fit32 = lm(
  formula = logsales ~ logassets + logmarketval + sector + logassets:sector
  )
# F-toets voor model 3.2
p0 = length(fit31$coefficients)
p = length(fit32$coefficients)
n = length(logsales)

f = (sum(fit31$residuals^2) - sum(fit32$residuals^2))/(sum(fit32$residuals^2))*(n-p)/(p-p0)

pval = 1 - pf(f, p-p0, n-p)
f 
pval

# Uitbreiding 3.3: interactie met logmarketval
fit33 = lm(
  formula = logsales ~ logassets + logmarketval + sector + logmarketval:sector
  )
# F-toets voor model 3.3
p0 = length(fit31$coefficients)
p = length(fit33$coefficients)
n = length(logsales)

f = (sum(fit31$residuals^2) - sum(fit33$residuals^2))/(sum(fit33$residuals^2))*(n-p)/(p-p0)

pval = 1 - pf(f, p-p0, n-p)
f 
pval

# Uitbreiding 3.4: beide interacties van 3.3 en 3.2 in model opgenomen (in de hoop dat dit wel werkt)
fit34 = lm(
  formula = logsales ~ logassets + logmarketval + sector + logassets:sector +logmarketval:sector
)

# F-toets voor model 3.4
p0 = length(fit31$coefficients)
p = length(fit34$coefficients)
n = length(logsales)

f = (sum(fit31$residuals^2) - sum(fit34$residuals^2))/(sum(fit34$residuals^2))*(n-p)/(p-p0)

pval = 1 - pf(f, p-p0, n-p)
f 
pval

# Paragraaf: gevaar van de default t-toetsen: een illustratie van hoe een t-toets handmatig uit
# te voeren en dat dit hetzelfde resultaat geeft.
# Opm: je hebt matlib nodig: install(matlib) om te installeren, library(matlib) om te importeren
fit35 = lm(formula = logsales ~ sector + logmarketval + logassets:sector)

a = c(0,0,0,0,0,-1,1,0,0)

logassetsFinanceMinuslogassetsEnergy = t(a) %*% fit35$coefficients
logassetsFinanceMinuslogassetsEnergy

library(matlib)

t_logassetsFinanceMinuslogassetsEnergy = (a %*% fit35$coefficients)/sqrt(sum(fit35$residuals^2)/(52-9)* (t(a) %*% inv(t(model.matrix(fit35)) %*% model.matrix(fit35)) %*% a))
t_logassetsFinanceMinuslogassetsEnergy


# ongetransformeerde model:
fit8 = lm(formula = sales ~ assets + marketval + sector + sector:assets + sector:marketval)

# De verdeling van de residuen:
# extended KS-test voor normaliteit:
T_KS_8 = ks.test((fit8$residuals - mean(fit8$residuals))/sd(fit8$residuals), pnorm)$statistic
pvalue(T_KS_8)

# Simulatie van de statistiek T:
# eerst de aantallen:
n = 52 #steekproefgrootte
k = 12 #regressieparameters totaal

SSres = sum(fit8$residuals^2) #totale residuele kwadratensom

k_ = c(3,3,3,3) #vector met per index i aantal parameters van submodel (=3)
n_ = numeric(4) #vector waarin steekproefgrootte per submodel komt
SSres_ = numeric(4) #vector waarin SSres(i) per submodel komt

for (l in 1:nlevels(sector)){ #vraag per submodel steekproefgrootte n_ en SSres_ op
  n_[l] = sum(as.numeric(sector) == l)
  SSres_[l] = sum(fit8$residuals[which(as.numeric(sector) == l)]^2)
}

TestStatistic = (n-k)*log(SSres/(n-k)) - sum((n_-k_)*log(SSres_/(n_-k_)))

N = 100000
SimT_ = numeric(N) # N is een constante, aantal simulaties (ergens hierboven gedefinieerd = 100000)
for (i in 1:N){
  X = numeric(4)
  for (l in 1:4){
    X[l] = rchisq(1,df = n_[l] - k_[l]) #simuleer uit chisquare n_i - k_i
  }
  SimT_[i] = (n-k)*log(sum(X)/(n-k)) - sum((n_-k_)*log(X/(n_-k_)))
}

hist( SimT_
      , probability = TRUE
      , breaks = 100
      , main = "Histogram van de nulverdeling van T"
      , xlab = "T"
      , ylab = "dichtheid")

lines(c(TestStatistic,TestStatistic), c(0,N), lwd = 2, col = "blue")

pval = mean(SimT_ > TestStatistic)

pval

# Aanpassingstoetsen uit de laatste paragraaf van dat hoofdstuk 
# Deze toetsen of de conditionele verdelingen van de residuen 
# geconditioneerd naar de sector een enigszins normale verdeling volgen.
# Ook deze toetsen verwerpen voor meerdere sectoren. Daarom is 
# het te betwijfelen of het aangepaste model met groepsheteroskedasticiteit
# eigenlijk wel past bij de data (immers zijn de residuen nog steeds niet normaal verdeeld!)
T_KS_8_ = numeric(4)
for (i in 1:nlevels(sector)){
  T_KS_8_[i] = ks.test((fit8$residuals[which(as.numeric(sector) == i)] 
                        - mean(fit8$residuals[which(as.numeric(sector) == i)]))
                       /sd(fit8$residuals[which(as.numeric(sector) == i)]), pnorm)$statistic
}

pvalue(T_KS_8_[1])
pvalue(T_KS_8_[2])
pvalue(T_KS_8_[3])
pvalue(T_KS_8_[4])

# Mergen van Energy en retail:
mergesector <- sector

levels(mergesector)[levels(mergesector) == "Energy"] <- "Merge"
levels(mergesector)[levels(mergesector) == "Retail"] <- "Merge"
fitMerge = lm(formula = logsales ~ logassets + logmarketval + sector + logmarketval:mergesector)

# Dit pas uitvoeren nadat het eerste model fitMerge gefit is!
levels(mergesector)[levels(mergesector) == "Manufacturing"]<- "Merge"
fitMerge2 = lm(formula = logsales ~ logassets + logmarketval + sector + logmarketval:mergesector) # kunnen we dit nog interpreteren?
