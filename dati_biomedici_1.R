if(!require(pacman)) install.packages('pacman', dependencies = T)

pacman::p_load(glmnet,
               lars,
               covTest,
               ElemStatLearn,
               tableone,
               magrittr,
               knitr,
               dplyr,
               here,
               readxl,
               tidyverse)

wdir <- here::here()
input_data <- file.path(wdir, 'data')

db <- read_excel(file.path(input_data,'liver_forThesis.xlsx'))

varnames <- c( 
              'Sex',
              'RangoOLT',
              'BMI',
              'MeldOLT',
              'CreaOLTreal',
              'Dialysis',
              'PrevAbdSurg',
              'LifeSupp',
              'Ascites',
              'PortalThromb',
              'HCC',
              'AgeD',
              'BMID',
              'SteaMacro',
              'D-MELD',
              'BAR',
              'DRI',
              'CIT',
              'RWIT',
              'GraftWeight',
              'GRWR',
              'EAD_Olthoff')

db <- db %>% 
  dplyr::select(varnames)

my_function_01 <- function(x){
  ifelse(x=='no',0,ifelse(x=='yes',1,NA))
}

varno <- c('Dialysis','PrevAbdSurg','LifeSupp','Ascites','PortalThromb', 'EAD_Olthoff', 'HCC')

db <- db %>% 
  mutate_at(vars(varno), funs(my_function_01))


# faccio un'analisi descrittiva per alcune variabili
# sex
db$Sex<-as.factor(db$Sex)
barplot(table(db$Sex),col=(c("red","blue")))
summary(db$Sex)

#ageD
hist(db$AgeD)
summary(db$AgeD)
var(db$AgeD)
sd(db$AgeD)

#ead_olthoff
db$EAD_Olthoff<-as.factor(db$EAD_Olthoff)
barplot(table(db$EAD_Olthoff), col=(c("chartreuse2","goldenrod1")))
summary(db$EAD_Olthoff)


db$BMID<-as.numeric(db$BMID)
db$SteaMacro<-as.numeric(db$SteaMacro)
db$CIT<-as.numeric(db$CIT)
db$RWIT<-as.numeric(db$RWIT)
db$RangoOLT<-as.factor(db$RangoOLT)
db$Dialysis<-as.factor(db$Dialysis)
db$PrevAbdSurg<-as.factor(db$PrevAbdSurg)
db$LifeSupp<-as.factor(db$LifeSupp)
db$Ascites<-as.factor(db$Ascites)
db$PortalThromb<-as.factor(db$PortalThromb)
db$EAD_Olthoff<-as.factor(db$EAD_Olthoff)
db$HCC<-as.factor(db$HCC)


# tolgo i dati mancanti
db<-na.omit(db)


# creo una tebella riassuntiva
CreateTableOne(vars = varnames,
               data = db) %>%
  print(printToggle=F) %>% kable()


# definisco la variabile risposta y 
y <- db$EAD_Olthoff 

# definisco le variabili predittive x
x1<-(db[,1:(ncol(db)-1)])
head(x1)
x <- model.matrix(y~. ,data = x1)
z=scale(x,FALSE,FALSE)

# seleziono i valori di lambda
set.seed(1230)
fit_lasso<-cv.glmnet(z,y,family="binomial")
fit_lasso

# stimo i coefficienti di regressione
coef(fit_lasso, s=fit_lasso$lambda.min)
coef(fit_lasso, s=fit_lasso$lambda.1se)
RegCoef=glmnet(z,y,family = "binomial",alpha = 1)

# stampo MSE
plot(fit_lasso)

# stampo i coefficienti
plot(RegCoef, xvar="lambda", lwd=1.8 )
abline(v=log(fit_lasso$lambda.1se))
abline(v=log(fit_lasso$lambda.min))


# studio il p-value di lambda
df=nrow(x1)-1
x2<-x1

x2$RangoOLT<-as.numeric(x2$RangoOLT)
x2$Dialysis<-as.numeric(x2$Dialysis)
x2$PrevAbdSurg<-as.numeric(x2$PrevAbdSurg)
x2$LifeSupp<-as.numeric(x2$LifeSupp)
x2$Ascites<-as.numeric(x2$Ascites)
x2$PortalThromb<-as.numeric(x2$PortalThromb)
x2$HCC<-as.numeric(x2$HCC)
y<-as.numeric(y)

p<-(x2[,(2:ncol(x2))])
head(p)

t=scale(p, center = TRUE, scale = TRUE)/sqrt(df)

# eseguo test di covarianza per testare la significatività delle variabili
LarsCoef=lars(t,y) 
LarsCoef
plot(LarsCoef)
summary(LarsCoef)
a<-covTest(LarsCoef,t,y)
a

