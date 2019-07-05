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

varnames <- c('AnniOLT',
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
              'IndiOLT',
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

CreateTableOne(vars = varnames,
               data = db) %>%
  print(printToggle=F) %>% kable()


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

# considero la variabile indiolt come categorica
db$IndiOLT<-as.factor(db$IndiOLT)

db$BMID<-as.numeric(db$BMID)
db$SteaMacro<-as.numeric(db$SteaMacro)
db$CIT<-as.numeric(db$CIT)
db$RWIT<-as.numeric(db$RWIT)


db$Dialysis<-as.factor(db$Dialysis)
db$PrevAbdSurg<-as.factor(db$PrevAbdSurg)
db$LifeSupp<-as.factor(db$LifeSupp)
db$Ascites<-as.factor(db$Ascites)
db$PortalThromb<-as.factor(db$PortalThromb)
db$EAD_Olthoff<-as.factor(db$EAD_Olthoff)
db$HCC<-as.factor(db$HCC)



# tolgo i dati mancanti
db<-na.omit(db)


# definisco la variabile risposta y 
y <- db$EAD_Olthoff 

# definisco le variabili predittive x
x<-(db[,1:(ncol(db)-1)])
head(x)
x <- model.matrix(y~. ,data = x)
z=scale(x,FALSE,FALSE)

# seleziono i valori di lambda
fit_lasso<-cv.glmnet(z,y,family="binomial")
fit_lasso

# stimo i coefficienti di regressione
coef(fit_lasso, s=fit_lasso$lambda.1se)
RegCoef=glmnet(z,y,family = "binomial",alpha = 1)

plot(fit_lasso)

plot(RegCoef, xvar="lambda", xlim=c(-10,-2), lwd=1.8 )
abline(v=log(fit_lasso$lambda.1se))
abline(v=log(fit_lasso$lambda.min))


# studio il p-value di lambda
df=nrow(x)-1
p<-(x[,2:(ncol(x))])

t=scale(p, center = TRUE, scale = TRUE)/sqrt(df)

# eseguo test di covarianza per testare la significatività delle variabili
LarsCoef=lars(t,y) # questo mi da errore
LarsCoef
plot(LarsCoef)
summary(LarsCoef)
a<-covTest(LarsCoef,IV2,y)
a

