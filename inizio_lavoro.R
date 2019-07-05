if(!require(pacman)) install.packages('pacman', dependencies = T)

pacman::p_load(dplyr,
               here,
               magrittr,
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
              'EAD_Olthoff',
              'PeakALT')

db <- db %>% 
  dplyr::select(varnames)

my_function_01 <- function(x){
  ifelse(x=='no',0,ifelse(x=='yes',1,NA))
}

varno <- c('Dialysis','PrevAbdSurg','LifeSupp','Ascites','PortalThromb', 'EAD_Olthoff', 'HCC')

db <- db %>% 
  mutate_at(vars(varno), funs(my_function_01))


