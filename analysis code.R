
# OES data exercise #
#####################

library(tidyverse)

# There are three datasets
  # baseline
base = read_csv('baseline.csv')[-c(153:157),] # Get rid of repeat lines

  # design
desg = read_csv('design.csv')

  # outcomes
outc = read_csv('outcomes.csv')[-c(153:157),] # Get rid of repeat lines

  # Merge
df = left_join(base,desg,by='id') %>%
  left_join(outc,by=c('id','b'))
  
  # Impute missing values for numeric baseline variables
df = df %>%
  group_by(b) %>%
  mutate_at(
    vars(contains('X')),
    function(x) replace_na(x,mean(x,na.rm=T))
  ) 




# Choice of method

## LSDV? Yes. The extreme condition given for SOP is satisfied for 
## 2 blocks:
na.omit(df) %>% 
  group_by(b) %>%
  summarize(
    extreme = 
      n()/ncol(na.omit(df)) >
      20*((n()*mean(Zdesign)*(1-mean(Zdesign)))/
            (ncol(na.omit(df))*mean(na.omit(df)$Zdesign)*(1-mean(na.omit(df)$Zdesign))))
  )

## Y = Z + X + Z*X + block, or Y = Z + X + block? The first as
## the number of subjects assigned to each arm is greater than 20:
sum(df$Zdesign)>=20 # TRUE

## How many covariates can I include in X? No more than:
sum(na.omit(df)$Zdesign)/20 # 1.25 (1 with rounding)

## Which covariate to include in X? The one that best predicts 
## the outcome:
na.omit(df) %>%
  map(~ cor(Yc,))

# Analysis
Y = log(df$Yc)
Z = df$Zdesign
X = data.matrix(df %>% select(contains('X'), - X2, - X3, -contains('Grp')))
vars = matrix(NA,ncol=3,nrow=ncol(X))
for(i in 1:ncol(X)){
  corr = cor(na.omit(data.frame(Y,X[,i])))[1,2]
  vars[i,] = c(i,corr,NA)
}
vars[,3] = rank(abs(vars[,2]))
use_vars = which(vars[,3]==14)
newX = X[,c(use_vars)]
block = as.factor(df$b)


  # Estimate ATE for Yc
mod = lm(Y ~ Z*newX + block)

  # Estimate
ATE = coef(mod)['Z']

  # SE
SE = sqrt(diag(BMlmSE(mod)$vcov))['Z']

  # t-value
t_value = ATE/SE

  # p-value
t_sim = c()
for(i in 1:10000){
  Znew = c(df %>%
    group_by(b) %>%
    mutate(newid = sample(min(id):max(id),size=n())) %>%
    ungroup() %>%
    transmute(Znew = Zdesign[newid]))$Znew
  modnew = lm(Y ~ Znew*newX + block)
  ATEnew = coef(modnew)['Znew']
  SEnew = sqrt(diag(BMlmSE(modnew)$vcov))['Znew']
  t_sim[i] = ATEnew/SEnew
}
p_left = mean(t_sim <= t_value,na.rm=T)
p_right = mean(t_sim >= t_value,na.rm=T)
p_value = min(2*min(p_left,p_right),1)

# Summary
out_c = c(ATE,SE,t_value,p_value)
names(out_c) = c("ATE","SE","t-value","p-value")
out_c


# Estimate ATE for Yb
Y = df$Yb
Z = df$Zdesign
X = data.matrix(df %>% select(contains('X'), - X2, - X3, -contains('Grp')))
vars = matrix(NA,ncol=3,nrow=ncol(X))
for(i in 1:ncol(X)){
  corr = cor(na.omit(data.frame(Y,X[,i])))[1,2]
  vars[i,] = c(i,corr,NA)
}
vars[,3] = rank(abs(vars[,2]))
use_vars = which(vars[,3]==14)
newX = X[,c(use_vars)]
block = as.factor(df$b)

# Estimate ATE for Yc
mod = lm(Y ~ Z*newX + block)

# Estimate
ATE = coef(mod)['Z']

# SE
SE = sqrt(diag(sandwich::vcovHC(mod,type="HC2")))['Z']

# t-value
t_value = ATE/SE

# p-value
t_sim = c()
for(i in 1:10000){
  Znew = c(df %>%
             group_by(b) %>%
             mutate(newid = sample(min(id):max(id),size=n())) %>%
             ungroup() %>%
             transmute(Znew = Zdesign[newid]))$Znew
  modnew = lm(Y ~ Znew*newX + block)
  ATEnew = coef(modnew)['Znew']
  SEnew = sqrt(diag(sandwich::vcovHC(modnew,type="HC2")))['Znew']
  t_sim[i] = ATEnew/SEnew
}
p_left = mean(t_sim <= t_value,na.rm=T)
p_right = mean(t_sim >= t_value,na.rm=T)
p_value = min(2*min(p_left,p_right),1)

# Summary
out_b = c(ATE,SE,t_value,p_value)
names(out_b) = c("ATE","SE","t-value","p-value")
out_b
