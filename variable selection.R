
# Variable Selection #
######################
library(tidyverse)

## Which covariate is most correlated with outcomes?
mf1 = model.frame(Yc ~ ., df %>%
                    select(
                      - X2,
                      -X3,
                      -X8,
                      -X11,
                      -X19,
                      -Yb,
                      -Zdesign,
                      -b
                    ))
mf2 = model.frame(Yb ~ ., df %>%
                    select(
                      - X2,
                      -X3,
                      -X8,
                      -X11,
                      -X19,
                      -Yc,
                      -Zdesign,
                      -b
                    ))
sort(abs(cor(mf1)['Yc',]))[ncol(mf1)-1] # X4 most strongly correlated.
sort(abs(cor(mf2)['Yb',]))[ncol(mf2)-1] # X4 most strongly correlated.

# Use lasso to double check:
library(glmnet)
newdata = df %>%
  ungroup() %>%
  select(
    -X2,
    -X3,
    -X11,
    -X19,
    -b,
    -Zdesign,
    -Yb,
    -id
  ) %>%
  filter(is.na(Yc)==F)
M = model.matrix(~.,newdata)[,-1]
X = M[,-ncol(M)]
y = M[,ncol(M)]
top_vars = matrix(NA,ncol=19,nrow=1000)
for(i in 1:50000){
  lbm = cv.glmnet(x=X,y=log(y),
                  alpha=1)$lambda.min
  gn = glmnet(x=X,y=log(y),
              alpha=1,
              family="gaussian",
              lambda = lbm)
  top_vars[i,] = as.numeric(coef(gn))
}
colnames(top_vars) = rownames(coef(gn))

# Which variables made the cut most often?
data.frame(top_vars) %>%
  summarise_all(function(x) mean(x!=0)) # Two! X4 and X9

# I can use only 1. Compare models:
f1 = lm(y ~ 1)
f2 = lm(y ~ X[,'X4'])
f3 = lm(y ~ X[,'X9'])
lmtest::waldtest(f1,f2) # X4 Significantly improves fit.
lmtest::waldtest(f1,f3) # X9 does not.

# Use X4 for Yc


newdata = df %>%
  ungroup() %>%
  select(
    -X2,
    -X3,
    -X11,
    -X19,
    -b,
    -Zdesign,
    -Yc,
    -id
  ) %>%
  filter(is.na(Yb)==F)
M = model.matrix(~.,newdata)[,-1]
X = M[,-ncol(M)]
y = M[,ncol(M)]
top_vars = matrix(NA,ncol=19,nrow=1000)
for(i in 1:50000){
  lbm = cv.glmnet(x=X,y=y,
                  alpha=1)$lambda.min
  gn = glmnet(x=X,y=y,
              alpha=1,
              family="binomial",
              lambda = lbm)
  top_vars[i,] = as.numeric(coef(gn))
}
colnames(top_vars) = rownames(coef(gn))

# Which variables made the cut most often?
data.frame(top_vars) %>%
  summarise_all(function(x) mean(x!=0)) # Several
vars = data.frame(top_vars) %>%
  summarise_all(function(x) mean(x!=0))
vars = which(vars==1)[-1]-1
fits = list()
f1 = lm(y ~ 1)
for(i in 1:length(vars)){
  x = X[,vars[i]]
  mod = lm(y ~ x)
  fits[[i]] = lmtest::waldtest(f1,mod)
}

# Use X4 for Yb
