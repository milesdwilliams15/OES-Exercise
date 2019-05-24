
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
  ) %>%
  ungroup()

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
df %>% filter(is.na(Yc)==F) %>% summarize(M.20 = sum(Zdesign)/20) # 1.6 
df %>% filter(is.na(Yb)==F) %>% summarize(M.20 = sum(Zdesign)/20) # 1.6
                                          
# Analysis
Y = log(df$Yc)
Z = df$Zdesign
X = with(
  df,
  X4 - ave(X4,by=b)
)
block = as.factor(df$b)


  # Estimate ATE for Yc
mod = lm(Y ~ Z*X + block)

  # Estimate
ATE = coef(mod)['Z']

  # SE
SE = sqrt(diag(BMlmSE(mod)$vcov))['Z']

  # t-value
t_value = ATE/SE

  # p-value
t_sim = c()
set.seed(1234567)
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
mean(abs(t_sim)>= abs(t_value))
p_value

# Summary
out_c = c(ATE,SE,t_value,p_value)
names(out_c) = c("ATE","SE","t.value","p.value")
out_c


# Estimate ATE for Yb
Y = df$Yb
Z = df$Zdesign
X = df$X4 - with(df,ave(X4,b))
block = as.factor(df$b)

# Estimate ATE for Yc
mod = lm(Y ~ Z*X + block)

# Estimate
ATE = coef(mod)['Z']

# SE
SE = sqrt(diag(BMlmSE(mod)$vcov))['Z']

# t-value
t_value = ATE/SE

# p-value
t_sim = c()
set.seed(1234567)
for(i in 1:10000){
  Znew = c(df %>%
             group_by(b) %>%
             mutate(newid = sample(min(id):max(id),size=n())) %>%
             ungroup() %>%
             transmute(Znew = Zdesign[newid]))$Znew
  modnew = lm(Y ~ Znew*X + block)
  ATEnew = coef(modnew)['Znew']
  SEnew = sqrt(diag(sandwich::vcovHC(modnew,type="HC2")))['Znew']
  t_sim[i] = ATEnew/SEnew
}
p_left = mean(t_sim <= t_value,na.rm=T)
p_right = mean(t_sim >= t_value,na.rm=T)
p_value = min(2*min(p_left,p_right),1)

# Summary
out_b = c(ATE,SE,t_value,p_value)
names(out_b) = c("ATE","SE","t.value","p.value")
out_b

# Create Table 1
library(kableExtra)

data.frame(rbind(out_c,out_b)) %>%
  mutate_all(function(x) round(x,3)) %>%
  add_column(Outcome = c("Continuous (ln)","Binary"),.before=T) %>%
  rename("t-statistic"=t.value,"p-value"=p.value) %>%
  kable(
    "latex",
    align="center",
    caption="OLS estimates of treatment effects",
    row.names=F,
    booktabs=T
  ) 

data.frame(rbind(out_c,out_b)) %>%
  add_column(Outcome=c("Continuous (ln)","Binary")) %>%
  ggplot(aes(Outcome,ATE)) +
  geom_point() +
  geom_errorbar(
    aes(ymin=ATE-1.96*SE,
        ymax=ATE+1.96*SE),
    width=0
  ) +
  geom_hline(yintercept=0,linetype=2) +
  theme_classic() +
  labs(y="ATE\n(with 95% Confidence Intervals)") +
  theme(
    text = element_text(family='serif'),
    axis.text = element_text(color="black"),
    axis.title = element_text(size=8)
  ) + 
  ggsave('coefplot.pdf',units='in',height=2,width=4)
