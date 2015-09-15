library(faraway)
data(teengamb)
# gambling as the response and the sex, status, income and verbal score as predictors.
teen_lmod <- lm(teengamb$gamble ~ teengamb$sex + teengamb$status + teengamb$income + teengamb$verbal)
summary(teen_lmod)
teen_r.squared <- summary(lmod_teen)$r.squared
#(a)
teen_rsd <- summary(lmod_teen)$residual
#(b)
teen_maxrsd <- match(max(teen_rsd), teen_rsd)
#(c)
mean(teen_rsd)
#[1] -3.065293e-17
median(teen_rsd)
#[1] -1.451392
#(d)
teen_fitted <- lmod_teen$fitted.values
cor(teen_fitted, teen_rsd)
#[1] -1.070659e-16
#(e)
cor(teengamb$income,teen_rsd)
#[1] -7.242382e-17


women <- subset(teengamb, teengamb$sex == 1, rm.na=TRUE)
men <- subset(teengamb, teengamb$sex == 0, rm.na=TRUE)


#===================exersice 4====================

names_pro <-names(prostate)

for(i in c(2:length(names_pro)-1)){
  lmod <- update(lmod , . ~  + prostate[[i]])
  summary(lmod)
  print(lmod$call)
}



varlist <- names(prostate)

for(i in c(2:length(names_pro))){
  models <- lapply(varlist, function(x) {
    lm(substitute(lcavol ~ i, list(i = as.name(x))), data = prostate)
  })
  print(models)
}


models <- lapply(varlist, function(x) {
  lm(substitute(lcavol ~ i, list(x), data = prostate))
})
print(models)

lm(substitute(lcavol ~ i, list(x)), data = prostate)

models <- lapply(varlist, function(x) {
  lm(substitute(lcavol ~ i, list(i = as.name(names(prostate)))), data = prostate)
})
plot(sigma)

#================================================
sigma <-c()
r.square <- c()
for(i in c(1:(length(names_pro)-1))){
  name <- c(names(prostate[1:i]),"lpsa")
  dataset <- subset(prostate,select=name)
  model <- lm(lpsa~.,data = dataset)
  print(summary(model)$sigma)
  sigma <- c(sigma,summary(model)$sigma)
  r.square <- c(r.square,summary(model)$r.square)
}
