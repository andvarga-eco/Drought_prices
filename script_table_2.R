library(readr)
library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(tibble)
library(plm)

url<-"https://raw.githubusercontent.com/andvarga-eco/Drought_prices/refs/heads/main/milktbl.csv"
milktbl<-read_csv(url)


dependent_var<-"lrprice"
x_vars<-c("drought","factor(month)","wpfull")
cs_vars<-c("droughtcs","l1droughtcs","l2droughtcs","l3droughtcs","l4droughtcs",
           "lrpricecs","l1lrpricecs","l2lrpricecs","l3lrpricecs","l4lrpricecs")
dx_vars<-c("d1drought","l1d1drought","l2d1drought","l3d1drought",
                    "l4d1drought","l5d1drought","l6d1drought")


# Temperature

## Temperature >25 (mtemp > 25)
N_hot<-milktbl|>filter(mtemp>25)|>summarise(N=n_distinct(cmpio))
N_hot<-N_hot$N

theta_hot<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(mtemp>25)|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_hot*(N_hot-1))))^(1/2))
  theta_hot[i,1]<-meantheta$meantheta
  theta_hot[i,2]<-sdtheta$sd
}

## Temperature <25 (mtemp <= 25)
N_cold<-milktbl|>filter(mtemp<=25)|>summarise(N=n_distinct(cmpio))
N_cold<-N_cold$N

theta_cold<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(mtemp<=25)|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_cold*(N_cold-1))))^(1/2))
  theta_cold[i,1]<-meantheta$meantheta
  theta_cold[i,2]<-sdtheta$sd
}

# Regions

N_cent<-milktbl|>filter(region2=="Central")|>summarise(N=n_distinct(cmpio))
N_cent<-N_cent$N

theta_cent<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(region2=="Central")|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_cent*(N_cent-1))))^(1/2))
  theta_cent[i,1]<-meantheta$meantheta
  theta_cent[i,2]<-sdtheta$sd
}

## Región west
N_west<-milktbl|>filter(region2=="West")|>summarise(N=n_distinct(cmpio))
N_west<-N_west$N

theta_west<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(region2=="West")|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_west*(N_west-1))))^(1/2))
  theta_west[i,1]<-meantheta$meantheta
  theta_west[i,2]<-sdtheta$sd
}

## Región Caribbean
N_car<-milktbl|>filter(region2=="Caribbean")|>summarise(N=n_distinct(cmpio))
N_car<-N_car$N

theta_car<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(region2=="Caribbean")|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_car*(N_car-1))))^(1/2))
  theta_car[i,1]<-meantheta$meantheta
  theta_car[i,2]<-sdtheta$sd
}

## Región pacific
N_pac<-milktbl|>filter(region2=="Pacific")|>summarise(N=n_distinct(cmpio))
N_pac<-N_pac$N

theta_pac<-matrix(0,7,2)

for (i in 1:length(dx_vars)) {
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                            paste(predictors,collapse="+")))

  cs_dl<-milktbl|>filter(region2=="Pacific")|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N_pac*(N_pac-1))))^(1/2))
  theta_pac[i,1]<-meantheta$meantheta
  theta_pac[i,2]<-sdtheta$sd
}
