library(readr)
library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(tibble)
library(plm)


url<-"https://raw.githubusercontent.com/andvarga-eco/Drought_prices/refs/heads/main/milktbl.csv"
milktbl<-read_csv(url)

# DL Model

N<-n_distinct(milktbl$cmpio)
T<-n_distinct(milktbl$date)
pz<-round(T^(1/3),0)


dependent_var<-"lrprice"
x_vars<-c("drought","factor(month)")
dx_vars<-c("d1drought","l1d1drought","l2d1drought","l3d1drought",
                    "l4d1drought","l5d1drought","l6d1drought")
theta_nocs<-matrix(0,7,2)
pcd_results_nocs<-vector("list",length(dx_vars))

for (i in 1:length(dx_vars)) {
  # Select the first i predictors
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",
                          paste(predictors,collapse="+")))

  dl<-milktbl|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  dl_coefs<-dl|>select(cmpio,tidy)|>unnest(tidy)
  dl_coefs<-dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N*(N-1))))^(1/2))
  theta_nocs[i,1]<-meantheta$meantheta
  theta_nocs[i,2]<-sdtheta$sd

  # Extract residuals and run CD test via pseries
  resid_long<-dl|>
    mutate(resid_df=map2(data,fit,~{
      idx<-as.integer(rownames(model.frame(.y)))
      resids<-rep(NA_real_,nrow(.x))
      resids[idx]<-residuals(.y)
      tibble(date=.x$date,.resid=resids)
    }))|>
    select(cmpio,resid_df)|>unnest(resid_df)
  pdata_resid<-pdata.frame(resid_long,index=c("cmpio","date"))
  pcd_results_nocs[[i]]<-pcdtest(pdata_resid$.resid)
}

pcd_nocs_summary<-data.frame(
  lags   =paste0("p=",1:7),
  z_stat =sapply(pcd_results_nocs,function(x) unname(x$statistic)),
  p_value=sapply(pcd_results_nocs,function(x) x$p.value)
)

# CS-DL model(1)

dependent_var<-"lrprice"
x_vars<-c("drought","factor(month)")
cs_vars<-c("droughtcs","l1droughtcs","l2droughtcs","l3droughtcs","l4droughtcs",
           "lrpricecs","l1lrpricecs","l2lrpricecs","l3lrpricecs","l4lrpricecs")
dx_vars<-c("d1drought","l1d1drought","l2d1drought","l3d1drought",
                    "l4d1drought","l5d1drought","l6d1drought")
theta_m1<-matrix(0,7,2)
pcd_results_m1<-vector("list",length(dx_vars))

for (i in 1:length(dx_vars)) {
  # Select the first i predictors
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                          paste(predictors,collapse="+")))

  cs_dl<-milktbl|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N*(N-1))))^(1/2))
  theta_m1[i,1]<-meantheta$meantheta
  theta_m1[i,2]<-sdtheta$sd

  # Extract residuals and run CD test via pseries
  resid_long<-cs_dl|>
    mutate(resid_df=map2(data,fit,~{
      idx<-as.integer(rownames(model.frame(.y)))
      resids<-rep(NA_real_,nrow(.x))
      resids[idx]<-residuals(.y)
      tibble(date=.x$date,.resid=resids)
    }))|>
    select(cmpio,resid_df)|>unnest(resid_df)
  pdata_resid<-pdata.frame(resid_long,index=c("cmpio","date"))
  pcd_results_m1[[i]]<-pcdtest(pdata_resid$.resid)
}
pcd_m1_summary<-data.frame(
  lags   =paste0("p=",1:7),
  z_stat =sapply(pcd_results_m1,function(x) unname(x$statistic)),
  p_value=sapply(pcd_results_m1,function(x) x$p.value)
)

# CS-DL model (2)

dependent_var<-"lrprice"
x_vars<-c("drought","factor(month)","wpfull")
cs_vars<-c("droughtcs","l1droughtcs","l2droughtcs","l3droughtcs","l4droughtcs",
           "lrpricecs","l1lrpricecs","l2lrpricecs","l3lrpricecs","l4lrpricecs")
dx_vars<-c("d1drought","l1d1drought","l2d1drought","l3d1drought",
                    "l4d1drought","l5d1drought","l6d1drought")
theta_m2<-matrix(0,7,2)
pcd_results_m2<-vector("list",length(dx_vars))

for (i in 1:length(dx_vars)) {
  # Select the first i predictors
  predictors <- dx_vars[1:i]
  formula<-as.formula(paste(dependent_var,"~",paste(x_vars,collapse="+"),"+",paste(cs_vars,collapse="+"),"+",
                          paste(predictors,collapse="+")))

  cs_dl<-milktbl|>group_by(cmpio)|>group_nest()|>
    mutate(fit=map(data,~lm(formula,data=.x)),
           tidy=map(fit,broom::tidy),
           glan=map(fit,broom::glance))
  cs_dl_coefs<-cs_dl|>select(cmpio,tidy)|>unnest(tidy)
  cs_dl_coefs<-cs_dl_coefs|>select(c(cmpio,term,estimate))|>pivot_wider(names_from=term, values_from = "estimate")
  meantheta<-cs_dl_coefs|>summarise(meantheta=mean(drought,na.rm=TRUE))
  sdtheta<-cs_dl_coefs|>summarise(sd=(sum((drought-meantheta$meantheta)^2)*(1/(N*(N-1))))^(1/2))
  theta_m2[i,1]<-meantheta$meantheta
  theta_m2[i,2]<-sdtheta$sd

  # Extract residuals and run CD test via pseries
  resid_long<-cs_dl|>
    mutate(resid_df=map2(data,fit,~{
      idx<-as.integer(rownames(model.frame(.y)))
      resids<-rep(NA_real_,nrow(.x))
      resids[idx]<-residuals(.y)
      tibble(date=.x$date,.resid=resids)
    }))|>
    select(cmpio,resid_df)|>unnest(resid_df)
  pdata_resid<-pdata.frame(resid_long,index=c("cmpio","date"))
  pcd_results_m2[[i]]<-pcdtest(pdata_resid$.resid)
}
pcd_m2_summary<-data.frame(
  lags   =paste0("p=",1:7),
  z_stat =sapply(pcd_results_m2,function(x) unname(x$statistic)),
  p_value=sapply(pcd_results_m2,function(x) x$p.value)
)

