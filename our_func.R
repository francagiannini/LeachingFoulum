
# Packages used
library(tidyverse)
library(readxl)
library(lubridate)

# Read table and identification of observations 

dfN <- read_excel("data/Flakke_DrainAPSIM_2016_17.xlsx") |> 
  unique() |> 
  mutate(idx=seq(1,n(),1))

# Spliting table between interpolation periods
# Te result is a list of data frames eac element will be one period 

dfN_spl <- dfN |> 
  mutate(
    drain=if_else(drain<0,0,drain),
    period=
     cut(as.numeric(idx), dfN$idx[!is.na(dfN$conc)],
         labels=FALSE,
         include.lowest = TRUE,
         right=FALSE)
      ) |>
  group_split(period) 


period_list <- list()

for(i in 1:length(dfN_spl)){
  
  p<-as.data.frame(dfN_spl[[i]])
  
  pfin<- as.data.frame(
    dfN_spl[[ifelse(unique(p$period)==length(dfN_spl),i,i+1)]][ifelse(unique(p$period)==length(dfN_spl),nrow(p),1),])
  
  period_list[[i]] <- rbind(p, pfin) |>
    mutate(index = row_number()) |> unique()
}


# The interpolation function it runs for each period by row 
# calculating a nwe concentration for each row 

ci_func <- function(i, p = period_list, id) {
  #browser()
  
  cat("The ID is", id, "\n")
  cat("The period is", unique(p$period), "\n")
  
  C_a <- p[1, "conc"]
  C_b <- p[nrow(p), "conc"]
  
  D_a <- p[1, "drain"]
  D_b <- p[nrow(p), "drain"]
  
  B <- nrow(p)
  
  D_i <- p[id, "drain"]
  
  f_Di <- ((D_a / 2) + sum(p[2:(id - 1), "drain"]) + (D_i / 2)) /
    ((D_a / 2) + sum(p[2:(B - 1), "drain"]) + (D_b / 2))
  
  f_Di <- ifelse(is.na(f_Di),0,f_Di)
  
  C_i = C_a + (C_b - C_a) * f_Di
  
  res = tibble('beh'=p[id,'beh'],
               'idx'=p[id,'idx'],
               'C_i'=ifelse(is.na(p[id,'conc']), C_i,p[id,'conc'])
               )
  
  return(res)
  
}

# Function to aply the period function to the list of periods

ci_func_df <- function(period_list_df) {
  
  lapply(1:(nrow(period_list_df)), function(x, p) {
    
    ci_func(p[x], p, id = x)
  
    }, p =  as.data.frame(period_list_df)
    )
  
}

# Running the interpolation 

interpol <- do.call(bind_rows, lapply(period_list, ci_func_df)) |> 
  distinct(idx,.keep_all = TRUE)

# Writing the nwe concentration
dfN$nweconc <- interpol$C_i

# Ploting the results 

ggplot(dfN, 
       aes(x= date, y=nweconc, color=as.factor(beh)))+
  geom_line()+
  geom_point()+
  theme_bw()


# long_df <- long_df |>  
#   group_by(beh, interpolation_method) |>  
#   arrange(date) |>
#   mutate(integrated_outflux= cumsum(drain*interp_conc)) 
# 
# ggplot(filter(long_df, beh==73), 
#        aes(x=date, y=interp_conc*drain, 
#            color=interpolation_method))+
#   facet_wrap(~beh)+
#   geom_line()+
#   geom_smooth()
