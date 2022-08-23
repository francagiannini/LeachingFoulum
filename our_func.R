library(tidyverse)
library(readxl)

beh <- read_excel("beh.xlsx")
View(beh)

dfN <- beh

breaks_date <- dfN$date[!is.na(dfN$conc)]

dfN_spl <- dfN |> mutate(period=cut(date, dfN$date[!is.na(dfN$conc)])) |> 
  group_split(period) 


period_list <- list()

for(i in 2:length(dfN_spl)-1){
  p=as.data.frame(dfN_spl[[i]])
  p1=as.data.frame(dfN_spl[[i+1]][1,])
  period_list[[i]] <- rbind(p, p1) |> mutate(index = row_number())
}

ci_func <- function(i, p = period_list, id) {
  #browser()
  #id <- as.numeric(rownames(i))
  
  cat("Mi ID es", id, "\n")
  # cat(rawToChar(as.raw(c(0x4c, 0x61, 0x20, 0x51, 0x75, 0x65, 0x72, 0x65, 0x6d, 
  #                        0x6f, 0x73, 0x20, 0x61, 0x20, 0x46, 0x72, 0x61, 0x6e, 0x63, 0x61
  # ))), "\n")
  
  C_a <- p[1, "conc"]
  C_b <- p[nrow(p), "conc"]
  
  D_a <- p[1, "drain"]
  D_b <- p[nrow(p), "drain"]
  
  j <- id
  B <- nrow(p)
  
  # if (D_b > 0) {
  
  D_i <- p[id, "drain"]
  
  f_Di <- ((D_a / 2) + sum(p[2:(id - 1), "drain"]) + (D_i / 2)) /
    ((D_a / 2) + sum(p[2:(B - 1), "drain"]) + (D_b / 2))
  
  f_Di <- ifelse(is.na(f_Di),0,f_Di)
  
  C_i = C_a + (C_b - C_a) * f_Di
  
  data.frame(id, C_i)
  
  # } else {
  # Pensar si esta bien
  # ((C_b - C_a) / B) * 
  # }
  
}

# apply(p[3:nrow(period_list[[3]])-1], 1, ci_func)
ci_func_df <- function(period_list_df) {
  sapply(2:(nrow(period_list_df) - 1), function(x, p) {
    ci_func(p[x], p, id = x)
    
  }, p = period_list_df)
}


lapply(period_list, ci_func_df)


period_list[[2]] |> mutate(
  ci=ifelse(
    is.na(conc),
    ci_func(),#aaca
    conc))