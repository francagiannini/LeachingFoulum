##some explore

dfN |> 
  ggplot(aes(y=conc,x=drain, col=as.factor(beh)))+
  geom_point()+
  theme_bw()

dfN |> group_by(beh) |> 
  mutate(cumdrain=cumsum(drain)) |> 
  ggplot(aes(y=conc,x=cumdrain #, col=as.factor(beh), fill=as.factor(beh)
             ))+
  geom_point(aes(col=as.factor(beh))) +
  geom_smooth() +
  theme_bw()

dfN |> group_by(beh) |> 
  mutate(cumdrain=cumsum(drain)) |> 
  ggplot(aes(y=conc,x=cumdrain #, col=as.factor(beh), fill=as.factor(beh)
  ))+
  geom_point(aes(col=as.factor(beh))) +
  geom_smooth() +
  #facet_grid(beh~.)+
  theme_bw()

dfN |> group_by(beh) |> 
  mutate(cumdrain=cumsum(drain)) |>
  pivot_longer(cols=c(conc,nweconc), 
               names_to = "interpol", values_to = "Concentration") |> 
  # group_by(beh) |> 
  # filter(row_number(beh) > 5) |>
  ggplot(aes(y=Concentration,x=cumdrain , 
             col=as.factor(interpol)#, fill=as.factor(interpol
             ))+
  geom_point(aes(col=as.factor(beh))) +
  geom_smooth(aes(col=as.factor(beh))) +
  facet_wrap(~beh)+
  theme_bw()

library(lme4)

nlm_gau=nlme(conc~je+alfa/(1+exp(-(cumdrain-beta)/gamma))
                             ,data=dfN
                             ,fixed=list(je~1
                                         ,alfa~1
                                         ,beta~1
                                         ,gamma~1)
                             ,random=list(je~1
                                          ,beta~1
                                          #,gamma~1
                                          )
                             ,groups=~beh
                             ,start=c(0,47,120,47)
                             ,na.action=na.omit
                             ,method="REML")

plot(nlm_gau)

summary(nlm_gau)

dfN$concgau <- predict(nlm_gau,dfN)

dfN |> 
  pivot_longer(cols=c(concgau,nweconc,conc), 
               names_to = "interpol", values_to = "Concentration") |> 
  # group_by(beh) |> 
  # filter(row_number(beh) > 5) |>
  ggplot(aes(y=Concentration,x=cumdrain , 
             col=as.factor(interpol)#, fill=as.factor(interpol
  ))+
  geom_point(aes(col=as.factor(interpol))) +
  #geom_line()+
  #geom_smooth(aes(col=as.factor(beh))) +
  facet_wrap(~beh)+
  theme_bw()
