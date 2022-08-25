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
  pivot_longer(cols=c(conc,nwecoc), 
               names_to = "interpol", values_to = "Concentration") |> 
  # group_by(beh) |> 
  # filter(row_number(beh) > 5) |>
  ggplot(aes(y=Concentration,x=cumdrain , 
             col=as.factor(interpol), fill=as.factor(interpol)
  ))+
  geom_point(#aes(col=as.factor(beh))
             ) +
  geom_smooth() +
  #facet_wrap(~beh)+
  theme_bw()


dfN |> group_by(beh) |> 
  mutate(cumdrain=cumsum(drain)) |>
  ggplot(aes())