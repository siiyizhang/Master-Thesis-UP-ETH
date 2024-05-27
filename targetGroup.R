# only use species and group column for target group approach
# convert Rdata to csv, which is to be converted to mat file and used in main.m

sp_group = get(load("/net/kryo/work/lixinh/data/Species_Table.RData"))
sp_group = sp_group[,c(2,8)]
write.csv(sp_group, file = "/net/meso/work/siyzhang/dominic/speciesGroup.csv")