require(tidyverse)
require(readxl)
require(scales)
require(matlab)

number.change <- c()
num.sim <- 100
percent.change <- seq(from = 0, to = 0.30, length.out = num.sim)

div. <- c("DMS","COA")[1]
# allo. <- c(54022, 44827)

# if (div. == "DMS") { total.allocation <-  allo.[1] }
# if (div. == "COA") { total.allocation <-  allo.[2] }

in.dat <- readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1]) %>% select(-c(`Years in Rank`, `Salary + Admin Stipend`))
rank.df <- matrix(nrow = c(in.dat %>% filter(`Division` == div.) %>% dim())[1], ncol = num.sim)

for (z in 1:num.sim) {
  in.dat <- readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1]) %>% select(-c(`Years in Rank`, `Salary + Admin Stipend`))
  
  tic()
  
  allocation <- readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1], sheet = "Allocation")
  allocation$Research <- allocation$Research/100
  allocation$Teaching <- allocation$Teaching/100
  allocation$Service <- allocation$Service/100
  
  allocation$Research <- runif(n = dim(allocation)[1], 
                               min = c(1 - percent.change[z])*allocation$Research, 
                               max = c(1 + percent.change[z])*allocation$Research)
  allocation$Teaching <- runif(n = dim(allocation)[1], 
                               min = c(1 - percent.change[z])*allocation$Teaching, 
                               max = c(1 + percent.change[z])*allocation$Teaching)
  allocation$Service <- 1 - rowSums(cbind(allocation$Research, allocation$Teaching))
  
  allocation[,c(3:5)] <- abs(allocation[,c(3:5)])
  divis. <- rowSums(allocation[,3:5])
  allocation[,3] <- allocation[,3]/divis.
  allocation[,4] <- allocation[,4]/divis.
  allocation[,5] <- allocation[,5]/divis.
  
  mean.sal <- in.dat %>% group_by(Division, Rank, Sub) %>% summarise(mean.sal = mean(Salary))
  in.dat <- in.dat %>% left_join(mean.sal)
  rm(mean.sal)
  
  in.dat <- in.dat %>% mutate(sal.dev = Salary - mean.sal)
  
  in.dat <- left_join(in.dat, allocation)
  ind <- c(1,2,3)
  
  in.dat.Teach <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), cbind(in.dat %>% select(starts_with("Teaching "))*in.dat$Teaching)[,ind])
  in.dat.Research <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), cbind(in.dat %>% select(starts_with("Research "))*in.dat$Research)[,ind])
  in.dat.Service <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), cbind(in.dat %>% select(starts_with("Service "))*in.dat$Service)[,ind])
  
  # Anonymize the data set
  in.dat <- in.dat %>% select(`W#`, Division, Sub, Rank,`Start Date`,sal.dev,Salary)
  
  comp. <- rowSums(cbind(rowMeans(in.dat.Teach[,5:7],na.rm = T),
                         rowMeans(in.dat.Research[,5:7],na.rm = T),
                         rowMeans(in.dat.Service[,5:7],na.rm = T)), 
                   na.rm = T)
  
  in.dat <- cbind(in.dat, comp.)
  
  dat. <- in.dat %>% filter(Division == div.) %>% arrange(desc(comp.))
  # dat.$Percent.Increase <- dat.$Update.Salary <- NA
  # 
  # dim(dat.)[1]
  
  
  # if (div. == "DMS") {
  #   tier.size <- c(ceiling((dim(dat.)[1]-1)*0.30),
  #                  dim(dat.)[1] - floor(dim(dat.)[1]*0.20)-ceiling(dim(dat.)[1]*0.30) + 2,
  #                  ceiling(dim(dat.)[1]*0.100)) }
  # 
  # if (div. == "COA") {
  #   tier.size <- c(ceiling((dim(dat.)[1]-1)*0.30),
  #                  dim(dat.)[1] - floor(dim(dat.)[1]*0.20)-ceiling(dim(dat.)[1]*0.30) + 2,
  #                  ceiling(dim(dat.)[1]*0.100)) 
  #   tier.size[2] <- 8}
  # 
  # evaluate scores in tiers
  # dat. <- dat. %>% arrange(desc(comp.))
  # dat.$comp.[1:tier.size[3]]
  # dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])]
  # dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])]
  # 
  # dat.$Percent.Increase[1:tier.size[1]] <- scales::rescale(dat.$comp.[1:tier.size[1]], to = c(0.051, 0.099))
  # dat.$Percent.Increase[seq(from = tier.size[1]+1, length.out = tier.size[2])] <- 
  #   scales::rescale(dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])], to = c(0.030, 0.050))
  # dat.$Percent.Increase[seq(from = dim(dat.)[1],by = -1, length.out = tier.size[3])] <- 
  #   scales::rescale(dat.$comp.[seq(from = dim(dat.)[1],by = -1, length.out = tier.size[3])], to = c(0.010, 0.030))
  # dat.$Update.Salary <- dat.$Salary*dat.$Percent.Increase
  
  # j <- 0.001
  # test.val <- 10^10
  # while (test.val > total.allocation) {
  #   
  #   dat.$Percent.Increase[1:tier.size[1]] <- scales::rescale(dat.$comp.[1:tier.size[1]], to = c(0.051, max(0.075 - j,0.052)))
  #   dat.$Percent.Increase[seq(from = tier.size[1]+1, length.out = tier.size[2])] <- scales::rescale(dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])], to = c(0.030, max(0.050 - j,0.035)))
  #   dat.$Percent.Increase[seq(from = c(max(seq(from = tier.size[1]+1, length.out = tier.size[2])) + 1), by = 1, length.out = tier.size[3])] <- 
  #     scales::rescale(dat.$comp.[seq(from = c(max(seq(from = tier.size[1]+1, length.out = tier.size[2])) + 1), by = 1, length.out = tier.size[3])], to = sort(c(0.02, max(0.030 - j,0.010))))
  #   
  #   dat.$comp. <- round(dat.$comp.,3)
  #   Percent.Increase <- dat. %>% group_by(`comp.`) %>% summarize(Percent.Increase = mean(Percent.Increase))
  #   dat. <- dat. %>% select(-`Percent.Increase`)
  #   dat. <- left_join(dat., Percent.Increase)
  #   dat.$Update.Salary <- (dat.$Salary*dat.$Percent.Increase)
  #   j = j + 0.0001
  #   
  #   test.val <- sum(dat.$Update.Salary)
  #   print(sum(dat.$Update.Salary))
  # }
  # 
  
  # write.csv(left_join(dat., readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1]) %>% select(Name, `W#`, Division,  Sub, Rank)),
  #          file = paste0("test.",div.,".csv"))
  
  # dat. <- dat. %>% arrange(`comp.`)
  rank.df[,z] <- dat.$`W#`
  
  number.change <- c(number.change, 
                     which(!(match(rank.df[,1],rank.df[,2]) == seq(1:17))) %>% length())
  
  toc()
}

plot(percent.change, number.change)

