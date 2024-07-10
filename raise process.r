require(tidyverse)
require(readxl)
require(scales)

div. <- c("DMS","COA")[2]
allo. <- c(54022, 44827)

in.dat <- readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1])
in.dat <- in.dat %>% select(-c(`Years in Rank`, `Salary + Admin Stipend`))
in.dat %>% glimpse()

allocation <- readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1], sheet = "Allocation")
allocation$Research <- allocation$Research/100
allocation$Teaching <- allocation$Teaching/100
allocation$Service <- allocation$Service/100

mean.sal <- in.dat %>% group_by(Division, Rank, Sub) %>% summarise(mean.sal = mean(Salary))
in.dat <- in.dat %>% left_join(mean.sal)
rm(mean.sal)

in.dat <- in.dat %>% mutate(sal.dev = Salary - mean.sal)

in.dat <- left_join(in.dat, allocation)

in.dat.Teach <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), in.dat %>% select(starts_with("Teaching "))*in.dat$Teaching)
in.dat.Research <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), in.dat %>% select(starts_with("Research "))*in.dat$Research)
in.dat.Service <- bind_cols(in.dat %>% select(`W#`,`Division`,`Sub`,`Rank`), in.dat %>% select(starts_with("Service "))*in.dat$Service)

# Anonymize the data set
in.dat <- in.dat %>% select(`W#`, Division, Sub, Rank,`Start Date`,sal.dev,Salary)

comp. <- rowSums(cbind(rowMeans(in.dat.Teach[,5:7],na.rm = T),
                       rowMeans(in.dat.Research[,5:7],na.rm = T),
                       rowMeans(in.dat.Service[,5:7],na.rm = T)), 
                 na.rm = T)

in.dat <- cbind(in.dat, comp.)

dat. <- in.dat %>% filter(Division == div.) %>% arrange(desc(comp.))
dat.$Percent.Increase <- dat.$Update.Salary <- NA

dim(dat.)[1]

if (div. == "DMS") {
tier.size <- c(ceiling((dim(dat.)[1]-1)*0.30),
               dim(dat.)[1] - floor(dim(dat.)[1]*0.20)-ceiling(dim(dat.)[1]*0.30) + 2,
               ceiling(dim(dat.)[1]*0.100)) }

if (div. == "COA") {
  tier.size <- c(ceiling((dim(dat.)[1]-1)*0.30),
                 dim(dat.)[1] - floor(dim(dat.)[1]*0.20)-ceiling(dim(dat.)[1]*0.30) + 2,
                 ceiling(dim(dat.)[1]*0.100)) 
  tier.size[2] <- 8}


# while loop on tier size

# evaluate scores in tiers
dat. <- dat. %>% arrange(desc(comp.))
dat.$comp.[1:tier.size[3]]
dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])]
dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])]

if (div. == "DMS") {
total.allocation <-  allo.[1] }
if (div. == "COA") {
  total.allocation <-  allo.[2] }

dat.$Percent.Increase[1:tier.size[1]] <- scales::rescale(dat.$comp.[1:tier.size[1]], to = c(0.051, 0.099))
dat.$Percent.Increase[seq(from = tier.size[1]+1, length.out = tier.size[2])] <- 
  scales::rescale(dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])], to = c(0.030, 0.050))
dat.$Percent.Increase[seq(from = dim(dat.)[1],by = -1, length.out = tier.size[3])] <- 
  scales::rescale(dat.$comp.[seq(from = dim(dat.)[1],by = -1, length.out = tier.size[3])], to = c(0.010, 0.030))
dat.$Update.Salary <- dat.$Salary*dat.$Percent.Increase

j <- 0.0001
test.val <- 10^10
while (test.val > total.allocation) {
  
  dat.$Percent.Increase[1:tier.size[1]] <- scales::rescale(dat.$comp.[1:tier.size[1]], to = c(0.051, max(0.075 - j,0.052)))
  dat.$Percent.Increase[seq(from = tier.size[1]+1, length.out = tier.size[2])] <- scales::rescale(dat.$comp.[seq(from = tier.size[1]+1, length.out = tier.size[2])], to = c(0.030, max(0.050 - j,0.035)))
  dat.$Percent.Increase[seq(from = c(max(seq(from = tier.size[1]+1, length.out = tier.size[2])) + 1), by = 1, length.out = tier.size[3])] <- 
    scales::rescale(dat.$comp.[seq(from = c(max(seq(from = tier.size[1]+1, length.out = tier.size[2])) + 1), by = 1, length.out = tier.size[3])], to = sort(c(0.02, max(0.030 - j,0.010))))
  
  dat.$comp. <- round(dat.$comp.,3)
  Percent.Increase <- dat. %>% group_by(`comp.`) %>% summarize(Percent.Increase = mean(Percent.Increase))
  dat. <- dat. %>% select(-`Percent.Increase`)
  dat. <- left_join(dat., Percent.Increase)
  dat.$Update.Salary <- (dat.$Salary*dat.$Percent.Increase)
  j = j + 0.0001
  
  test.val <- sum(dat.$Update.Salary)
  print(sum(dat.$Update.Salary))
}

dat.$Percent.Increase <- dat.$Percent.Increase*100

write.csv(left_join(dat., readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1]) %>% select(Name, `W#`, Division,  Sub, Rank)),
          file = paste0("test.",div.,".csv"))

x = left_join(dat., readxl::read_xlsx(dir(pattern = "SOSE Faculty and")[1]) %>% select(Name, `W#`, Division,  Sub, Rank))

plot(jitter(x$comp.,50), jitter(x$Percent.Increase), 
     pch = 20, xlab = "Composite Score", ylab = "Percent Salary Increase",cex = 1.3)
abline(h = c(2.99,5.001))



