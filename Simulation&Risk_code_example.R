#install.packages("truncnorm")
#install.packages("Rlab")
#install.packages("graphics")
#install.packages("ks")
#install.packages("metRology")
#install.packages("stats")

library(graphics)
library(ks)
library(metRology)
library(truncnorm)
library(Rlab)
library(stats)
###############################################################################
##################### Saving 3 homeworks data File Locations ####################
###############################################################################

file.dir <- "/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/Analysis_Data_2.csv"
input.file1 <- "Analysis_Data_2.csv"

# setting the working directory to the file directory 
setwd("/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/")

analysis <- read.csv(file = "Analysis_Data_2.csv", header = TRUE)

cost <-  analysis[c(2:18),c(2:4)]
change <- analysis[c(2:18),c(5:7)]


# Saving homework data File Locations #
file.dir <- "/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/Analysis_Data_3.csv"
input.file1 <- "Analysis_Data_3.csv"

# setting the working directory to the file directory 
setwd("/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/")

analysis3 <- read.csv(file = "C:\\Users\\Asus\\Documents\\Learning\\Advanced Analytics\\Simulation and Risk\\Homework\\Homework 3\\Analysis_Data_3_2006.csv", header = TRUE)

file.dir <- "/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/Analysis_Data_5.csv"
input.file1 <- "Analysis_Data_5.csv"

# setting the working directory to the file directory 
setwd("/Users/ablaelsergany/Google Drive/SchoolWork/6-Spring1/Simulation_and_Risk/HOMEWORK/Homework1_SR/")

analysis5 <- read.csv(file = "C:\\Users\\Asus\\Documents\\Learning\\Advanced Analytics\\Simulation and Risk\\Homework\\Homework 3\\Analysis_Data_5.csv", header = TRUE)

###############################################################################
############# Standardize and destandardize functions #########################
###############################################################################

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}


###################################
#### Single Dry Well Cost #########
###################################

set.seed(1234)
sim.size = 100
Dry_well_cost <- rep(0,sim.size) # Create an empty list to store dry well costs
for(i in 1:sim.size){
  P02 <- 2664.6
  r2 <- rkde(fhat = kde(analysis3$Return, h=.07935), n=1)
  Pt2 <- P02*(1+r2)
  for(j in 1:5){
    r2<- rkde(fhat = kde(analysis3$Return, h=.07935), n=1)
    #hist(Est.Pt2, breaks=50, main='Estimated Value Distribution',
    # xlab='Final Value')
    Pt2 <- Pt2*(1+r2)
  }
  for(k in 1:3){
    triangle_dist<-rtri(1, min =-.22, max = -.07, mode= -.0917)
    Pt2 <- Pt2*(1+triangle_dist)
  }
  for(l in 1:5){
    triangle_dist<-rtri(1, min =.02, max = .06, mode= .05)
    Pt2 <- Pt2*(1+triangle_dist)
  }
  Cost2 <- Pt2*1000
  # Fixed costs for the Dry Well
  FC_lease<- 960*rnorm(n=1, mean=600, sd=50)
  FC_seismic<- 43000*rnorm(n=1, mean=3, sd=.35)
  FC_completion<- rnorm(n=1, mean=390000, sd=50000)
  FC_overhead<- rtri(n=1, min=172000,max=279500, mode=215000)
  # Initial Cost for a Dry well
  Dry_well_cost[i] <- Cost2 +FC_lease+FC_seismic+FC_overhead #Store dry well costs
}

# histogram and quantiles
hist(Dry_well_cost, breaks=50, main='Sampling Distribution of fixed well cost', xlab='dry fixed cost')
quantile(Dry_well_cost, probs = c(0, 1, 0.25, 0.01, 0.05, 0.95,0.99, 0.75, 0.5))

###################################
#### Single Wet Well Cost #########
###################################

R <- matrix(data=cbind(1,.64, .64, 1), nrow=2)
U <- t(chol(R))
Decline<- runif(n=sim.size, min=.15, max=.32)
IP <- rlnorm(n=sim.size, meanlog=6, sdlog=0.28)
Both <- cbind(standardize(IP), standardize(Decline))
ipdecl.rate1 <- U %*% t(Both)
ipdecl.rate <- t(ipdecl.rate1)
final.ipdecl.rate<- cbind(destandardize(ipdecl.rate[,1], IP), destandardize(ipdecl.rate[,2], Decline))
FNR_WACC<-rep(0,sim.size)
Wet_well_cost <- rep(0,sim.size)
set.seed(1234)

for(i in 1:sim.size){
  P02 <- 2087.4 # Added in Wet_well drilling costs here as a repeat of your code above
  r2 <- rkde(fhat = kde(analysis3$Return, h=.07935), n=1)
  Pt2 <- P02*(1+r2)
  for(j in 1:5){
    r2<- rkde(fhat = kde(analysis3$Return, h=.07935), n=1)
    #hist(Est.Pt2, breaks=50, main='Estimated Value Distribution',
    # xlab='Final Value')
    Pt2 <- Pt2*(1+r2)
  }
  for(k in 1:3){
    triangle_dist<-rtri(1, min =-.22, max = -.07, mode= -.0917)
    Pt2 <- Pt2*(1+triangle_dist)
  }
  for(l in 1:5){
    triangle_dist<-rtri(1, min =.02, max = .06, mode= .05)
    Pt2 <- Pt2*(1+triangle_dist)
  }
  Cost2 <- Pt2*1000
  # Fixed costs for the Dry Well
  FC_lease<- 960*rnorm(n=1, mean=600, sd=50)
  FC_seismic<- 43000*rnorm(n=1, mean=3, sd=.35)
  FC_completion<- rnorm(n=1, mean=390000, sd=50000)
  FC_overhead<- rtri(n=1, min=172000,max=279500, mode=215000)
  # Initial Cost for a Dry well
  Wet_well_cost[i] <- Cost2 +FC_lease+FC_seismic+FC_overhead+FC_completion #Store dry well costs



 
  #Production Risk per barrel #Correlation
  #start at year 1, pick where you left off 
  Rate_year_end <- rep(0,15)
  begin<- rep(0,15)
  for(m in 1:15){
    begin[m] <- final.ipdecl.rate[i,1]*(1-final.ipdecl.rate[i,2])^(m-1)
    Rate_year_end[m]  <- final.ipdecl.rate[i,1]*(1-final.ipdecl.rate[i,2])^(m)
  }
  Annual_prod<- 365*((begin+Rate_year_end)/2)
  #Revenue Risk: oil price-operating costs
  WACC<-rep(0,15)
  for(r in 1:15){
    WACC[r]<-(1+.1)^r
  }
  oil_price <- rep(0,15) #Create 15 oil prices, one for each year
  for(n in 1:15){
    oil_price[n] <-rtri(1, max=analysis5$High[n] , min=analysis5$Low[n] , mode=analysis5$Mode[n])
  }
  annual_revenue<- oil_price*Annual_prod #Now multiplied by the appropriate oil price for that year
  NRI<-annual_revenue*rnorm(n=1, mean=.75, sd=.02)
  severance_taxes<-.046
  Operating_Cost<-rnorm(n=15, mean=2.25, sd=.3) #15 costs, 1 for each year
  Revenue_taxed=NRI*(1-severance_taxes)
  Total_Cost<-(Operating_Cost*Annual_prod)+FC_overhead #Cost repeats each year
  #Net Present Value Calculation
  FNR<- Revenue_taxed-Total_Cost
  FNR_WACC[i]<-sum(FNR/WACC)
}
NPV <- FNR_WACC-Wet_well_cost

hist(NPV, breaks=50, main='NPV Distribution', xlab='Final Value')
quantile(NPV, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))


###################################
######### DRY WELL RISK ###########
###################################

set.seed(1234)
WetWells <- rep(1, sim.size)
for(i in 1:sim.size){
  wellsN <- runif(n=1, min=10, max=30) 
  
  # getting the hydrcarbons * reservoir
  hydrocarbons <- rtruncnorm(n=wellsN, a=0, b=1, mean=.99, sd=.05)
  reservoir <- rtruncnorm(n=wellsN, a=0, b=1, mean=.8, sd=.1)
  Phr <- hydrocarbons * reservoir
  # getting the probability of 1 or 0
  bernouli<-rbinom(n=wellsN, size=1, prob=Phr)
  a <- table(bernouli) 
  numb <- a[names(a)==1]
  WetWells[i] <- numb
}


hist(WetWells, breaks=50, main='trial', xlab='Value')

# multiplying NPV with wet wells dist to get total cost for all wet wells ... not sure this is correct
tot_cost <- WetWells * NPV
hist(tot_cost, breaks=50, main='trial', xlab='Value')
quantile(tot_cost, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))
