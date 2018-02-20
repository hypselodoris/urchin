# Urchin
# Bootstrapping experiment with Peter Halmay data
# author: Nathan Spindel
# date: 2018-02-20

# Set working directory: 
urchin.directory <- "/Volumes/workspaces/nate/public/coding_work/R/urchin"
setwd(urchin.directory)
# Load utility functions
source("./functions.R")
# Load packages
packages <- c("pbapply", "boot", "ggplot2")
ipak(packages)

## Schroeter 2018-02-20: "I used No/m2 as the data and looked at different years.  To start, concentrate on 2009 that has n=151."
# Get data:
file.name = "./data/Halmay_2004_2013_for_Maine_talk.csv"
halmay.data <- read.csv(file.name)
# Subset data of interest:
halmay.data.2009 <- select(filter(halmay.data, Year == 2009, !is.na(No_m2)),c(Year,Longitude,Latitude,No_m2))
# # Visualize sampling sites spatially:
# ggplot(halmay.data.2009, aes(x=halmay.data.2009$Longitude, y=halmay.data.2009$Latitude, xlab="", ylab="")) +
#   geom_point() +
#   theme_classic()
# Get mean density:
mean.urchin.density.2009 <- mean(halmay.data.2009$No_m2)
# Get variance:
var.urchin.density.2009 <- var(halmay.data.2009$No_m2)
# Get standard deviation:
sd.urchin.density.2009 <- sd(halmay.data.2009$No_m2)
# Get standard error:
se.urchin.density.2009 <- sd(halmay.data.2009$No_m2)/sqrt(length(halmay.data.2009$No_m2))
# Get 95% confidence intervals of the mean:
ci.urchin.density.2009 <- c(mean(halmay.data.2009$No_m2)-2*se.urchin.density.2009,mean(halmay.data.2009$No_m2)+2*se.urchin.density.2009)
# Set the desired number of resample iterations:
number.of.resamples <- 1000
# Set the desired number of samples to use for bootstrapping:
subsample.number <- 15
# Resample with replacement the set number of times and compute the mean: 
bootstrap.urchin.density <- pbsapply(1:number.of.resamples,function(x) mean(sample_n(tbl = halmay.data.2009$No_m2, size=subsample.number,replace=TRUE)))

# Compute 95% CI from bootstrapped data:
bootstrap.urchin.density.ci <- boot(data=halmay.data.2009$No_m2,statistic=function(x,i) mean(x[i]),R=number.of.resamples)
boot.ci(bootstrap.urchin.density.ci)

par(mfrow=c(nrows=2, ncols=1))
hist(halmay.data.2009$No_m2)
abline(v=mean(halmay.data.2009$No_m2),col="orange",lwd=3)
hist(bootstrap.urchin.density)
abline(v=mean(bootstrap.urchin.density),col="orange",lwd=3)
