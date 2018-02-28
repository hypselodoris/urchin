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
packages <- c("pbapply", "boot", "dplyr")
ipak(packages)

## Schroeter 2018-02-20: "I used No/m2 as the data and looked at different years.  To start, concentrate on 2009 that has n=151."
# Get data:
file.name = "./data/Halmay_2004_2013_for_Maine_talk.csv"
halmay.data <- read.csv(file.name)
# Subset data of interest. Include 2009 data, exclude rows lacking measurement of "No_m2":
halmay.data.2009 <- select(filter(halmay.data, Year == 2009, !is.na(No_m2)),c(Year,Longitude,Latitude,No_m2))

# Calculate statistics based on data without resampling:
# Get mean density:
mean.urchin.density.2009 <- mean(halmay.data.2009$No_m2)
# Get standard error:
se.urchin.density.2009 <- sd(halmay.data.2009$No_m2)/sqrt(length(halmay.data.2009$No_m2))
# Get 95% confidence intervals of the mean:
ci.urchin.density.2009 <- c(mean(halmay.data.2009$No_m2)-2*se.urchin.density.2009,mean(halmay.data.2009$No_m2)+2*se.urchin.density.2009)

# Calculate statistics based on resampled data:
# Set the desired number of resample iterations:
number.of.resamples <- 1000
# Set the desired number of random rows to subsample from the data to use for bootstrapping:
subsample.number <- 25
# Resample a subset of the data the set number of times, with replacement, and compute the mean: 
bootstrap.urchin.density <- pbsapply(1:number.of.resamples,function(x) mean(sample(x=halmay.data.2009$No_m2, size=subsample.number,replace=TRUE)))
# Visualize actual vs bootstrap distribution:
hist(halmay.data.2009$No_m2, ylim=c(0,200), xlim=range(pretty(range(halmay.data.2009$No_m2, bootstrap.urchin.density))), xlab = expression("Density of urchins " ~ (individuals/m ^{2})), ylab = "Frequency", main = "Urchin Density Distribution", col = "black", cex.lab=1.3, las=1)
hist(bootstrap.urchin.density, xlab = "", ylab = "", density = 20, col = "grey", axes=F, add=TRUE, lty=1)
abline(v=mean.urchin.density.2009,col="firebrick",lwd=1)
abline(v=mean(bootstrap.urchin.density),col="blue",lwd=1)
legend("topright", c("actual", "resampled"), col = c("black", "grey"), lty = c(1,1), bty = "n")

# Use package "boot" to run non-parametric bootstrap:
bootstrap.urchin.density.ci <- boot(data=halmay.data.2009$No_m2,statistic=function(x,i) mean(x[i]),R=number.of.resamples)
boot.ci(bootstrap.urchin.density.ci)
