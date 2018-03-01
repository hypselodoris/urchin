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
packages <- c("pbapply", "boot", "dplyr", "pwr")
ipak(packages)

## Schroeter 2018-02-20: "I used No/m2 as the data and looked at different years.  To start, concentrate on 2009 that has n=151."
# Get data:
file.name = "./data/Halmay_2004_2013_for_Maine_talk.csv"
halmay.data <- read.csv(file.name)
# Subset data of interest. Include 2009 data, exclude rows lacking measurement of "No_m2":
halmay.data.2009 <- select(filter(halmay.data, Year == 2009, !is.na(No_m2)),c(Year,Longitude,Latitude,No_m2))

# Calculate statistics based on data without resampling:
# Mean density:
mean.urchin.density.2009 <- mean(halmay.data.2009$No_m2)
# Standard deviation:
sd.urchin.density.2009 <- sd(halmay.data.2009$No_m2)
# Sample size:
n.urchin.density.2009 <- length(halmay.data.2009$No_m2)
# Standard error:
se.urchin.density.2009 <- sd.urchin.density.2009/sqrt(n.urchin.density.2009)
# Get 95% confidence intervals of the mean:
ci.urchin.density.2009 <- c(mean.urchin.density.2009-2*se.urchin.density.2009, mean.urchin.density.2009+2*se.urchin.density.2009)

# Calculate statistics based on resampled data:
# Set the desired number of resample iterations:
number.of.resamples <- 1000
# Set the desired number of random rows to subsample from the data to use for bootstrapping:
subsample.number <- 15
# Resample a subset of the data the set number of times, with replacement, and compute the mean: 
bootstrap.urchin.density.means <- pbsapply(1:number.of.resamples,function(x) mean(sample(x=halmay.data.2009$No_m2, size=subsample.number,replace=TRUE)))
# Calculate the SEM of the resulting means:
bootstrap.urchin.density.se <- sd(bootstrap.urchin.density.means)

# Visualize actual vs bootstrap distribution:
hist(halmay.data.2009$No_m2, ylim=c(0,200), xlim=range(pretty(range(halmay.data.2009$No_m2, bootstrap.urchin.density))), xlab = expression("Density of urchins " ~ (individuals/m ^{2})), ylab = "Frequency", main = "Urchin Density Distribution", col = "black", cex.lab=1.3, las=1)
hist(bootstrap.urchin.density.mean, xlab = "", ylab = "", density = 20, col = "grey", axes=F, add=TRUE, lty=1)
abline(v=mean.urchin.density.2009,col="firebrick",lwd=1)
abline(v=mean(bootstrap.urchin.density),col="blue",lwd=1)
abline(v=mean(bootstrap.urchin.density) + bootstrap.urchin.density.se,col="green",lwd=1)
abline(v=mean(bootstrap.urchin.density) - bootstrap.urchin.density.se,col="green",lwd=1)
legend("topright", c("actual", "resampled"), col = c("black", "grey"), lty = c(1,1), bty = "n")

# Use package "boot" to run non-parametric bootstrap analysis. Store results in object of class "boot":
dat = halmay.data.2009$No_m2
subsample.means <- function(dat, ind) 
{
  return(mean(sample(x = dat[ind], size = subsample.number, replace = TRUE)))
}
boot.urchin.density <- boot(data = dat, statistic = subsample.means, R=number.of.resamples)
# Visualize boot object:
plot(boot.urchin.density)
# Calculate confidence intervals for statistic:
boot.ci(boot.urchin.density)

# Set up parameters for power analysis using package "pwr":
effect.size <- 0.1 # Cohen's "d" effect size index. Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.
significance <- 0.05 # alpha. Conventionally = 0.05
power.level <- 0.6 # probability of detecting an effect when there is an effect to be detected.
type.test <- "one.sample" # type of t test.
alt.hypothesis <- "two.sided"

# Run power analysis, store results in object of class "power.htest":
urchin.pwr <- pwr.t.test(n = NULL, d = effect.size, sig.level = significance, power = power.level, type = c("one.sample"), alternative = alt.hypothesis)
# Create ggplot object illustrating relationship of sample size and test power for given set of parameters: 
urchin.pwr.plot <- plot(urchin.pwr)
