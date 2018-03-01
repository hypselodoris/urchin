# Urchin
# Functions
# author: Nathan Spindel
# date: 2018-02-20

# ipak function: install and load multiple R packages.  check to see if
# packages are installed. Install them if they are not, then load them
# into the R session.
# SOURCE: https://gist.github.com/stevenworthington/3178163
# argument: pkg
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage: packages <- c('RMySQL', 'ggplot2', 'gridExtra', 'reshape2',
# 'scales', 'grid') ipak(packages)

# Estimation function to be used for each bootstrap iteration. Intended to be used in conjunction with "boot" function in package "boot".
# Take a random sample of rows, the number of which are set in variable "subsample.number", with replacement from the data and calculate the mean.
subsample.means <- function(dat, ind) 
{
  return(mean(sample(x = dat[ind], size = subsample.number, replace = TRUE)))
}

# Custom plot function for comparing actual vs bootstrapped data.
plot.actual.vs.bootstrap <- function()
{
  hist(halmay.data.2009$No_m2, ylim=c(0,200), xlim=range(pretty(range(halmay.data.2009$No_m2, bootstrap.urchin.density.means))), xlab = expression("Density of urchins " ~ (individuals/m ^{2})), ylab = "Frequency", main = "Urchin Density Distribution", col = "black", cex.lab=1.3, las=1)
  hist(bootstrap.urchin.density.means, xlab = "", ylab = "", density = 20, col = "grey", axes=F, add=TRUE, lty=1)
  abline(v=mean.urchin.density.2009,col="firebrick",lwd=1)
  abline(v=mean.urchin.density.2009 - se.urchin.density.2009,col="firebrick",lty="dashed")
  abline(v=mean.urchin.density.2009 + se.urchin.density.2009,col="firebrick",lty="dashed")
  abline(v=mean.urchin.density.2009,col="firebrick",lwd=1)
  abline(v=mean(bootstrap.urchin.density.means),col="blue",lwd=1)
  abline(v=mean(bootstrap.urchin.density.means) + bootstrap.urchin.density.se,col="green",lty="dashed")
  abline(v=mean(bootstrap.urchin.density.means) - bootstrap.urchin.density.se,col="green",lty="dashed")
  legend("topright", c("actual", "resampled"), col = c("black", "grey"), lty = c(1,1), bty = "n")
}
