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

# Function to generate n number of random deviates from the uniform distribution (between 0 and 1).
# argument: n
# return: numeric vector containing n random probabilities.
randomProbability <- function(n) {
  if (reproducible.results.mode == TRUE) {
    set.seed(200)
  } 
  result <- runif(n)
  return(result)
}

# Variable to store the user-defined degree of certainty in a format compatible with matrices.
desired.percentile <<- 100 - degree.of.certainty

# Monte Carlo (MC) simulation function. 
# return: the percentile calculation for a given acreage.
simulation.function <- function(X, n){
  # Call randomProbability function to generate n random probability values and store the results in a numeric vector.
  random.probability.value <- randomProbability(number.rows)
  # Calculate inverse of the standard normal cumulative distribution given a probability, random.probability.value. Store results in a numeric vector.
  inverse.value <- qnorm(random.probability.value) 
  # Calculate normalized values for biomass density using the inverse.value vector. Store results in a numeric vector.
  normalized.biomass.density <-  biomass.density + standard.error * inverse.value
  # Convert biomass density (g/m2) to US tons for each reef area in acreage.sequence. Store results in a matrix, tons.matrix.
  tons.matrix <- outer(normalized.biomass.density,acreage.sequence.prediction, FUN = tonsFromAcreageAndBiomassDensity)
  # Calculate 4-row moving average column-wise over tons.matrix. Store results in a matrix, four.trial.moving.average.matrix.
  four.trial.moving.average.matrix <- apply(tons.matrix[,1:length(acreage.sequence.prediction)], 2, SMA, n=4)
  # Get pairwise maxima for corresponding values in tons.matrix and four.trial.moving.average.matrix. Store results in a matrix, pairwise.max.matrix.
  pairwise.max.matrix <- pmax(tons.matrix, four.trial.moving.average.matrix)
  # Generate a probability sequence for calculating percentiles.
  probability.sequence <- seq(0.01, 0.99, 0.01)
  # Calculate column-wise percentiles from pairwise.max.matrix. Store results in a matrix, percentile.matrix.
  percentile.matrix <- apply(pairwise.max.matrix, 2, quantile, probs = probability.sequence, na.rm = TRUE)
  if (output == "tonnage") {
    # If the switch "output" is set to tonnage, return only the single calculated value from the percentile matrix corresponding to the acreage of interest.
    return(percentile.matrix[desired.percentile, 1])
  } else {
    # If the switch "output" is NOT set to tonnage, return the entire set of tonnage calculations at each acreage in acreage.sequence.
    return(percentile.matrix[desired.percentile, ])
  }
}
