ExponentialMeans=function(n, m) {
  means=c(1:n)
  distribution=(1:m)
  for (i in 1:n) {
    distribution=rexp(m, rate = 1)
    means[i]=mean(distribution)
  }
  
  meanOfMeans=mean(means)
  deviationOfMeanOfMeans=sd(means)
  
  print(meanOfMeans)
  print(meanOfMeans)
  
  title=paste(n, "samples of", "size", m, sep=" ")
  
  hist(means, main=title, xlab = "Mean")
}

confidenceQ4=function(data, conflevel) {
  tTestData=t.test(data, conf.level = conflevel)

  lowerbound=tTestData$conf.int[1]
  upperbound=tTestData$conf.int[2]
  
  CIsentence=cat("We are", conflevel*100, 
                 "percent sure that the mean is between", lowerbound, 
                 "and", upperbound, ".")
  
}

confidence=function(data, conflevel, meanInfo, measurement) {
  if (missing(conflevel)) {
    conflevel=0.95
  }
  
  tTestData=t.test(data, conf.level = conflevel)
  
  lowerbound=tTestData$conf.int[1]
  upperbound=tTestData$conf.int[2]
  
  CIsentence=cat("We are", conflevel*100, 
                 "percent sure that the mean", meanInfo,
                 "is between", lowerbound, 
                 "and", upperbound, measurement, ".")
}
