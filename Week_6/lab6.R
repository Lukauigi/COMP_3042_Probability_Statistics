DiceMeans=function(n, m) # n rolls of m dice
{
  meanEveryRoll<-c(1:n)
  diceRolls<-c(1:m)
  
  for (i in 1:n) {
    diceRolls<-sample(c(1, 2, 3, 4, 5, 6), m, repl=T)
    meanEveryRoll[i]<-mean(diceRolls)
  }
  
  title<-paste("Means for", n, "rolls of", 
              m, "dice", sep = " ")
  
  hist(meanEveryRoll, main = title, 
          ylab = "Ocurrences", xlab = "Sample Means")
  
  finalMean<-mean(meanEveryRoll)
  diceRollDeviation<-sd(meanEveryRoll)
   
  print(finalMean)
  print(diceRollDeviation)
  
}
