DiceRollFrequenciesWithoutSample=function(n, m) # n rolls of m dice
{
  
  rolls=rbinom(n, m, 1/6)
  rolls.freq = table(rolls)/n
  
  title<-paste("Probability of Rolling 3's from", n, "rolls of", 
               m, "dice", sep = " ")
  
  barplot(rolls.freq, main = title, 
       ylab = "Probabitlies", xlab = "Number of 3's in dice rolls")
  
  View(rolls.freq)
  return(rolls.freq)
  
}

DiceRollFrequenciesWithSample=function(n, m)
{
  threesResult<-c(1:n)
  threesFromDiceRolls<-c(1:n)
  diceRolls<-c(1:m)
  
  for (i in 1:n) {
    diceRolls<-sample(c(1, 2, 3, 4, 5, 6), m, repl=T)
    threesFromDiceRolls[i]=sum(diceRolls==3)
  }
  
  threesFromDiceRolls.freq=table(threesFromDiceRolls)/n
  View(table(threesFromDiceRolls.freq))
  
  title<-paste("Probability of Rolling 3's from", n, "rolls of", 
               m, "dice", sep = " ")
  
  barplot(threesFromDiceRolls.freq, main = title, 
          ylab = "Proababilities", xlab = "Number of 3's in dice rolls")
  
  return(threesFromDiceRolls.freq)
}