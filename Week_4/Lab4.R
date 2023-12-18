FlipOnce = function()
{  HeadOrTail<-sample(c("Heads", "Tails"), 1)
   return(HeadOrTail)
}

CoinResults=function(n)
{
  coinList<-c(1:n) # forms a list from 1 to n
  for (i in 1:n)
  {
    coinList[i]=FlipOnce() # a result is assigned to index
  }
  return(coinList)
}

CoinResultsNoLoop=function(n)
{
  coinList<-sample(c("Heads", "Tails"), n, repl=T)
  return(coinList)
}

ProbHeads=function(n)
{
  coinList<-CoinResults(n)
  numHeads<-sum(coinList=="Heads")
  return(numHeads/n)
}

MaxAndMinHeads=function(n, m)
{
  probResults<-c(1:m)
  for (i in 1:m)
  {
    probResults[i]=ProbHeads(n)
  }
  maxHeads = max(probResults)
  minHeads = min(probResults)
  return(c(maxHeads, minHeads))
}

RollDie=function(n)
{
  dieRolls<-sample(c(1, 2, 3, 4, 5, 6), n, repl=T)
  dieTable<-table(dieRolls)
  
  title=paste("Die was rolled", n, "times", sep=" ")
  
  return(barplot(dieTable, xlab="Die faces rolled",
                 ylab="Frequency rolled", 
                 main=title))
}

RollSomeDice=function(n, m)
{
  threesFromDiceRolls<-c(1:m)
  diceRolls<-c(1:n)
  
  for (i in 1:m) {
    diceRolls<-sample(c(1, 2, 3, 4, 5, 6), n, repl=T)
    threesFromDiceRolls[i]=sum(diceRolls==3)
  }
  View(diceRolls)
  
  title=paste("Number of 3's obtained in rolling",
              m, "dice", sep=" ")
  
  return(barplot(threesFromDiceRolls,
                 xlab="Dice rolled", 
                 ylab="Frequency of 3's", 
                 main=title))
}

DrawCardsWithReplacement=function(n, m)
{
  suitColors<-c("red","black")
  deck<-rep(c(suitColors), times=26)
  
  draw<-c(1:m)
  redCards<-c(1:n)
  
  for (i in 1:n)
  {
    draw<-sample(c(deck), m, repl=T)
    redCards[i]<-sum(draw=="red")
  }
  
  title<-paste("Frequency of red cards from",
               n, "seperate draws of", m, "cards",
               sep=" ")

  return(barplot(redCards, main=title, 
                 xlab="Seperate draws", 
                 ylab="Frequncy of red cards"))
}

DrawCardsWithoutReplacement=function(n, m)
{
  suitColors<-c("red","black")
  deck<-rep(c(suitColors), times=26)
  
  draw<-c(1:n)
  redCards<-c(1:m)
  
  for (i in 1:n)
  {
    draw<-sample(c(deck), m, repl=F)
    redCards[i]<-sum(draw=="red")
  }
  
  title<-paste("Frequency of red cards from",
               n, "seperate draws of", m, "cards",
               sep=" ")
  
  return(barplot(redCards, main=title, 
                 xlab="Seperate draws", 
                 ylab="Frequncy of red cards"))
}