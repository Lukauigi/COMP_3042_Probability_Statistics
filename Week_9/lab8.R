DrawCardsSample=function(n, m)
{
  hand<-c(1:m)
  acesDrawn=c(1:n)
  
  for (i in 1:n)
  {
    hand=sample(Cards, m, repl=F)
    acesDrawn[i]=sum(grepl("A", hand))
  }
  
  acesDrawn.freq=table(acesDrawn)/n
  # View(table(acesDrawn.freq))
  
  title<-paste("Probability of drawing an ace from",
               n, "seperate draws of", m, "cards",
               sep=" ")
  
  barplot(acesDrawn.freq, main = title, 
          ylab = "Probability", xlab = "Result of aces drawn")
  
  return(acesDrawn.freq)
}

DrawCardsRHyper=function(n, m)
{
  
  aceProbs=table(rhyper(m, 4, 48, n))/m
  
  title<-paste("Probability of drawing an ace from",
               n, "seperate draws of", m, "cards",
               sep=" ")
  
  barplot(aceProbs, main = title, 
          ylab = "Probability", xlab = "Result of aces drawn")
  
  return(aceProbs)
}