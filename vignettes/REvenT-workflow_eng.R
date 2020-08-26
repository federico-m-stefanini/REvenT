## -----------------------------------------------------------------------------
library(REvenT)
testPart2 <- list(Pa1= c("sane","sick","dead"),
                  Pa2= c("cicatrix","normal"),
                  Pa3=c("young","adult","old"))
testPart2

## -----------------------------------------------------------------------------
primaColPat <- REvenT::eve_tree(testPart2)
primaColPat

## -----------------------------------------------------------------------------
attr(primaColPat,"Tipologia")

## -----------------------------------------------------------------------------
primoPTree<- proba_tree(primaColPat)
primoPTree

## -----------------------------------------------------------------------------
test1 <-'Pa1:sick;Pa2:normal'
tmp7 <-assignPro(primoPTree,test1,0.23)
tmp7$proba_tree

## -----------------------------------------------------------------------------
tmpGen <- genProbLs(primoPTree)
tmpGen[,-1]

## -----------------------------------------------------------------------------
cbind(tmpGen$event[1:3])
tmpGen$probVal[1:3] <- c(0.1, 0.7, 0.2)
tmpGen[1:3,]

## -----------------------------------------------------------------------------
sum(tmpGen[1:3,4])

## -----------------------------------------------------------------------------
tmpGen$probVal[c(4,7)] <- c(0.1, 0.9)
tmpGen[c(4,7),-1]

tmpGen$probVal[c(5,8)] <- c(0.3, 0.7)
tmpGen[c(5,8),-1]

tmpGen$probVal[c(6,9)] <- c(0.95, 0.05)
tmpGen[c(6,9),-1]

tmpGen$probVal[c(10,16,22)] <- c(1,5,10)/16
tmpGen[c(10,16,22),-1]

tmpGen$probVal[c(10,16,22)+1] <- c(5,5,5)/15
tmpGen[c(10,16,22)+1,-1]

tmpGen$probVal[c(10,16,22)+2] <- c(25,5,15)/45
tmpGen[c(10,16,22)+2,-1]

tmpGen$probVal[c(10,16,22)+3] <- c(50,5,25)/80
tmpGen[c(10,16,22)+3,-1]

tmpGen$probVal[c(10,16,22)+4] <- c(25,5,25)/55
tmpGen[c(10,16,22)+4,-1]

tmpGen$probVal[c(10,16,22)+5] <- c(1,5,10)/16
tmpGen[c(10,16,22)+5,-1]

## -----------------------------------------------------------------------------
tmpGen[,-1]

## -----------------------------------------------------------------------------
primoPTree <- fetchProb(primoPTree,tmpGen)
primoPTree$proba_tree

## -----------------------------------------------------------------------------
tavolaCongiunta <- congiungi(primoPTree)
tavolaCongiunta

## -----------------------------------------------------------------------------
sum(tavolaCongiunta$jointPro)

## -----------------------------------------------------------------------------
mymargin1 <- marginalizza(tavolaCongiunta,"Pa2:cicatrix;Pa3:adult")
mymargin1

## -----------------------------------------------------------------------------
marginalizza(tavolaCongiunta,"Pa2:cicatrix")
marginalizza(tavolaCongiunta,"Pa1:dead;Pa3:old")

## -----------------------------------------------------------------------------
estraSottotavola(tavolaCongiunta,"Pa2:cicatrix;Pa3:adult")

## -----------------------------------------------------------------------------
myCondizio <-estraSottotavola(tavolaCongiunta,
                              "Pa2:cicatrix;Pa3:adult")
myCondizio$jointPro <- myCondizio$jointPro / mymargin1$probaMargin
cbind(
      myCondizio[,1:4],
      condizionata= myCondizio$jointPro/ mymargin1$probaMargin
)

## -----------------------------------------------------------------------------
estraSottotavola(tavolaCongiunta,"Pa3:adult;Pa1:sane;Pa2:normal")
estraSottotavola(tavolaCongiunta,"Pa2:normal;Pa3:adult;Pa1:sane")

## -----------------------------------------------------------------------------
tetCondiz <- condiziona(tavolaCongiunta,
   "Pa1:sane  | Pa3:old; Pa2:cicatrix")
tetCondiz

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "   | Pa3:old; Pa2:cicatrix")

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "   | Pa2:cicatrix")
sum(condiziona(tavolaCongiunta, "   | Pa2:cicatrix")$condPro)

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "    | Pa3:adult")
condiziona(tavolaCongiunta, " Pa2:normal   | Pa3:adult")

## -----------------------------------------------------------------------------
test7 <-condiziona(tavolaCongiunta, " Pa1:sane   | Pa2:normal")
test7

## -----------------------------------------------------------------------------
marginalizza(test7,"Pa1:sane;Pa2:normal")$tavolaMarginale

