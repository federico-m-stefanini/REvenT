## -----------------------------------------------------------------------------
library(REvenT)
testPart2 <- list(Pa1= c("sano","malato","morto"),
                  Pa2= c("cicatrix","normal"),
                  Pa3=c("giovane","adulto","vecchio"))
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
test1 <-'Pa1:malato;Pa2:normal'
tmp7 <-assignPro(primoPTree,test1,0.23)
tmp7$proba_tree

## -----------------------------------------------------------------------------
tmpGen <- genProbLs(primoPTree)
tmpGen

## -----------------------------------------------------------------------------
tmpGen$event[1:3]
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
for(aux in 1:nrow(tmpGen)){
    primoPTree <-assignPro(primoPTree,tmpGen$path[aux],
                           tmpGen$probVal[aux])
    }
primoPTree$proba_tree

## -----------------------------------------------------------------------------
tavolaCongiunta <- congiungi(primoPTree)
tavolaCongiunta

## -----------------------------------------------------------------------------
sum(tavolaCongiunta$jointPro)

## -----------------------------------------------------------------------------
mymargin1 <- marginalizza(tavolaCongiunta,"Pa2:cicatrix;Pa3:adulto")
mymargin1

## -----------------------------------------------------------------------------
marginalizza(tavolaCongiunta,"Pa2:cicatrix")
marginalizza(tavolaCongiunta,"Pa1:morto;Pa3:vecchio")

## -----------------------------------------------------------------------------
estraSottotavola(tavolaCongiunta,"Pa2:cicatrix;Pa3:adulto")

## -----------------------------------------------------------------------------
myCondizio <-estraSottotavola(tavolaCongiunta,
                              "Pa2:cicatrix;Pa3:adulto")
myCondizio$jointPro <- myCondizio$jointPro / mymargin1$probaMargin
cbind(
      myCondizio[,1:4],
      condizionata= myCondizio$jointPro/ mymargin1$probaMargin
)

## -----------------------------------------------------------------------------
estraSottotavola(tavolaCongiunta,"Pa3:adulto;Pa1:sano;Pa2:normal")
estraSottotavola(tavolaCongiunta,"Pa2:normal;Pa3:adulto;Pa1:sano")

## -----------------------------------------------------------------------------
tetCondiz <- condiziona(tavolaCongiunta,
   "Pa1:sano  | Pa3:vecchio; Pa2:cicatrix")
tetCondiz

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "   | Pa3:vecchio; Pa2:cicatrix")

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "   | Pa2:cicatrix")
sum(condiziona(tavolaCongiunta, "   | Pa2:cicatrix")$condPro)

## -----------------------------------------------------------------------------
condiziona(tavolaCongiunta, "    | Pa3:adulto")
condiziona(tavolaCongiunta, " Pa2:normal   | Pa3:adulto")

## -----------------------------------------------------------------------------
test7 <-condiziona(tavolaCongiunta, " Pa1:sano   | Pa2:normal")
test7

## -----------------------------------------------------------------------------
marginalizza(test7,"Pa1:sano;Pa2:normal")$tavolaMarginale

