GSS <-read.csv(file="GSScsv.csv",header=TRUE)
x = as.factor(GSS$BIGBANG)
y = as.factor(GSS$RACE)



fisher.two = function(x, y){
  # x and y are factor variables
  levelsY = levels(y)
  n= length(levelsY)
  for (i in 1:(n-1))
    for (j in (i+1):n) {
      print(c(levelsY[i],levelsY[j]))
      tempDF=data.frame(x,y)
      tempDFfiltered = filter(tempDF, y %in% c(levelsY[i],levelsY[j]))
    
      droplevels(tempDFfiltered$y)
      dropIndex = c(i,j)
      fisherTable = as.matrix(table(tempDFfiltered$x, tempDFfiltered$y)[,dropIndex])
      print(fisherTable)

      fisherTest=fisher.test(fisherTable, conf.int = TRUE)
      print(fisherTest)

    }  
}

fisher.two(x,y)

