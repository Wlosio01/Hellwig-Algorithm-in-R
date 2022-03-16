
# We give the exogenous variable in the vector
# For example
Y <- c(69633.20,74286.60,79376.80,84807.10,85272.50,133371.70,140014.70,156475,134439.70,146970.5,155269.30,178435.80,152377.20,166308.2,171138.8,195772.5,165580.5,176563.2,179928.9,200268.2)

#Next we give the endogenous variables in the vectors
# For example
X1 <- c(55016.90, 59571.30, 61465.1, 64109.1, 72329.3, 109053.2, 112324.8, 108964.5, 119933.1, 123952.4, 123976.2, 59571.3, 61465.1, 64109.1, 72329.3, 109053.2, 112324.8, 149274.4, 147160.5, 148256.9)

X2 <- c(42664.4, 45427.2, 47733.4, 49739, 56495.8, 85560.6, 88979.6, 87150.9, 94867.9, 97427.4, 98440.6, 99737.7, 109532.7, 108941.9, 47733.4,
        49739, 56495.8, 60406.7, 61818.6, 118716)
X3 <- c(11637.7, 13098.2, 15334, 20634.7, 11595.2, 30680.3, 34631.5, 57013.6, 23900.6, 34199.2, 39592.2, 64593.3, 28670.4, 39687.6, 43552, 69782.9, 25806.1, 35004.8, 37553.4, 58210.1)
X4 <- c(10661.4, 12160.7, 14488.8, 20093.7, 10967.2, 29370.4, 33302.9, 54992.6, 23808.2, 32983.6, 37654.2, 62244.4, 27285, 37304.3, 41598.4, 67493.2, 28758.9, 34377.1, 36084.6, 56342)
X5 <- c(66654.6, 72669.5, 76799.1, 163568.4, 188678.4, 139733.5, 146956.3, 165978.1, 143833.7, 158151.6, 163568.4, 188678.4, 165905.3, 177436.6, 181212.7, 208484.2, 173329.2, 184279.2, 184713.9, 206467)



# We put endogenous variables into list

list_of_endogenous <- list(X1,X2,X3,X4,X5)



# Function

Hellwig <- function(Y, listX)
{
  n <- length(listX)
  number_of_combination <- 2^n - 1 
  vector_H <- c()
  # We will use binary notation to optimize the algorithm
  for(i in 1:number_of_combination){
    H <- 0 
    case <- intToBits(i)
    elements_in_case <- list()
    for(j in 1:n){
      if(case[j] == 1){
        len <- length(elements_in_case)
        elements_in_case[[len + 1]] <- listX[[j]]
      }}
      for (j in 1:length(elements_in_case)){
        # We count rj
        corXY <- cor(elements_in_case[[j]],Y)
        sum <- 0 
        for(k in 1:length(elements_in_case)){
          #we count sum of rij
          sum = sum + abs(cor(elements_in_case[[j]], elements_in_case[[k]]))
        }
        # Here we count H by add all h in loop
        H = H + (corXY^2)/sum
      }
      vector_H <- c(vector_H, H)
  }
  # We will build a data frame to show information 
  combinations <- c()
  for(i in 1:number_of_combination){
    combinations <- c(combinations,toString(intToBits(i)[1:n]))
  }
 dataHellwig <- data.frame("combination" = combinations,
                           "H - value" = vector_H)
 
 return(dataHellwig)
}

# Using Function "Hellwig" to show results of "H" to all combinations

dane <- Hellwig(Y, list_of_endogenous)
# We show separately the best combination
cat("The best combinantion is ",dane[which.max(dane$H...value),1]," where integral capacity of information is equal to: ",dane[which.max(dane$H...value),2])

