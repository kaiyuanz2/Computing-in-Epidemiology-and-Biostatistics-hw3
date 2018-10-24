# 作業

# 練習9：請自行編寫一個計算簡單線性迴歸係數估計值的函數，並以課程網頁上seizure.csv的y當成反應變數，ltime當成解釋變數，求迴歸係數估計值(含截距項與斜率項)。
Seizure1 <- read.csv('seizure.csv')
slm <- function(y, x){
  Sxy <- 0
  Sxx <- 0
  for(i in 1:length(x)){
    Sxy <- Sxy + (y[i]-mean(y))*(x[i]-mean(x))
    Sxx <- Sxx + (x[i]-mean(x))^2
  }
  b <- Sxy/Sxx
  a <- mean(y) - b*mean(x)
  return(cat("Intercept = ",a, "Slope = ",b))
}

slm(y=Seizure1$y, x=Seizure1$ltime)

# 練習13：請求出2~100所有的質數 (用while來作do…until…迴圈)
n <- 2
while(n <= 100){
  isPrime = TRUE
  i <- 2
  while(isPrime && n>i){
    isPrime = isPrime && ((n%%i) != 0)
    i = i+1
  }
  if(isPrime){
    print(n)
  }
  n = n+1
}
