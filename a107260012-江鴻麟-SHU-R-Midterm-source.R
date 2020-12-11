# 2020/12/11(五), 109學年第一學期 資料科學應用 R期中考

學號:a107260012        姓名: 江鴻麟
  
  
# ex1(a)
  
study(x,y)
data.frame(x,y, U , Tuition, Fit)
list(Eng.hr=x, Comp.hr=y, Tuition=Tuition, U=U )
study <- function(x,y){
  #  x <-c(13:17)
  #  y <-c(8:12)
  a <-matrix(0, 25, 5)
  for(x in 13:17){
    for(y in 8:12){
      U <- x*(0.5)*y*(0.5)
      Tuition <- 400*x+600*y
      fit <- ifelse(Tuition <= 12000, "*", "")
      cat(x,y, Tuition, U)
    }
    cat("\n")
  }
}

library(readxl)
readxl_example()
#ex2(a)
xlsx_file<- "Score-109.xlsx"
excel_sheets(xlsx_file)
data<-read_excel(xlsx_file,sheet="score",na="NA",skip=1)
data11 <- as.data.frame(data)
x<-as.data.frame(head(data11, 5))
x<-as.data.frame(tail(data11, 5))
x

#ex2(b)
data[is.na(data)] <- 0
AS <- which(data[,2] < 60 & data[,3] < 60)
data[AS,]

# ex2(c)
a1 <- sum(data[,2])/75
b1 <- sum(data[,3])/75
my.cor <-for(i in 1:75){
  z1 <- (data[i,2] - a1)*(data[i,3] - b1)
  z2 <- (data[i,2] - a1)*2*0.5
  z3 <- (data[i,3] - b1)*2*0.5
  z <- z1/(z2*z3)
  z
}


# ex2(d)
cor(mydata11[,2:3

