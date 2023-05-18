setwd("E:/Salford Uni/ASDV_assignment")
library(fBasics) #for applied statistic analysis
library("stats") #to t-test
library("psych")
library(readxl) #to import excel file
Import_Export <- read_excel("E:/Salford Uni/Import_Export.xlsx") 
View(Import_Export)
#descriptive statistical analysis
attach(Import_Export)
summary(GDP_Growth)
basicStats(data.frame(Food_Production,Food_Exports,Food_Imports))
describe(Import_Export$Food_Production)
describe(Import_Export$Food_Imports)
describe(Import_Export$Food_Exports)
# correlation between two variables
cor.test(Import_Export$Food_Imports, Import_Export$Food_Exports)
cor.test(Import_Export$GDP_Growth, Import_Export$Tax_Revenue, method = "kendall")
#correlation between all variables
#first import library
library(corrplot)
#correlation Matrix
#select only numerical Columns from the dataset and store in data variable

data <- data.frame(GDP = Import_Export$GDP_Growth,
                   Import = Import_Export$Food_Imports,
                   Export = Import_Export$Food_Exports,
                   Tax = Import_Export$Tax_Revenue,
                   Production= Import_Export$Food_Production)
# for showing first 10 rows of the dataset
head(data,10)
datastore=cor(data)
corrplot(datastore, method = "color", type="upper", order="hclust",addCoef.col = "black", tl.col = "black", diag = FALSE)


#Linear Regression Analysis

model_1 <-lm(GDP ~ Tax, data)
summary.lm(model_1)

plot(GDP ~ Tax, data, col="red", main = "Regression : GDP growth and Tax avenue ", xlab = "GDP Growth", ylab = "Tax")
abline(model_1, col="blue")
plot(model_1,1)
plot(model_1,2)
plot(model_1,3)

# Multiple Linear Regression Analysis
# for net exports as GDP percentage

model_2 <- lm( Production ~ Export + Import, data)
summary.lm(model_2)
data.frame(colnames(data))
pairs(data[,c(1,2,3,4,5)], lower.panel = NULL, pch = 19, cex = 0.2, col='blue')
plot(model_2,1)
plot(model_2,2)
plot(model_2,3)

#Import car package
library(car)
#time series
show(Import_Export)
# install t-series package as well as library
install.packages("tseries")
install.packages("TTR")
library(TTR)
library(tseries)
library(forecast)
class(Import_Export)
Import_Export$Country<- NULL
Import_Export$GDP_Growth..annual... <- NULL
Import_Export$Food_Imports....of.merchandise.imports. <- NULL
Import_Export$Food_Exports....of.merchandise.exports. <- NULL
Import_Export$Year <- NULL
class(Import_Export)
Timeseries <- ts(Import_Export$Tax_Revenue,start = c(2012), frequency = 1)
class(Timeseries)
plot.ts(Timeseries)
Diff_Timeseries <- diff(Timeseries)
plot(Diff_Timeseries)
arima_model <- auto.arima(Timeseries,d=1,D=1,stepwise = FALSE,trace = TRUE)
summary(arima_model)
checkresiduals(arima_model)
fore_cast<- forecast(arima_model, h=10)
autoplot(fore_cast)



#hypothesis analysis

install.packages("ggplot2")
install.packages("datarium")
install.packages("qqplotr")
library(datarium)
library(ggplot2)
library(qqplotr)
#non-perametrive t-test
library(readxl)
Import_Export <- read_excel("E:/Salford Uni/Import_Export.xlsx") 
x1=trunc(Import_Export$Food_Production)
y1=trunc(Import_Export$Tax_Revenue)
Import_Export_plot1 <- ggplot(data = Import_Export, aes(group = Year==2012 , x = x1,y = y1))+
    geom_boxplot()+
     theme_classic()+
    theme(legend.position = "None")+
    scale_fill_manual(breaks = x1)
Import_Export_plot

x1=trunc(Import_Export$Food_Exports)
y1=trunc(Import_Export$Food_Imports)
Import_Export_plot2 <- ggplot(data = Import_Export, aes(group = Year==2012 , x = x1,y = 
                                                         y1))+
  geom_boxplot()+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_manual(breaks = x1)
Import_Export_plot

wilcox.test(x1-y1, data=Import_Export_plot1)
wilcox.test(x1-y1, data=Import_Export_plot2)
#--------------------------------------------------------
#Anova hypothesis testing
Anova_test<- Import_Export
Anova_test
 
plot(Tax_Revenue ~ GDP_Growth, data = Anova_test)
boxplot(Tax_Revenue ~ GDP_Growth, data = Anova_test,
        xlab = "Tax Revenue",
        ylab = "GDP Growth", main = "GDP growth according Tax_Revenue of population")

Anova_test1 <- aov(Tax_Revenue ~ GDP_Growth, data = Anova_test)
summary(Anova_test1)
#---------------------------------------------------------
boxplot(Import_Export$Food_Imports,Import_Export$Food_Exports,Import_Export$GDP_Growth,x1,y1, col = c(4,5,6,7,8))

