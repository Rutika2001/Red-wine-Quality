#---- load all necessary packages----

library(ggplot2) 
library(tidyr)
library(reshape2)
library(gridExtra)
library(olsrr)
library(caret)

#----read csv file----
data<-read.csv("winequality-red.csv")

x=sum(is.na(data))
if(x==0)
{
  cat("\nNo missing values in data set\n")
}else {
  print(x=map_int(data,~sum(is.na(.))))
}  

y=is.null(data)
if(y==FALSE)
{
  cat("\nNo Null values present in data\n\n")
}else {
  cat("Null values : \n\n")
  print(map(data,~sum(is.null(.))))
}

#----Plotting Histogram of Distributions of Value for all varibale----
melt_wine <- gather(data) # melt data to plot
#print(melt_wine)
q<-ggplot(gather(melt_wine), aes(value)) + 
  geom_histogram(color = "white",bins=30) + 
  facet_wrap(~key, scales = "free")+ggtitle("Univariate Plots")
print(q)

#print(summary(data))

#----------------------------Summary-------------------------------
cat("\n\n---Summary---\n\n")
f <- lapply(data, summary)
print(f)

cat("\n\nCount of quality\n\n")
print(dplyr::count(data, quality))

#----create correlation matrix and heatmap----
cormat <- round(cor(data),2)
lower_tri <- cormat
lower_tri[lower.tri(lower_tri)] <- NA #OR upper.tri function
lower_tri
melted_cormat <- melt(lower_tri, na.rm = TRUE)

#plot heatmap
p=ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_trans() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())+ggtitle("Heatmap for Correlation")
    
print(p)
#-------------------------------------------------------------------------------------------------
#classify wine in good , bad , normal
data$taste <- ifelse(data$quality < 5, "bad", "good")
data$taste[data$quality == 5] <- "normal"
data$taste[data$quality == 6] <- "normal"
data$taste<-as.factor(data$taste)

table_data=table(data$taste)
print(table_data)

#----Bivariate Plots----

melt_wine2 <- melt(data[,-which(names(data)=="quality")], "taste")
mix<-ggplot(melt_wine2, aes(taste,value)) +  
  geom_boxplot() + 
  facet_wrap(.~variable, scales = "free")+
  scale_x_discrete(limits = c("bad", "normal","good"))+ggtitle("Bivariate Plots")
print(mix)



dens <- ggplot(aes(x = pH, y = density), data = data) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  coord_trans(x = "log10") +
  geom_smooth(method = "lm", color = "red")

citr.ac <- ggplot(aes(x = pH, y = citric.acid), data = data) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  coord_trans(x = "log10") +
  geom_smooth(method = "lm", color = "red")

fix.ac <- ggplot(aes(x = pH, y = fixed.acidity), data = data) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  coord_trans(x = "log10") +
  geom_smooth(method = "lm", color = "red")

grid.arrange(dens, citr.ac, fix.ac, ncol = 2)

pos1 <- ggplot(aes(x = fixed.acidity, y = density), data = data) + 
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm", color = "red")

pos2 <- ggplot(aes(x = fixed.acidity, y = citric.acid), data = data) + 
  geom_jitter(alpha = 1/5) +
  geom_smooth(method = "lm", color = "red")

grid.arrange(pos1, pos2, ncol = 2)

#----split data----
set.seed(123)
samp<-sample(nrow(data),0.8*nrow(data))
train_data<-data[samp,]
test_data<-data[-samp,]

#----Multiple Linear Regression----

m1 <- lm(quality ~ alcohol*sulphates*citric.acid*fixed.acidity, data = train_data)

cat("\n\n-------------------------Summary of m1---------------------------\n")
print(summary(m1))
m2 <- update(m1, ~ . + density*fixed.acidity)
cat("\n\n-------------------------Summary of m2---------------------------\n")
print(summary(m2))
m3 <- update(m2, ~ . + volatile.acidity)
cat("\n\n-------------------------Summary of m3---------------------------\n")
print(summary(m3))

ols_plot_cooksd_chart(m3)
to.rm <- c(508,840)
train_data <- train_data[-to.rm,]
rownames(train_data) <- NULL

m4<-lm(quality ~ alcohol*sulphates*citric.acid*fixed.acidity+density*fixed.acidity+volatile.acidity, data = train_data)
cat("\n\n-------------------------Summary of m4---------------------------\n")
print(summary(m4))
cat("\nValue of rSquared for model m1 :")
print(summary(m1)$r.squared)
cat("\nValue of rSquared for model m2 :")
print(summary(m2)$r.squared)
cat("\nValue of rSquared for model m3 :")
print(summary(m3)$r.squared)
cat("\nValue of rSquared for model after removal of outliers :")
print(summary(m4)$r.squared)


#----Random forest algorithm----

random <- train(taste ~ alcohol*sulphates*citric.acid*fixed.acidity+density*fixed.acidity+volatile.acidity,
               data = train_data, 
               method = 'rf',
               trControl = trainControl(method = 'cv',
                                        number = 5))
print(random)

prediction <- predict(random, newdata = test_data)
results<-cbind(prediction,test_data$taste)

colnames(results)<-c('pred','real')

results<-as.data.frame(results)

View(results)





