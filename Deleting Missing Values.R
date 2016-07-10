library(ggplot2)
library(plyr)
library(reshape2)
library(pROC)
library(Deducer)
library(neuralnet)
library(nnet)
library(e1071)
library(xgboost)
hepa = read.csv("hepatitis.csv")

#Finding missing values
na_count_hepa <-as.data.frame(sapply(hepa, function(y) sum(length(which(is.na(y))))))
colnames(na_count_hepa) <- "NA Count"
na_count_hepa$Pct_Missing <- ((na_count_hepa$`NA Count`)/nrow(hepa))*100

#Omitting NA values (as described in the paper)
hepa = na.omit(hepa)

#Standardising Variables
range <- function(x){(x-min(x))/(max(x)-min(x))}
hepa$BILIRUBIN = range(hepa$BILIRUBIN)
hepa$ALK_PHOSPHATE = range(hepa$ALK_PHOSPHATE)
hepa$SGOT = range(hepa$SGOT)
hepa$ALBUMIN = range(hepa$ALBUMIN)
hepa$PROTIME = range(hepa$PROTIME)

hepa[,1] = as.factor(hepa[,1])

#Exploratory Data Analysis
name = "PROTIME"
var_meds = ddply(hepa, .(Class), summarise, med = median(hepa[[name]]))
d = ggplot(hepa,aes(x = hepa[[name]])) + geom_density() +xlab("Value") + ylab(name)+title(paste0("Density curve for ",name))
b = ggplot(hepa,aes(x = as.factor(hepa$Class), y = hepa[[name]])) +   geom_boxplot() + geom_text(data = var_meds, aes(x = as.factor(var_meds$Class), y = med, label = med), size = 3, vjust = -1.5) + xlab("Class") + ylab(name)
ggsave(paste0(name,"Density Curve.png"),plot=d)
ggsave(paste0(name,"Box Plot.png"),plot=b)


#Correlation Heatmap
mydata = hepa[,2:20]
cormat = round(cor(mydata),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+ geom_tile(color = "white")+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +theme_minimal()+theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+coord_fixed()


#Applying SVM
## set the seed to make your partition reproductible
train = hepa[,c(-1)]
svm_model <- svm(Class ~ ., data=hepa,kernel="linear", ranges=list(cost=2^(1:10), gamma=c(.05,0.5,1)))
pred = predict(svm_model,train)
train$Class_Predict = pred
confusionMatrix(hepa$Class,train$Class_Predict)
output = as.data.frame(hepa$Class)
colnames(output) = "Actual Class"
output$Predict = train$Class_Predict
output$`Actual Class` = as.numeric(output$`Actual Class`)
output$Predict = as.numeric(output$Predict)
auc(roc(output$`Actual Class`,output$Predict))
plot(svm_model,hepa,AGE~ALK_PHOSPHATE) + labels(hepa)

x <- importance(svm_model)
#Applying Decision Trees


