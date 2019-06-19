mydata<-mtcars[-1]
mydata<-scale(mydata)
mypca<-prcomp(mydata)
summary(mypca)

plot(mypca,type="l",col=c("blue"))
print(mypca$rotation)
cor(mypca$x)


mydata_pca<-cbind(mtcars[,1],as.data.frame(mypca$x))
mydata_pca

lm_pca_mtcars<-lm(mtcars[,1]~PC1+PC2+PC3,data=mydata_pca)
summary(lm_pca_mtcars)
pre_mtcars<-predict(lm_pca_mtcars,mydata_pca)
gg1<-data.frame(pre_mtcars,mydata_pca$`mtcars[, 1]`)
gg1<-mutate(gg1,e2=(pre_mtcars-mydata_pca$`mtcars[, 1]`)^2)
mb1<-mean(gg1$e2)
rms1<-sqrt(mb1)
rms1
