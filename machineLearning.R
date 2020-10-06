#machine learning

#lecture de fichier de donnée
bank_data=read.csv("bank.csv",sep = ";")
summary(bank_data)

#visualisation des données
library(ggplot2)
g<- ggplot(bank_data,aes(x=y,y=duration,fill=y))+geom_boxplot()
g
ggplotly(g)

g<- ggplot(bank_data,aes(x=y,y=age,fill=y))+geom_boxplot()
g
ggplotly(g)

g<- ggplot(bank_data,aes(y,fill=contact))+geom_bar()
g

#création d'un jeu de données d'entrainement et de test
install.packages("caret")
library("caret")

dummy_variables=dummyVars(~.,data = bank_data)
dummy_variables_data=predict(dummy_variables,newdata=bank_data)
class(dummy_variables_data)
dummy_variables_data=as.data.frame(dummy_variables_data)
dummy_variables_data$"souscription"=ifelse(dummy_variables_data$"y.no"==1, "no", "yes")
dummy_variables_data$"y.no"=NULL
dummy_variables_data$"y.yes"=NULL

#création d'un jeu de donnée d'entrainement et de test

set.seed(3033)
taille_du_jeu_entrainement=floor(0.7*nrow(dummy_variables_data))
head(taille_du_jeu_entrainement)
nrow(dummy_variables_data)
indices=sample(seq_len(nrow(dummy_variables_data)), size = taille_du_jeu_entrainement)
length(indices)
head(indices)
data_bank_entrainement=dummy_variables_data[indices,]
data_bank_test=dummy_variables_data[-indices,]
dim(data_bank_entrainement)
dim(data_bank_test)

#normalisation

data_preprocess_value=preProcess(data_bank_entrainement,method = c("center","scale"))
data_bank_entrainement_scaled=predict(data_preprocess_value,data_bank_entrainement)
data_bank_test_scaled=predict(data_preprocess_value,data_bank_test)
head(data_bank_entrainement_scaled)

#comment faire si on a des données deséquilibré
#caret = downsample et upsample

table(data_bank_entrainement_scaled[,"souscription"])
set.seed(3033)
'%ni%' = Negate("%in%")

#downsample
data_bank_entrainement_scaled_downsample=downSample(data_bank_entrainement_scaled
                              [,colnames(data_bank_entrainement_scaled) %ni% "souscription"]
                              ,y=as.factor(data_bank_entrainement_scaled$"souscription"))
head(data_bank_entrainement_scaled[,colnames(data_bank_entrainement_scaled) %ni% "souscription"])
head(data_bank_entrainement_scaled_downsample)
dim(data_bank_entrainement_scaled_downsample)
dim(data_bank_entrainement_scaled)
names(data_bank_entrainement_scaled_downsample)[names(data_bank_entrainement_scaled_downsample)=="Class"]="souscription"
table(data_bank_entrainement_scaled_downsample[,"souscription"])

#upsample

data_bank_entrainement_scaled_upsample=upSample(data_bank_entrainement_scaled
                                                    [,colnames(data_bank_entrainement_scaled) %ni% "souscription"]
                                                    ,y=as.factor(data_bank_entrainement_scaled$"souscription"))
head(data_bank_entrainement_scaled[,colnames(data_bank_entrainement_scaled) %ni% "souscription"])
head(data_bank_entrainement_scaled_upsample)
dim(data_bank_entrainement_scaled_upsample)
dim(data_bank_entrainement_scaled)
names(data_bank_entrainement_scaled_upsample)[names(data_bank_entrainement_scaled_upsample)=="Class"]="souscription"
table(data_bank_entrainement_scaled_upsample[,"souscription"])


#modélisation avec Naives Bayes
set.seed(3033)
trainControl_data=trainControl(method = "repeatedcv", number = 10,repeats = 3)
naives_bayes_desequilibree=train(souscription ~., data=data_bank_entrainement_scaled, method="nb", preProcess=NULL)
print(naives_bayes_desequilibree)

#prédiction avec notre modèle sur le jeu de donnée tests

prediction_naive_bayes_desequilibree=predict(naives_bayes_desequilibree,newdata =data_bank_test_scaled[,-ncol(data_bank_test_scaled)])

#création de matrice de confusion 
head(prediction_naive_bayes_desequilibree)
confusionMatrix(prediction_naive_bayes_desequilibree,as.factor(data_bank_test_scaled[,ncol(data_bank_test_scaled)]))
head(data_bank_test_scaled[,ncol(data_bank_test_scaled)])

#modélisation avec Naives Bayes sur les données downsample
set.seed(3033)
trainControl_data=trainControl(method = "repeatedcv", number = 10,repeats = 3)
naives_bayes_downsample=train(souscription ~., data=data_bank_entrainement_scaled_downsample, method="nb", preProcess=NULL)
print(naives_bayes_downsample)

#prédiction avec notre modèle sur le jeu de donnée tests

prediction_naive_bayes_downsample=predict(naives_bayes_downsample,newdata =data_bank_test_scaled[,-ncol(data_bank_test_scaled)])

#création de matrice de confusion 
head(prediction_naive_bayes_downsample)
confusionMatrix(prediction_naive_bayes_downsample,as.factor(data_bank_test_scaled[,ncol(data_bank_test_scaled)]))
head(data_bank_test_scaled[,ncol(data_bank_test_scaled)])




#modelication du jeu de donnée avec SVM(données deséquilibrées)
set.seed(3033)
trainControl_data=trainControl(method = "repeatedcv", number = 10,repeats = 3)
SVM_desequilibree=train(souscription ~., data=data_bank_entrainement_scaled, method="svmLinear", preProcess=NULL)
print(SVM_desequilibree)

#prédiction avec notre modèle sur le jeu de donnée tests

prediction_SVM_desequilibree=predict(SVM_desequilibree,newdata =data_bank_test_scaled[,-ncol(data_bank_test_scaled)])

#création de matrice de confusion 
head(prediction_SVM_desequilibree)
confusionMatrix(prediction_SVM_desequilibree,as.factor(data_bank_test_scaled[,ncol(data_bank_test_scaled)]))
head(data_bank_test_scaled[,ncol(data_bank_test_scaled)])

#varImp
varImp(naives_bayes_downsample, scale = F)



