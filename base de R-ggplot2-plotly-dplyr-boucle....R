install.packages("dplyr",dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("plotly",dependencies = T)
install.packages("caret",dependencies = T)
install.packages("e1071", dependencies = T)
install.packages("rpart", dependencies = T)
install.packages("randomForest", dependencies = T)

library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(e1071)
library(rpart)
library(randomForest)

class(2)
typeof(2)
class(3.5)
typeof(as.integer(2))
class("a")
typeof("a")

60 > 75
60 < 75

mon_caracter = "a"
class(mon_caracter)
mon_boolean = 3 > 1
3>1

mon_premier_nombre=10
mon_deuxieme_nombre=4
4+10
mon_premier_nombre + mon_deuxieme_nombre
mon_premier_nombre - mon_deuxieme_nombre
mon_premier_nombre * mon_deuxieme_nombre
mon_premier_nombre ** mon_deuxieme_nombre
mon_premier_nombre / mon_deuxieme_nombre
mon_premier_nombre %% mon_deuxieme_nombre

vecteur = c(1,2,3)
class(vecteur)
typeof(vecteur)
typeof(as.integer(vecteur))

c(1,"A",3)

vecteur1= seq(from=1 , to=10)
vecteur1
vecteur2= rep(10, times=10)
vecteur1 + 1
vecteur1 * 2
vecteur1/ vecteur2

concatenation =c(vecteur1,vecteur2)
concatenation

vecteur2[2]
vecteur1[2]
vecteur1[1:3]
vecteur2[c(1,2,3)]
superieur = vecteur1 > 3
superieur[vecteur1]
vecteur1[superieur]

mean(vecteur1)
?mean
length(vecteur2)
length(vecteur1)
names(vecteur1)= paste("ma valeur", vecteur1, sep = " ")

vecteur3= c(5,2, 9, 3)
sort(vecteur3)
sort(vecteur3, decreasing = TRUE)
sort(vecteur3, decreasing = FALSE)

rank(vecteur1)
rank(concatenation)

sum(vecteur1)
max(vecteur1)
min(vecteur1)
mean(vecteur1)
summary(vecteur1)
median(vecteur1)

help(mean)
?mean
help.start()
help.search("mean")

names(moyenne_de_classe)=c("lala", "ouZ","epek", "eylul")
moyenne_de_classe=sample(1:20,4)
moyenne_de_classe
mean(moyenne_de_classe)
min(moyenne_de_classe)
max(moyenne_de_classe)
moyenne_de_classe[moyenne_de_classe>mean(moyenne_de_classe)]
table_recaputilatif=c(mean(moyenne_de_classe),min(moyenne_de_classe),max(moyenne_de_classe),
                      length(moyenne_de_classe>mean(moyenne_de_classe)))
table_recaputilatif
names(table_recaputilatif)= c("moyenne", "mauvaise note", "meilleur note", "nombre d'étudiant ayant une note supérieur à 10")
table_recaputilatif
moyenne_de_classe[moyenne_de_classe==max(moyenne_de_classe)]
sort(moyenne_de_classe,decreasing = TRUE)
summary(moyenne_de_classe)

notes=sample(1:20,15)
notes
matrix(notes, ncol = 3,nrow = 5)
matrix(notes, ncol=3,nrow = 5, byrow = TRUE)
notes_eleve=matrix(notes, ncol = 3, nrow = 5)
colnames(notes_eleve)= c("SVT", "math","français")
rownames(notes_eleve)=c("lala", "eva", "boba", "lina","poki")
notes_eleve
notes_eleve[1,]
notes_eleve[,1]
notes_eleve[2,2]
notes_eleve[1,c(2,3)]
notes_eleve[3,c(1,2,3)]
notes_eleve["lala","math"]
notes_eleve[c("lala","eva"),c("math","français")]

notes_eleve[c("lala","eva"), "français"]= c(17,14)
notes
notes[2]=13
notes[2]

notes2 = c(rep(0.5,times=5),rep(1,times=5),rep(2,times=5))
notes2
matrice_des_notes=matrix(notes2, ncol = 3, nrow = 5)
matrice_des_notes

notes_eleve * matrice_des_notes
notes_eleve + matrice_des_notes
notes_eleve / matrice_des_notes
notes_eleve - matrice_des_notes

rowsums(notes_eleve)
rowsums(notes_eleve)
rowSums(notes_eleve)
colSums(notes_eleve)
rowMeans(notes_eleve)
colMeans(notes_eleve)

install.packages("Stat2Data")
library(Stat2Data)

data("HorsePrices")

matrice_PrixCheval=as.matrix(HorsePrices[,-c(1,5)])
rownames(matrice_PrixCheval)=HorsePrices[,1]
colnames(matrice_PrixCheval)=c("prix","age","taille")
matrice_PrixCheval[,3]=matrice_PrixCheval[,3]*0.1016
matrice_PrixCheval[,1]=matrice_PrixCheval[,1]*0.86
length(matrice_PrixCheval)
dim(matrice_PrixCheval)
summary(matrice_PrixCheval)
matrice_PrixCheval=na.omit(matrice_PrixCheval)
dim(matrice_PrixCheval)

matrice_PrixCheval[matrice_PrixCheval[,1]==946,]
matrice_PrixCheval[matrice_PrixCheval[,1]==51600,]
matrice_PrixCheval[matrice_PrixCheval[,1]==946,]=c(2500,19,1.67)
matrice_PrixCheval[matrice_PrixCheval[,1]==2500,]
dim(matrice_PrixCheval[matrice_PrixCheval[,1]>24000,])
dim(matrice_PrixCheval[matrice_PrixCheval[,3]>1.659,])
colMeans(matrice_PrixCheval[matrice_PrixCheval[,3]>1.6,])
colMeans(matrice_PrixCheval[matrice_PrixCheval[,3]<=1.6,])
colSums(matrice_PrixCheval)


dataframe=data_frame(c(18,6,45,78), c(65,84,76,62), c("F","M","M","F"), c(TRUE,TRUE,FALSE,TRUE))
colnames(dataframe)=c("age","taille","sex","boolean")
rownames(dataframe)=c("eva","tata","rita","rima")
dataframe

data_iris<-iris
data_iris[,1]
data_iris[,c(1:3)]
head(data_iris[,c(1:3)])
data_iris[c(1,50,60),c(1:3)]

data_iris
data_iris[,1]
data_iris[,c(1:3)]
head(data_iris[,c(1:3)])
data_iris[c(1,47,34),c(1:3)]
data_iris[c(1,34,47),c("Sepal.Length", "Petal.Length")]
data_iris$Sepal.Length

data_iris[which(data_iris$Species=="setosa"),]
dim(data_iris[data_iris$Species=="virginica",])
data_iris[which(data_iris$Species=="setosa"& data_iris$Petal.Length==14),]
data_iris[data_iris$Species %in% c("setosa"),]

data_iris[data_iris$Species=="setosa",]
data_iris[which(data_iris$Species=="setosa"),]
data_iris[data_iris$Species %in% c("setosa"),]
subset(data_iris, Species=="setosa",select=c("Petal.Length"))


class(data_iris$Species)
head(data_iris$Species)
rownames(data_iris)=paste("iris-",rownames(data_iris),sep = " ")
colnames(data_iris)=c("Longueur des sepales","largeur des sepales","longueur des petales","largeur des petals","Espece")
summary(data_iris)
min(data_iris[data_iris$Espece =="setosa",3])
mean(data_iris[data_iris$Espece =='virginica',3])

class(iris$Species)
head(iris$Species)
rownames(iris) = paste("iris-",rownames(iris), sep = "")
colnames(iris) = c("longueur des sepals", "largeur des sepals","longueur des petals", "largeur des petals", "Espece")
summary(iris)
min(iris[iris$Espece=="setosa",3])
mean(iris[iris$Espece=="virginica","longueur des petals"])

data_iris_quali=as.data.frame(data_iris[,c(5)])
data_iris_quali
data_iris_quanti=data_iris[,c(1:4)]


data_iris_complet=merge(data_iris_quanti,data_iris_quali,by="row.names")

if(5<3 & 2<3){
  print("ok")
} else{
  print("pas ok")
}

for (valeur in c(1,2,3,4,5,6)) {
  print(valeur+1)
}
for (valeur in c(1,2,3,4,5,6)) {
  print(paste("numero:" ,valeur+1))
}

for (element in c(1,2,3,4,5,6)) {
  if (element > 1 & element<5){
    print(element)
  }
  
}
valeur=200
while (valeur/5>1) {
  valeur=valeur/5
  print(valeur)
}

if(5>3 &  2<3){
  print("ok")
} else{
  print("pas ok")
}

for (valeur in c(1,2,3,4,5,6)) {
  print(paste("numero:", valeur+1))
  
}

valeur=100
while (valeur/2>1) {
  valeur=valeur/2
  print(valeur)  
}


data(iris)
#combien d'individu dans le tableau

compteur=0
for (length in iris$Sepal.Length) {
  if(length>=5){
    compteur=compteur+1
  }
  
}
print(paste("le nombre de membre est :", compteur))

cp_setosa=0
cp_virginica=0
cp_versicolor=0
for (length in iris$Species) {
  if(length=="setosa"){
    cp_setosa=cp_setosa+1
  } else
    if(length=="virginica"){
      cp_virginica=cp_virginica+1
    }else
      if(length=="versicolor"){
        cp_versicolor=cp_versicolor+1
      }
}
print(paste("le nombre de setosa est :", cp_setosa))
print(paste("le nombre de virginica est :", cp_virginica))
print(paste("le nombre de versicolor est :", cp_versicolor))

#parcourir tout le tableau

dim(iris)
n_setosa=0
for (ligne in 1:dim(iris)[1]) {
  individu=iris[ligne,]
  if(individu$Species=="setosa" & individu$Sepal.Length >=5){
    n_setosa=n_setosa+1
  }
  
}
print(paste("le nombre de setosa est : ",n_setosa))


dim(iris)
for (ligne in 1:dim(iris)[1]) {
  individu=iris[,ligne]
  print(individu)
}


dim(iris)
for (colonne in 1:dim(iris[1])) {
  individu=iris[,colonne]
  print(individu)
}

apply(iris[,-5], 1,mean)
by(iris,iris$Species,mean)
aggregate(iris[,-5],as.data.frame(iris$Species),mean)

data_iris=as_tibble(iris)
select(iris,Sepal.Length,Sepal.Width,Species)
select(iris,Sepal.Length:Species)

select(data_iris, -Species)
select(iris, starts_with("Petal"))
select(iris, starts_with("Sepal"))
select(iris, ends_with("Length"))
select(iris, ends_with("Width"))
select(iris,Species)
select(iris, contains("al"))
select(iris, contains("ies"))

filter(iris, Sepal.Length>5,Species=="setosa")
filter(iris, between(Petal.Length,4,7))
filter(iris,Species=="setosa")
filter(iris, Species!="setosa")
filter(iris, Species %in% c("setosa","virginica"))
filter(iris, Species=="setosa" | Species=="virginica")


filter_all(iris[,-5], any_vars(.>5))
filter-all(iris[,-5],all_vars(.>5))

iris %>% 
  select(Petal.Length,Petal.Width,-Species) 
    %>% 
  filter_all(all_vars(.>2))

iris %>% 
  arrange(Sepal.Length)

iris %>% 
  arrange(desc(Sepal.Length))

iris %>% 
  arrange(Sepal.Length,Petal.Length)

iris %>% 
  summarise(moyenne=mean(Sepal.Length),
            Min=min(Petal.Length),
            total= n())

iris %>%  
  summarise_each(funs(mean,sum),Sepal.Length,Petal.Length)

iris %>% 
  group_by(Species=="setosa") %>% 
  select(Sepal.Length) %>% 
  filter(Sepal.Length>5)

iris %>% 
  mutate(somme_longueur_largeur_petal= Petal.Length+ Petal.Width,
         somme_longueur_largeur_Sepal=Sepal.Length + Sepal.Width)

iris %>% 
  mutate(Species=NULL)

iris %>% 
  mutate(Sepal.Length=Sepal.Length*2)

#exercice
FastFood<- read.csv("FastFoodRestaurants.csv")
FastFood = as_tibble(FastFood)

#les 5 villes qui ont plus de fast food 

FastFood %>% 
  group_by(city) %>% 
  summarise(nombre_de_fast_food=length(city)) %>% 
  arrange(desc(nombre_de_fast_food)) %>% 
  head(n=5)

# les fast food es plus présents dans les 5 premiers villes

city_list=FastFood %>% 
group_by(city) %>% 
summarise(nombre_de_fast_food=length(city)) %>% 
arrange(desc(nombre_de_fast_food)) %>% 
head(n=5) %>% 
pull(city)

city_list

FastFood %>%
  filter(city %in% city_list)

FastFood %>% 
  filter(city %in% city_list) %>% 
  group_by(name) %>% 
  summarise(nombre_de_name=length(name)) %>% 
  arrange(desc(nombre_de_name)) %>% 
  head(n=5)

FastFood %>% 
  group_by(name) %>% 
  summarise(nombre_de_name=length(name)) %>% 
  arrange(desc(nombre_de_name)) %>% 
  head(n=5)

FastFood %>% 
  group_by(city) %>% 
  select(city,name) %>% 
  filter(name=="McDonald's") %>% 
  summarize(nombre_de_fast_food=length(city)) %>% 
  arrange(desc(nombre_de_fast_food))

FastFood %>% 
  group_by(city) %>% 
  select(city,name) %>% 
  filter(city=="New York") %>% 
  summarize(nombre_de_fast_food=length(city))

FastFood %>% 
  group_by(name) %>% 
  select(city,name) %>% 
  filter(city=="New York") %>% 
  summarize(nombre_de_namefastfood=length(name)) %>% 
  arrange(desc(nombre_de_namefastfood)) %>% 
  head(n=5)

#visualisation des données
#package plotly
data(iris)
plot(iris$Sepal.Length,iris$Sepal.Width)
plot(iris$Sepal.Length,iris$Sepal.Width,xlab = "longueur des sepales",ylab = "largeur des sepales")
plot(iris$Petal.Length,iris$Petal.Width,xlab = "longueur des petales",ylab = "largeur des petales",col="red")
lines(iris$Sepal.Length,iris$Sepal.Width,xlab = "longueur des sepales",ylab = "largeur des sepales",col="blue",type = "p")
plot(iris$Petal.Length,iris$Petal.Length,xlab = "longueur des petales",ylab = "largeur des petales",col="red",
xlim=c(min(iris$Sepal.Length,iris$Petal.Length),max(iris$Sepal.Width,iris$Petal.Width)),
ylim = c(min(iris$Sepal.Length,iris$Petal.Length),max(iris$Sepal.Width,iris$Petal.Width))

title(main = "longueur en fonction de la largeur", col="blue")

#ggplot2

ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width))+
  geom_point()

ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,color=iris$Species))+
  geom_point()
g<-ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,color=Sepal.Length,shape=Species))+
  geom_point(size=3)
g+
g + scale_color_gradient(low = "blue",high = "red")

ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,color=Sepal.Length,shape=Species))+
  geom_point(size=3)+scale_color_gradient(low = "orange",high = "green")+theme_minimal()+theme(legend.position = "right")+
  theme(legend.title = element_blank())

ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,color=Sepal.Length,shape=Species))+
  geom_point(size=3)+scale_color_gradient(low = "orange",high = "green")+theme_minimal()+
  theme(legend.position = "right")+
  theme(legend.title = element_text(colour = "red",size = 9,face = "bold"))+
  theme(legend.text = element_text(size =9,face = "bold.italic"))+
  theme(legend.background = element_rect(fill="pink",colour ="black",size=1,linetype = "dotted"))

#axes et titres
ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,color=Sepal.Length,shape=Species))+
  geom_point(size=3)+scale_color_gradient(low = "orange",high = "green")+theme_minimal()+
  xlab("longueur de sepales")+ylab("largeur de sepales")+
  ggtitle("longeur en fonction de la largeur de sepales")+
  theme(legend.position = "right")+
  theme(legend.title = element_text(colour = "red",size = 9,face = "bold"))+
  theme(legend.text = element_text(size =9,face = "bold.italic"))+
  theme(legend.background = element_rect(fill="pink",colour ="black",size=1,linetype = "dotted"))+
  theme(plot.title = element_text(size = 15,face = "bold.italic",colour = "red"))+
  theme(axis.title = element_text(size=9,face="bold",colour="red"))+
  theme(axis.text = element_text(size=8,face = "bold",angle = 50))+
  theme(axis.line = element_line(size = 1,linetype = "dotted",colour = "black"))+
  facet_wrap(~Species)+
  facet_wrap(~Species, ncol = 1)

# histogrammes sur les données iris

g<- ggplot(iris,aes(x=Petal.Length))+geom_histogram(binwidth = 0.5)
g

ggplot(iris,aes(x=Petal.Length,fill=Species))+geom_histogram(binwidth = 0.5)
ggplot(iris,aes(x=Petal.Length,fill=Species))+geom_histogram(binwidth = 0.5,color="white")

#exporter le praph
png("histogramme_iris.png")
ggplot(iris,aes(x=Petal.Length,fill=Species))+geom_histogram(binwidth = 0.5,color="white")
dev.off()


#boxplot
pdf("boxplot_iris.pdf")
ggplot(iris,aes(x=Species,y=Petal.Length,fill=Species))+geom_boxplot()+ 
  ggtitle("boxplot de la longeur des petales selon l'espece")+
  xlab("espece")+
  ylab("longueur des petales")
dev.off()

#exercice fastfood

FastFood
FastFood_tibble= as_tibble(FastFood)

city_list=FastFood_tibble %>% 
  group_by(city) %>% 
  summarise(ville_plus_de_fastfood=length(city)) %>% 
  arrange(desc(ville_plus_de_fastfood)) %>% 
  head(n=10) %>% 
  pull(city)

villes_10_fastfood=FastFood_tibble %>% 
  filter(city %in% city_list)



name_fastfood_list=villes_10_fastfood %>% 
  group_by(name) %>% 
  summarise(name_de_fastfood=length(name)) %>% 
  arrange(desc(name_de_fastfood)) %>% 
  head(n=10) %>% 
  pull(name)

name_fastfood_10list=villes_10_fastfood %>% 
  filter(name %in% name_fastfood_list)


name_fastfood_10list

pdf("fastfood.pdf")
g<-ggplot(name_fastfood_10list,aes(x=city,fill=name))+geom_bar()+theme_minimal()+
  ggtitle("nom des fastfood les plus présent \n dans les 10 villes")+
  xlab("villes")+ylab("les 10 resto les plus implantés")+
  theme(axis.text = element_text(angle = 50,size=6))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,100)+theme(legend.title = element_blank())+
  scale_fill_brewer(palette = "Paired")
dev.off()
g

#rendre un praph interactive
library(plotly)
ggplotly(g)

data(iris)
g<-ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+geom_boxplot()
g
ggplotly(g)





































