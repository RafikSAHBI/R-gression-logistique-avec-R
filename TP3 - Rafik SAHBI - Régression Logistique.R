evans <- read.table("http://web1.sph.emory.edu/dkleinb/allDatasets/datasets/evans.dat")
Data=evans
Data
head(Data)
names(Data) <- c("id","chd","cat","age","chl","smk","ecg","dbp","sbp","hpt","ch","cc")
head(Data)
Y=Data$chd
Y
X=Data$age
X
Reg1=glm(Y~X, family=binomial(link=logit))
Reg1
summary(Reg1)
Coeff1=coefficients(Reg1)
Coeff1
beta1=round(Coeff1[2],6)
beta1
beta0=round(Coeff1[1],6)
beta0
X_lab=expression(Age)
X_lab
Y_lab = expression(CHD)
Y_lab
Titre=paste("Régression Logistique du CHD en fonction de l'Age")
Titre
plot(Y~X, xlab=X_lab, ylab=Y_lab, col="blue", main=Titre)
Droite=beta1*X+beta0
Droite
Logit=exp(Droite)/(1+ exp(Droite))
Logit
points(X,Logit, col="red")
o=order(X)
points(X[o],Logit[o], col="red", type="l", lwd=2)

T=Data$cat
T
Reg2=glm(Y~X+T, family=binomial(link=logit))
Reg2
summary(Reg2)

P=Data$chl
P
Q=Data$smk
Q
R=Data$ecg
R
S=Data$hpt
Reg3=glm(Y~X+T+P+Q+R+S, family=binomial(link=logit))
Reg3
summary(Reg3)
