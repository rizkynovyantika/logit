disabilitas=matrix(c(0,0,0,0,1,1,2,0,3,5,1,1,1,0,2,0,2,4,0,3,3,1,0,3,3,0,0,1,3,0,1,1,1,1,4,1,0,1,2,0,3,5,0,1,2,
0,2,1,1,2,0,1,0,0,2,1,2,5,1,2,0,1,0,0,1,1,0,0,0,0,3,0,2,0,0,0,2,1,0,2,1,1,0,0,0,0,1,2,0,2,1,0,0,0,1,
2,2,2,5,4,4,1,5,2,2,1,1,4,1,0,1,0,2,0,3,0,1,0,0,4,0,4,0,0,0,1,3,3,0,1,1,2,2,3,2,3,0,2,0,4,0,1,1,0,3,
0,0,0,0,0,1,0,0,0,0,0,32,24,44,27,21,32,50,23,23,55,4,10,10,16,35,9,18,36,8,37,38,48,16,42,16,32,31,
37,23,55,28,26,14,13,32,4,28,45,11,13,9,31,8,23,11,11,12,36,31,39,9,28,33,19,11,10,19,42,16,24,30,33,
22,24,24,6,33,4,9,6,26,17,4,6,4,8,9,8,8,6,4,4,5,7,9,7,7,19,1,5,4,6,9,0,6,6,6,0,9,8,5,3,5,8,9,3,5,23,7,
10,5,5,4,2,7,15,4,3,3,11,1,8,2,0,0,11,8,12,2,6,9,2,6,3,9,9,8,12,8,11,11,4,7,0,8,1,1,1,5,4,1,0,5,3,2,3),
nrow=78,ncol=4,byrow=FALSE)
disabilitas

rownames(disabilitas)=c('TEMON','WATES','PANJATAN','GALUR','LENDAH','SENTOLO','PENGASIH','KOKAP','GIRIMULYO','NANGGULAN','SAMIGALUH','KALIBAWANG','SRANDAKAN','
SANDEN','KRETEK','PUNDONG','BAMBANG LIPURO','PANDAK','PAJANGAN','BANTUL','
JETIS','IMOGIRI','DLINGO','BANGUNTAPAN','PLERET','PIYUNGAN','SEWON','KASIHAN','
SEDAYU','WONOSARI','NGLIPAR','PLAYEN','PATUK','PALIYAN','PANGGANG','TEPUS','SEMANU','
KARANGMOJO','PONJONG','RONGKOP','SEMIN','NGAWEN','GEDANGSARI','SAPTOSARI','
GIRISUBO','TANJUNGSARI','PURWOSARI','GAMPING','GODEAN','MOYUDAN','MINGGIR','
SEYEGAN','MLATI','DEPOK','BERBAH','PRAMBANAN','KALASAN','NGEMPLAK','NGAGLIK','
SLEMAN','TEMPEL','TURI','PAKEM','CANGKRINGAN','TEGALREJO','JETIS','GONDOKUSUMAN','
DANUREJAN','GEDONGTENGEN','NGAMPILAN','WIROBRAJAN','MANTRIJERON','KRATON','GONDOMANAN','
PAKUALAMAN','MERGANGSAN','UMBULHARJO','KOTAGEDE')
colnames(disabilitas)=c('ADK.MJ','ADK.MF','PD.MJ','PD.MF')
disabilitas

#------------------------------Outlier----------------------------------------
library(MVN)
hasil<- mvOutlier(disabilitas, qqplot=TRUE, method ="quan")
disabilitas_baru =hasil$newData
disabilitas_baru



#pengecekan tipe data
str(disabilitas)
#eksplorasi data
summary(disabilitas)
#jarak
jarak=dist(scale(disabilitas))
jarak

#----------------------------METODE AVERAGE------------------------------------
hierarkiave<-hclust(dist(scale(disabilitas)), method="ave")
hierarkiave
windows()
plot(hierarkiave) #dendogram

rect.hclust(hierarkiave,3) 		#plot mengelompokkan data
anggotaave<-cutree(hierarkiave,k=3) #hasil kelompok data
anggotaave

tabulasiave<-data.frame(disabilitas,anggotaave) # hasil kelompok data dalam bentuk data frame
View(tabulasiave)

cophenetic(hierarkiave) #jarak cophenetic average
#korelasi cophenetic
d1 <- dist(disabilitas)
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
corave=cor(d1, d2)
corave

#-----------------------------METODE COMPLETE----------------------------------
hierarkicomp<-hclust(dist(scale(disabilitas)), method="complete")
hierarkicomp
windows()
plot(hierarkicomp) #dendogram

rect.hclust(hierarkicomp,3) 		  #plot mengelompokkan data
anggotacomp<-cutree(hierarkicomp,k=3) #hasil kelompok data
anggotacomp

tabulasicomp<-data.frame(disabilitas,anggotacomp) # hasil kelompok data dalam bentuk data frame
View(tabulasicomp)

cophenetic(hierarkicomp) #jarak cophenetic complete 
#korelasi cophenetic
d1 <- dist(disabilitas)
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
corcomp=cor(d1, d2)
corcomp

#-------------------------------METODE SINGLE----------------------------------
hierarkising<-hclust(dist(scale(disabilitas)), method="single")
hierarkising
windows()
plot(hierarkising) #dendogram

rect.hclust(hierarkising,3) 		  #plot mengelompokkan data
anggotasing<-cutree(hierarkising,k=3) #hasil kelompok data
anggotasing

tabulasising<-data.frame(disabilitas,anggotasing) # hasil kelompok data dalam bentuk data frame
View(tabulasising)

cophenetic(hierarkising) #jarak cophenetic single
#korelasi cophenetic
d1 <- dist(disabilitas)
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
corsing=cor(d1, d2)
corsing

#---------------------------------METODE WARD----------------------------------
hierarkiward<-hclust(dist(scale(disabilitas)), method="ward.D")
hierarkiward
windows()
plot(hierarkiward) #dendogram

rect.hclust(hierarkiward,3) 		  #plot mengelompokkan data
anggotaward<-cutree(hierarkiward,k=3) #hasil kelompok data
anggotaward

tabulasiward<-data.frame(disabilitas,anggotaward) # hasil kelompok data dalam bentuk data frame
View(tabulasiward)

cophenetic(hierarkiward) #jarak cophenetic ward
#korelasi cophenetic
d1 <- dist(disabilitas)
hc <- hclust(d1, "ward.D")
d2 <- cophenetic(hc)
corward=cor(d1, d2)
corward

#-------------------------------METODE CENTROID--------------------------------
hierarkicent<-hclust(dist(scale(disabilitas)), method="centroid")
hierarkicent
windows()
plot(hierarkicent) #dendogram

rect.hclust(hierarkicent,3) 		  #plot mengelompokkan data
anggotacent<-cutree(hierarkicent,k=3) #hasil kelompok data 
anggotacent

tabulasicent<-data.frame(disabilitas,anggotacent) # hasil kelompok data dalam bentuk data frame
View(tabulasicent)

cophenetic(hierarkicent) #jarak cophenetic centroid
#korelasi cophenetic
d1 <- dist(disabilitas)
hc <- hclust(d1, "centroid")
d2 <- cophenetic(hc)
corcent=cor(d1, d2) 
