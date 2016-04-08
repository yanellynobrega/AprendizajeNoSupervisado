################# a.csv ############################
data_a = read.csv('a.csv', header = F);
colnames(data_a) <- c("X","Y","CLASS");
data_a$CLASS <- data_a$CLASS + 1;
data_a <- data_a[order(data_a$CLASS), ]
#################################################

############ kmedias a.csv #########################
km_a <- kmeans(data_a[,1:2], 3)

plot(data_a$X, data_a$Y, col = km_a$cluster)
points(km_a$centers[, c("X","Y")], col = 1:3, pch = 19, cex = 2)
MatrizConfusionK_a <- table(true = data_a$CLASS, predicted = km_a$cluster)
MatrizConfusionK_a
ka = 0;
for (i in 1:ncol(MatrizConfusionK_a)){
  ka = ka + MatrizConfusionK_a[i,i]
}
aciertos_ka <- (ka / nrow(data_a))*100
aciertos_ka
####################################################


############ Cluster jerarquicos (a.csv) ###########
data_a1 = data_a
data_a1$CLASS <- NULL
data_a1= as.matrix(data_a1)
distancia = dist(data_a1)
####################################################



############single method (a.csv) ##################
cluster_as <- hclust(distancia, method="single")
corte_as <- cutree(cluster_as, k=3)
plot(x = data_a[,1], y <- data_a[,2],col = corte_as)
MatrizConfusion_as <- table(true = data_a$CLASS, predicted = corte_as)
MatrizConfusion_as
as = 0
for (i in 1:ncol(MatrizConfusion_as)){
  as = as + MatrizConfusion_as[i,i]
}
aciertos_as <- (as / nrow(data_a))*100
aciertos_as
######################################################


############complete method (a.csv) ##################
cluster_ac <- hclust(distancia, method="complete")
corte_ac <- cutree(cluster_ac, k=3)
plot(x = data_a[,1], y <- data_a[,2],col = corte_ac)
MatrizConfusion_ac <- table(true = data_a$CLASS, predicted = corte_ac)
MatrizConfusion_ac
ac = 0
for (i in 1:ncol(MatrizConfusion_ac)){
  ac = ac + MatrizConfusion_ac[i,i]
}
aciertos_ac <- (ac / nrow(data_a))*100
####################################################


############ average method (a.csv) ##################
cluster_aa <- hclust(distancia, method="average")
corte_aa <- cutree(cluster_aa, k=3)
plot(x = data_a[,1], y <- data_a[,2],col = corte_aa)
MatrizConfusion_aa <- table(true = data_a$CLASS, predicted = corte_aa)
MatrizConfusion_aa
aa = 0
for (i in 1:ncol(MatrizConfusion_aa)){
  aa = aa + MatrizConfusion_aa[i,i]
}
aciertos_aa <- (aa / nrow(data_a))*100
aciertos_aa
###################################################


########### Mejor modelo #####################


#############################################

######a_big.csv
data_a_big = read.csv('a_big.csv', header = F);
colnames(data_a_big) <- c("X","Y","CLASS");
data_a_big$CLASS <- data_a_big$CLASS + 1;
plot(data_a_big$X, data_a_big$Y);
data_a_big <- data_a_big[order(data_a_big$CLASS), ]

k <- kmeans(data_a_big[,1:2], 3)
plot(data_a_big$X, data_a_big$Y, col = k$cluster)
points(k$centers[, c("X","Y")], col = 1:3, pch = 19, cex = 2)

MatrixConfusionk_a_big <- table(data_a_big$CLASS,k$cluster)
MatrixConfusionk_a_big


################ moon.csv ########################
data_moon = read.csv('moon.csv', header = F)
colnames(data_moon) <- c("X","Y","CLASS");
plot(data_moon$X, data_moon$Y);
data_moon$CLASS <- data_moon$CLASS + 1;
##############################################

############ k medias moon.csv #########################
km_moon <- kmeans(data_moon[,1:2], 2)

plot(data_moon$X, data_moon$Y, col = km_moon$cluster)
points(km_moon$centers[, c("X","Y")], col = 1:2, pch = 19, cex = 2)
MatrizConfusionK_moon <- table(true = data_moon$CLASS, predicted = km_moon$cluster)
MatrizConfusionK_moon
km = 0;
for (i in 1:ncol(MatrizConfusionK_moon)){
  km = km + MatrizConfusionK_moon[i,i]
}
aciertos_km <- (km / nrow(data_moon))*100
aciertos_km
####################################################


############ Cluster jerarquicos (moon.csv) ###########
data_moon1 = data_moon
data_moon1$CLASS <- NULL
data_moon1= as.matrix(data_moon1)
distancia = dist(data_moon1)
####################################################


############single method (moon.csv) ##################
cluster_ms <- hclust(distancia, method="single")
corte_ms <- cutree(cluster_ms, k=2)
plot(x = data_moon[,1], y <- data_moon[,2],col = corte_ms)
MatrizConfusion_ms <- table(true = data_moon$CLASS, predicted = corte_ms)
MatrizConfusion_ms
ms = 0
for (i in 1:ncol(MatrizConfusion_ms)){
  ms = ms + MatrizConfusion_ms[i,i]
}
aciertos_ms <- (ms / nrow(data_moon))*100
aciertos_ms
######################################################


############complete method (moon.csv) ##################
cluster_mc <- hclust(distancia, method="complete")
corte_mc <- cutree(cluster_mc, k=2)
plot(x = data_moon[,1], y <- data_moon[,2],col = corte_mc)
MatrizConfusion_mc <- table(true = data_moon$CLASS, predicted = corte_mc)
MatrizConfusion_mc
mc = 0
for (i in 1:ncol(MatrizConfusion_mc)){
  mc = mc + MatrizConfusion_mc[i,i]
}
aciertos_mc <- (mc / nrow(data_moon))*100
aciertos_mc
####################################################

############ average method (a.csv) ##################
cluster_ma <- hclust(distancia, method="average")
corte_ma <- cutree(cluster_ma, k=2)
plot(x = data_moon[,1], y <- data_moon[,2],col = corte_ma)
MatrizConfusion_ma <- table(true = data_moon$CLASS, predicted = corte_ma)
MatrizConfusion_ma
ma = 0
for (i in 1:ncol(MatrizConfusion_ma)){
  ma = ma + MatrizConfusion_ma[i,i]
}
aciertos_ma <- (ma / nrow(data_moon))*100
aciertos_ma
###################################################



######good_luck.csv#################################
data_good_luck = read.csv('good_luck.csv', header = F)
data_good_luck$V11 <- data_good_luck$V11 + 1
##################################################


########## k means good_luck.csv #################

km_gl <- kmeans(data_good_luck[,1:10], 2)
plot(data_good_luck[,1:10], col = km_gl$cluster)

MatrizConfusionK_gl <- table(true = data_good_luck$V11, predicted = km_gl$cluster)
MatrizConfusionK_gl
kgl = 0;
for (i in 1:ncol(MatrizConfusionK_moon)){
  kgl = kgl + MatrizConfusionK_moon[i,i]
}
aciertos_kgl <- (kgl / nrow(data_moon))*100
aciertos_kgl
##################################################



############ Cluster jerarquicos (good_luck.csv) ###########
data_good_luck1 = data_good_luck
data_good_luck1$CLASS <- NULL
data_good_luck1= as.matrix(data_good_luck1)
distancia = dist(data_good_luck1)
####################################################


############single method (good_luck.csv) ##################
cluster_gls <- hclust(distancia, method="single")
corte_gls <- cutree(cluster_gls, k=2)
plot(x = data_good_luck[,1], y <- data_good_luck[,2],col = corte_gls)
MatrizConfusion_gls <- table(true = data_good_luck$V11, predicted = corte_gls)
MatrizConfusion_gls
gls = 0
for (i in 1:ncol(MatrizConfusion_gls)){
  gls = gls + MatrizConfusion_gls[i,i]
}
aciertos_gls <- (gls / nrow(data_moon))*100
aciertos_gls
######################################################

############complete method (good_luck.csv) ##################
cluster_glc <- hclust(distancia, method="complete")
corte_glc <- cutree(cluster_glc, k=2)
plot(x = data_good_luck[,1], y <- data_good_luck[,2],col = corte_glc)
MatrizConfusion_glc <- table(true = data_good_luck$V11, predicted = corte_glc)
MatrizConfusion_glc
glc = 0
for (i in 1:ncol(MatrizConfusion_glc)){
  glc = glc + MatrizConfusion_glc[i,i]
}
aciertos_glc <- (glc / nrow(data_moon))*100
aciertos_glc
####################################################

############ average method (good_luck.csv) ##################
cluster_gla <- hclust(distancia, method="average")
corte_gla <- cutree(cluster_gla, k=2)
plot(x = data_good_luck[,1], y <- data_good_luck[,2],col = corte_gla)
MatrizConfusion_gla <- table(true = data_good_luck$V11, predicted = corte_gla)
MatrizConfusion_gla
gla = 0
for (i in 1:ncol(MatrizConfusion_gla)){
  gla = gla + MatrizConfusion_gla[i,i]
}
aciertos_gla <- (gla / nrow(data_good_luck))*100
aciertos_gla
###################################################











##h.csv
data_h = read.csv('moon.csv', header = F)
colnames(data_h) <- c("X","Y","CLASS");
data_h$CLASS <- data_h$CLASS + 1;
plot(data_h$X, data_h$Y);
data_h <- data_h[order(data_h$CLASS), ]

k <- kmeans(data_h[,1:2], 2)
plot(data_h$X, data_h$Y, col = k$cluster)
points(k$centers[, c("X","Y")], col = 1:3, pch = 19, cex = 2)

MatrixConfusionk_h <- table(data_h$CLASS,k$cluster)
MatrixConfusionk_h

##help.csv
data_help = read.csv('help.csv', header = F)

##s.csv
data_s = read.csv('s.csv', header = F)

