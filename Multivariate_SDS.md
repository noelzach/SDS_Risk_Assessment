### Import the data

``` r
vir <- read.csv("2015_master.csv")
str(vir)
```

    ## 'data.frame':    96 obs. of  56 variables:
    ##  $ x               : num  0.381 0.381 0.381 0.381 0.381 0.381 0.381 0.381 0.381 0.381 ...
    ##  $ y               : num  1.52 4.57 7.62 10.67 13.72 ...
    ##  $ Pass            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Range           : int  1 1 2 2 3 3 4 4 5 5 ...
    ##  $ Sample          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Section         : Factor w/ 4 levels "NE","NW","SE",..: 4 4 4 4 4 4 4 4 2 2 ...
    ##  $ V3.DW           : num  0.63 0.75 0.6 0.77 0.79 0.63 0.93 0.91 0.69 0.55 ...
    ##  $ R5.DW           : num  4.41 4.76 8.07 6.48 3.78 5.68 5.45 3.06 5.74 4.97 ...
    ##  $ V3.foliar       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ V3.root         : num  15 5 26.7 11.7 5 ...
    ##  $ R5.foliar       : num  1.33 0.33 0 0 0.33 0 0.5 2.33 0 0 ...
    ##  $ R5.root         : num  3.33 10 0 0 16.67 ...
    ##  $ R4.DI           : int  0 0 0 0 0 0 0 20 5 0 ...
    ##  $ R4.DS           : num  0 0 0 0 0 0 0 1 1 0 ...
    ##  $ R4.DX           : num  0 0 0 0 0 0 0 2.22 0.56 0 ...
    ##  $ R5.DI           : int  5 1 0 0 0 0 5 75 5 0 ...
    ##  $ R5.DS           : num  1.5 1 0 0 0 0 1.5 3 1 0 ...
    ##  $ R5.DX           : num  0.83 0.11 0 0 0 0 0.83 25 0.56 0 ...
    ##  $ R6.DI           : int  50 40 20 15 30 10 25 70 10 30 ...
    ##  $ R6.DS           : num  3 2.5 1.5 1.5 2 3 1.5 4.5 2 1.5 ...
    ##  $ R6.DX           : num  16.67 11.11 3.33 2.5 6.67 ...
    ##  $ PreSCN.cysts    : int  14 21 12 12 13 5 18 42 27 13 ...
    ##  $ PreSCN.eggs     : int  1140 1600 860 860 820 320 1280 3200 2080 880 ...
    ##  $ PreSCN.juvs     : int  200 240 60 100 100 20 240 480 400 140 ...
    ##  $ PreSCN.eggsjuvs : int  1340 1840 920 960 920 340 1520 3680 2480 1020 ...
    ##  $ PreSCN.total    : int  1354 1861 932 972 933 345 1538 3722 2507 1033 ...
    ##  $ Pre.spiral      : int  4 0 8 8 0 24 16 24 24 0 ...
    ##  $ Pre.lesion      : int  0 0 0 20 8 0 0 16 8 24 ...
    ##  $ Pre.dagger      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PostSCN.cysts   : int  15 29 13 13 7 8 33 22 7 51 ...
    ##  $ PostSCN.eggs    : int  3000 1880 1200 1120 560 840 1240 1680 680 2000 ...
    ##  $ PostSCN.juvs    : int  640 440 200 320 100 80 200 280 80 480 ...
    ##  $ PostSCN.eggsjuvs: int  3640 2320 1400 1440 660 920 1440 1960 760 2480 ...
    ##  $ PostSCN.total   : int  3655 2349 1413 1453 667 928 1473 1982 767 2531 ...
    ##  $ Post.spiral     : int  8 16 4 16 8 12 4 20 32 52 ...
    ##  $ Post.lesion     : int  0 0 0 0 0 0 0 0 0 4 ...
    ##  $ Pre.DNA         : num  65.4 64.1 69.2 79.5 55.5 ...
    ##  $ Pre.qPCR        : num  463 536 618 941 590 ...
    ##  $ Pre.ratio       : num  0.00354 0.00418 0.00446 0.00592 0.00531 ...
    ##  $ V3.qPCR         : num  7.58 6.42 8.21 10 10.8 ...
    ##  $ R5.qPCR         : num  6.81 7.35 6.76 7.35 6.7 ...
    ##  $ Post.qPCR       : num  563 520 424 297 230 ...
    ##  $ V3.Phi2         : num  0 0.00247 0.0111 0.01352 0.01357 ...
    ##  $ V3.PhiNPQ       : num  0 0.0456 0.0226 -0.0216 0.0123 ...
    ##  $ V3.PhiNO        : num  0 -0.04873 -0.03349 0.00681 -0.02685 ...
    ##  $ V3.SPAD         : num  0 1.18 1.08 3.64 3.32 4.18 1.08 1.84 2.42 5.94 ...
    ##  $ V3.NPQt         : num  0 0.33 0.203 -0.118 0.14 ...
    ##  $ V3.LEF          : num  0 1.4 7.39 7.96 7.43 ...
    ##  $ V3.qL           : num  0 0.03411 0.02564 0.00979 0.0338 ...
    ##  $ R1.Phi2         : num  0 0.0195 0.0178 0.021 0.0406 ...
    ##  $ R1.PhiNPQ       : num  0 0.00412 -0.0347 -0.02573 -0.06934 ...
    ##  $ R1.PhiNO        : num  0 -0.02561 0.01385 0.00303 0.02437 ...
    ##  $ R1.SPAD         : num  0 2.283 2.233 0.517 1.717 ...
    ##  $ R1.NPQt         : num  0 0.183 -0.245 -0.147 -0.401 ...
    ##  $ R1.LEF          : num  0 -1.5976 2.416 -0.0152 4.9205 ...
    ##  $ R1.qL           : num  0 0.08411 0.00821 0.03931 0.041 ...

``` r
sample <- vir$Sample
direction <- vir$Section

var <- vir[,-c(1:5)]
nrow(var)
```

    ## [1] 96

Principle Coordinates analysis
------------------------------

``` r
library(ape)
library(ecodist)
pre.plantvar <- grep("Pre", names(var))
pre.plant <- var[,pre.plantvar]
pre.plant.pca <- pre.plant[,-c(1,2,4,5,9,11)]
#pre.plant <- cbind(pre.plant, pam.clust.R5$V5)
n = nrow(pre.plant.pca)
p = ncol(pre.plant.pca)
rank.pre = min(n,p)

delta = function(D) {
  DD=as.matrix(D)
  n=nrow(DD)
  d=matrix(0,n,n)
  A=-0.5*(DD^2)
  Arm = rowMeans(A)
  Acm = colMeans(A)
  Agm = mean(A)
  
  for (i in 1:n) {
    for (j in 1:n) {
      d[i,j] = A[i,j] - Arm[i] - Acm[j] + Agm
    }}
  return(d)
}

library(vegan)
pre.bray <- vegdist(pre.plant.pca, method = "canberra")
e.bray <- eigen(delta(pre.bray))

#Now calculate the eigenvectors of the delta matrix
pre.cmd <- cmdscale(pre.bray, k = rank.pre, eig = TRUE)
prop.pre <- as.matrix((pre.cmd$eig/sum(pre.cmd$eig))*100)
prop.pre[1:rank.pre]
```

    ## [1] 31.303915 23.974388 17.950494 14.102658  8.766719

``` r
barplot(pre.cmd$eig[1:rank.pre], main="",
   xlab="Dimensions", ylab = "Eigenvalues")
lines(pre.cmd$values)
abline(h = mean(pre.cmd$eig[1:rank.pre]), col = "red", lty = 2)
```

![](Multivariate_SDS_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
pre.pco <- as.data.frame(pre.cmd$points)
pre.pco$Section <- vir$Section
pre.pco$R5.DX <- vir$R5.DX

library(ggplot2)
plot3 <- ggplot(pre.pco, aes(x = V1, y = V2, color = R5.DX)) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  xlab("Axis1 (71%)") +
  ylab("Axis2 (16%)") + 
  theme_bw() 
  #theme(axis.text.x = element_text(size = 10, face = "bold"),
   #     axis.text.y = element_text(size = 10, face = "bold"),
    #    axis.title.x = element_text(size = 20, face = "bold"),
     #   axis.title.y = element_text(size = 20, face = "bold"),
      #  legend.title = element_text(size  = 0, face = "bold"),
       # legend.text = element_text(size  = 20, face = "bold.italic"))
plot.1 <- as.data.frame(cbind(pre.plant.pca$PreSCN.juvs, pre.pco$V1))
plot1 <- ggplot(plot.1, aes(x = V1, y = V2, color = var$R5.DX )) + 
  geom_point(size = 3) + 
  stat_smooth(color = "black", se = FALSE) + 
  scale_colour_gradient(name = "Disease\nIndex R5", low="forestgreen", high = "red") + 
  xlab("Pre soil SCN juviniles") +
  ylab("PcoA1 (71%)") +
  theme_bw()
plot.2 <- as.data.frame(cbind(pre.plant.pca$Pre.qPCR, pre.pco$V1, pre.pco$V2))
plot2 <- ggplot(plot.2, aes(x = V1, y = V2, color = var$R5.DX )) + 
  geom_point(size = 3) + 
  scale_colour_gradient(name = "Disease\nIndex R5", low="forestgreen", high = "red") + 
  stat_smooth(color = "black", se = FALSE) + 
  xlab("Pre soil qPCR") +
  ylab("PcoA1 (71%)") +
  theme_bw()

library(gridExtra)
grid.arrange(plot3, arrangeGrob(plot2, plot1, ncol = 2))
```

![](Multivariate_SDS_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
pre.qpcr1 <- cor(pre.plant.pca$Pre.qPCR, pre.pco$V1)
pre.juv1 <- cor(pre.plant.pca$PreSCN.juvs, pre.pco$V1)
pre.spiral1 <- cor(pre.plant.pca$Pre.spiral, pre.pco$V1)
pre.lesion1 <-  cor(pre.plant.pca$Pre.lesion, pre.pco$V1)
pre.qpcr2 <- cor(pre.plant.pca$Pre.qPCR, pre.pco$V2)
pre.juv2 <- cor(pre.plant.pca$PreSCN.juvs, pre.pco$V2)
pre.spiral2 <- cor(pre.plant.pca$Pre.spiral, pre.pco$V2)
pre.lesion2 <-  cor(pre.plant.pca$Pre.lesion, pre.pco$V2)
pre.qpcr3 <- cor(pre.plant.pca$Pre.qPCR, pre.pco$V3)
pre.juv3 <- cor(pre.plant.pca$PreSCN.juvs, pre.pco$V3)
pre.spiral3 <- cor(pre.plant.pca$Pre.spiral, pre.pco$V3)
pre.lesion3 <-  cor(pre.plant.pca$Pre.lesion, pre.pco$V3)

data.frame1 <- data.frame(c(pre.qpcr1, pre.juv1, pre.spiral1, pre.lesion1))
data.frame2 <- data.frame(c(pre.qpcr2, pre.juv2, pre.spiral2, pre.lesion3))
data.frame3 <- data.frame(c(pre.qpcr3, pre.juv3, pre.spiral3, pre.lesion3))

corelations <- cbind.data.frame(data.frame1, data.frame2, data.frame3)
colnames(corelations) <- c("Dim1", "Dim2", "Dim3")
rownames(corelations) <- c("qPCR", "SCN", "Spiral", "Lesion")
library(knitr)
kable(corelations)
```

|        |       Dim1|        Dim2|        Dim3|
|--------|----------:|-----------:|-----------:|
| qPCR   |  0.3150360|  -0.5641062|   0.0174347|
| SCN    |  0.4583827|  -0.5701695|  -0.0241942|
| Spiral |  0.5236592|  -0.2134894|   0.0355331|
| Lesion |  0.4498116|  -0.1075225|  -0.1075225|

### Canonical correlation analysis

``` r
options(scipen = 999) # stops anything from being in scientific notation

#Canonical Correlation Analysis (CCA)
pre.plantvar <- grep("Pre", names(var))
pre.plant <- var[,pre.plantvar]

R5.plantvar <- grep("R5", names(var))
R5.plant <- var[,R5.plantvar]
R5.plant <- R5.plant[,-c(4,5,6,7)]

disease <- data.frame(R5.plant)
pre <- pre.plant[,-c(1,2,4,5,9,11)]

#install.packages("yacca")
library(yacca)
cca <- cca(pre, disease)
cca$chisq
```

    ##      CV 1      CV 2      CV 3 
    ## 54.130946 18.166115  7.344583

``` r
cca$df
```

    ## CV 1 CV 2 CV 3 
    ##   15    8    3

``` r
pchisq(cca$chisq[1],cca$df[1],lower.tail = F)
```

    ##           CV 1 
    ## 0.000002497663

``` r
pchisq(cca$chisq[2],cca$df[2],lower.tail = F)
```

    ##     CV 2 
    ## 0.020015

``` r
pchisq(cca$chisq[3],cca$df[3],lower.tail = F)
```

    ##       CV 3 
    ## 0.06168917

``` r
pchisq(cca$chisq[4],cca$df[4],lower.tail = F)
```

    ## <NA> 
    ##   NA

``` r
-cca$xstructcorr[,1]
```

    ## PreSCN.juvs  Pre.spiral  Pre.lesion  Pre.dagger    Pre.qPCR 
    ##  0.94141253 -0.02633386 -0.09556726  0.15302870  0.51178917

``` r
-cca$xstructcorr[,2]
```

    ## PreSCN.juvs  Pre.spiral  Pre.lesion  Pre.dagger    Pre.qPCR 
    ##  -0.1771668   0.3772380  -0.2394357   0.6614294   0.3998948

``` r
-cca$ystructcorr[,1]
```

    ##      R5.DW  R5.foliar    R5.root 
    ## -0.5430657  0.8049481  0.8811459

``` r
-cca$ystructcorr[,2]
```

    ##      R5.DW  R5.foliar    R5.root 
    ##  0.5738460  0.4713794 -0.4717497

``` r
F.test.cca(cca)
```

    ## 
    ##  F Test for Canonical Correlations (Rao's F Approximation)
    ## 
    ##          Corr        F   Num df Den df     Pr(>F)    
    ## CV 1  0.57266  3.92468 15.00000 243.33 0.00000253 ***
    ## CV 2  0.33571  2.34904  8.00000 178.00    0.02004 *  
    ## CV 3  0.27919  2.53619  3.00000  90.00    0.06170 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Plotting the CCA analysis

``` r
cca.bar.X1 <- data.frame(as.vector(cca$xstructcorr[,1]), "X", "Dim1", names(cca$xstructcorr[,1]))
colnames(cca.bar.X1) <- c("CV", "Axis", "Dim", "Var")
cca.bar.X2 <- data.frame(as.vector(cca$xstructcorr[,2]), "X", "Dim2", names(cca$xstructcorr[,2]))
colnames(cca.bar.X2) <- c("CV", "Axis", "Dim", "Var")
cca.bar.Y1 <- data.frame(as.vector(cca$ystructcorr[,1]), "Y", "Dim1", names(cca$ystructcorr[,1]))
colnames(cca.bar.Y1) <- c("CV", "Axis", "Dim", "Var")
cca.bar.Y2 <- data.frame(as.vector(cca$ystructcorr[,2]), "Y", "Dim2", names(cca$ystructcorr[,2]))
colnames(cca.bar.Y2) <- c("CV", "Axis", "Dim", "Var")

cca.bar <- rbind.data.frame(cca.bar.X1, cca.bar.X2, cca.bar.Y1, cca.bar.Y2)

#Is there increasing nematode diversity and virguliforme numbers with incrasing disease??
points1 <- data.frame(cca$canvary[,1], cca$canvarx[,1], vir$R5.DX, vir$Section)
colnames(points1) <- c("y", "x", "R5.DX", "Section")
points2 <- data.frame(cca$canvary[,2], cca$canvarx[,2], vir$R5.DX, vir$Section)
colnames(points2) <- c("y", "x", "R5.DX", "Section")
library(ggplot2)
p3 <- ggplot(points1, aes(y = -y, x = -x, color = Section)) + 
  geom_point(size = 4) + 
  xlab(expression("CCA X"[1])) + 
  ylab(expression("CCA Y"[1])) +
  theme_bw() +
  ggtitle("Dimension 1")
p4 <- ggplot(points2, aes(y = -y, x = -x, color = Section)) + 
  geom_point(size = 4) + 
  xlab(expression("CCA X"[2])) + 
  ylab(expression("CCA Y"[2])) +
  theme_bw() + 
  ggtitle("Dimension 2")
p1 <- ggplot(cca.bar[cca.bar$Dim == "Dim1",], aes(x = Var, y = -CV)) + 
  geom_bar(aes(fill = Axis), stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle("Dimension 1") + 
  scale_x_discrete(labels = c("R5.root" = "Root Rot","R5.foliar" = "Foliar", "R5.DX" = "Disease Index", 
                              "R5.DW" = "Root Dry Weight", "PreSCN.juvs" = "Soybean Cyst Nematode", "Pre.spiral" = "Spiral Nematodes", "Pre.qPCR" = "F. virguliforme", "Pre.lesion" = "Lesion Nematode", "Pre.dagger" = "Dagger Nematodes")) +
  xlab("Variables") + 
  ylab("Loadings") + 
  theme_bw()
p2 <- ggplot(cca.bar[cca.bar$Dim == "Dim2",], aes(x = Var, y = -CV)) + 
  geom_bar(aes(fill = Axis), stat = "identity", position = "dodge") +
  ggtitle("Dimension 2") + 
  scale_x_discrete(labels = c("R5.root" = "Root Rot","R5.foliar" = "Foliar", "R5.DX" = "Disease Index", 
                              "R5.DW" = "Root Dry Weight", "PreSCN.juvs" = "Soybean Cyst Nematode", "Pre.spiral" = "Spiral Nematodes", "Pre.qPCR" = "F. virguliforme", "Pre.lesion" = "Lesion Nematode", "Pre.dagger" = "Dagger Nematodes")) +
  xlab("Variables") + 
  ylab("Loadings") + 
  coord_flip() +
  theme_bw()
library(Rmisc)
multiplot(p1, p3, p2, p4, cols = 2)
```

![](Multivariate_SDS_files/figure-markdown_github/unnamed-chunk-4-1.png)
