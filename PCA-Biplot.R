######SCRIPT TO BUILD UP A PCA BIPLOT FROM DATASET OF RAT MALE COPULATORY BEHAIOR######

######DOWNLOAD PACKAGES######
install.packages("dplyr")
install_github("vqv/ggbiplot")

library("dplyr")
library("ggbiplot")

######DATA LOADING######
Sex.Beh <- read.table(file="SexBeh.csv", header = TRUE, sep=",")
str(Sex.Beh)

Dataset available from  https://doi.org/10.6084/m9.figshare.12108984.v1

######PCA of Copulatory Behavior######

######DEFINE VARIABLES######
Sex.Beh.Var <-Sex.Beh[2:7]
SB.LEClass <- Sex.Beh[, 11]
SB.Class2 <- Sex.Beh[, 10]
SB.Id <- c(Sex.Beh[, 1])

######BUID UP THE PCA BIPLOT######
SB1 <- prcomp(Sex.Beh[2:7], center=TRUE, scale=TRUE)

SBplot2 <- ggbiplot(SB1, obs.scale = 1, var.scale = 1, groups = SB.LEClass, ellipse = TRUE, 
                    labels = SB.Id, label.size = 5, circle = TRUE, circle.prob = 0.9,
                    varname.size = 3.5, varname.adjust = 4, varname.abbrev = TRUE)+
  scale_color_manual(name="Categories", values=c("magenta", "blue", "limegreen", "red"))+
  theme_minimal()+ theme(legend.direction ='horizontal', legend.position = 'top')

print(SBplot2)


SBplot3 <- ggbiplot(SB1, obs.scale = 1, var.scale = 1, groups = SB.Class2, ellipse = TRUE, 
                    labels = SB.Id, label.size = 5, circle = TRUE, circle.prob = 0.9,
                    varname.size =3.5, varname.adjust = 4, varname.abbrev = TRUE) +
  scale_color_manual(name="Categories", values=c("magenta","limegreen","cyan3","blue","red"))+
  theme_minimal()

print(SBplot3)


######PCA Complements#####

library("FactoMineR")
library("factoextra")

SB2 <- PCA(Sex.Beh[2:7])
print(SB2)

fviz_cos2(SB2, choice = "var", axes = 1:2)+
  ggtheme=theme(axis.text=element_text(size=10),
              axis.title=element_text(size=16, face="bold"))

fviz_contrib(SB2, choice = "var", axes = 1)
fviz_contrib(SB2, choice = "var", axes = 2)
fviz_contrib(SB2, choice = "ind", axes = 1:2, labels = Sex.Beh$ID)
fviz_eig(SB1, addlabels = TRUE, ylim = c(0, 50))

SB2$var$contrib
SB2$ind
SB2$call


#######CorrPlot of Copulatory Variables#######

library("corrplot")
SexBeh.cor.mat <- round(cor(Sex.Beh[, 2:7]), 2)

layout(matrix(1, nrow = 1))

col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
col2 <- col3 <- colorRampPalette(c("red", "white", "blue"))

layout(matrix(1), widths = lcm(15), heights = lcm(15))

corrplot.mixed(SexBeh.cor.mat, order="FPC", lower="number", upper="circle", 
               tl.pos = "lt", number.cex = 0.8, cl.cex=0.8, 
               tl.col="black", tl.cex=0.8, tl.srt=45, diag = "u", col=col1(50))


#######Dendrogram of Copulatory Behavior#######

install.packages('dendextend')

library("dendextend")
library("cluster")

install.packages("rafalib")
library("rafalib")
library("digest")

Sex.Beh <- read.table(file="SexBeh.csv", header = TRUE, sep=",")
str(Sex.Beh)

Sex.Beh.Var <-Sex.Beh[2:7]
SB.LEClass <- Sex.Beh[, 11]
print(SB.LEClass)
SB.Class2 <- Sex.Beh[, 10]
print(SB.Class2)
SB.Id <- c(Sex.Beh[, 1])
print(SB.Id)

Sex.Beh.sc <- scale(Sex.Beh.Var)
Sex.Beh.Clustering <- dist(Sex.Beh.sc, method = 'euclidean')
Sex.Beh.complete <- hclust(Sex.Beh.Clustering, method = 'complete')
SB.Dendrogram <- as.dendrogram(Sex.Beh.complete)

ColorCode1 <- c("Normal"="magenta","Rapid"="blue", "Slow"="limegreen", "Sluggish"="red")

LabCol1 <- labels_colors(SB.Dendrogram) <- ColorCode1[Sex.Beh$Categories]
print(LabCol1)
myplclust(Sex.Beh.complete, labels=Sex.Beh$ID, lab.col=LabCol1, cex=1.2)

SB.4g <- rect.hclust(Sex.Beh.complete, h=4, border="red")
print(SB.4g)

SB.groups <- cutree(SB.Dendrogram, k = 4)
table(SB.groups)

SB.tree<-agnes(Sex.Beh.sc, method = 'complete')
SB.tree$ac

legend(locator(1), c("Normal", "Rapid", "Slow", "Sluggish"), cex = 1,
       col = c("magenta", "blue", "limegreen", "red"), title="Categories", 
       text.font=0.1, pch = 19, pt.cex = 2, box.lty = 0, horiz = FALSE)



ColorCode2 <- c("Average"="magenta", "High"="limegreen", "Low" = "cyan3", 
                "Very High"="blue", "Very Low"="red")

LabCol2 <- labels_colors(SB.Dendrogram) <- ColorCode2[Sex.Beh$Class2]
print(LabCol2)
myplclust(Sex.Beh.complete, labels=Sex.Beh$ID, lab.col=LabCol2, cex=1.2)

legend(locator(1), c("Average", "High", "Low", "Very High", "Very Low"), cex = 0.9,
       col = c("magenta", "limegreen", "cyan3", "blue", "red"), title="Categories", 
       text.font=0.1, pch = 19, pt.cex = 2, box.lty = 0, horiz = FALSE)



#####KUSRKAL-WALLIS TEST######

install.packages("dplyr")
library("dplyr")

install.packages("ggpubr")
library("ggpubr")

Sex.Beh <- read.table(file="SexBeh.csv", header = TRUE, sep=",")
str(Sex.Beh)

Sex.Beh$Categories <- ordered(Sex.Beh$Categories,
                         levels = c("Rapid", "Normal", "Slow", "Sluggish"))

Sex.Beh$Calass2 <- ordered(Sex.Beh$Class2,
                              levels = c("Very High", "High", "Average", 
                                         "Low", "Very Low"))

#####Ejaculation Latency######

ggboxplot(Sex.Beh, x = "Categories", y = "EL", 
          color = "Categories", palette = c("blue", "magenta", "limegreen", "red"),
          order = c("Rapid", "Normal", "Slow", "Sluggish"),
          ylab = "Ejaculation latency (sec)", xlab = "Categories")

kruskal.test(EL ~ Categories, data = Sex.Beh)

pairwise.wilcox.test(Sex.Beh$EL, Sex.Beh$Categories, p.adjust.method="none")



ggboxplot(Sex.Beh, x = "Class2", y = "EL", 
          color = "Class2", palette = c("blue", "limegreen", "magenta", "cyan",
                                       "red"),
          order = c("Very High", "High", "Average", "Low", "Very Low"),
          ylab = "Ejaculation latency (sec)", xlab = "Categories EN")

kruskal.test(EL ~ Class2, data = Sex.Beh)

pairwise.wilcox.test(Sex.Beh$EN, Sex.Beh$Class2, p.adjust.method="none")

