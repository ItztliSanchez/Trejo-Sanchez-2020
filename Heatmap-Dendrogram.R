####CODE TO REPLY HEAT MAP AND DENDROGRAM COMBINATION#####
#PACKAGE LOAD
library("ggbiplot")
library("dplyr")
install.packages("gplots")
library("gplots")

#DATA LOAD
V1 <- read.table(file="MultipleRegression2.csv", header = TRUE, sep=",") %>% na.omit(AMG)
str(V1)
v1.Id <- V1[, 1]
print(v1.Id)
v1.Class2 <- V1[, 4]
print(v1.Class2)
V1.ClassGGO <- V1[, 2]
print(V1.ClassGGO)

#CHOSE COLOUR PALETTE
V1.sc = as.matrix(scale(V1[12:35]))

my_palette <- colorRampPalette(c("red", "black", "blue"))(n = 1000)

color.Id1  <- function(v1.Class2){if(v1.Class2=="Normal") "magenta" 
  else if(v1.Class2=="Rapid") "blue" 
  else if(v1.Class2=="Slow") "limegreen"
  else if(v1.Class2=="Sluggish") "red"}
str(color.Id1)

sidebarcolors <- unlist(lapply(V1$Categories, color.Id1))
str(sidebarcolors)
print(sidebarcolors)

color.Id2  <- function(V1.ClassGGO){if(V1.ClassGGO=="Average") "magenta" 
  else if(V1.ClassGGO=="Very High") "blue" 
  else if(V1.ClassGGO=="High") "limegreen"
  else if(V1.ClassGGO=="Very Low") "red"
  else if(V1.ClassGGO=="Low") "cyan3"}
str(color.Id2)

sidebarcolors2 <- unlist(lapply(V1$ClassGGO, color.Id2))
str(sidebarcolors2)
print(sidebarcolors2)

MyLabCol <- c("AR", "ESR1", "CYP19", "PR", "DNMT1", "DNMT3a", 
              "AR", "ESR1", "CYP19", "PR", "DNMT1", "DNMT3a",
              "AR", "ESR1", "CYP19", "PR", "DNMT1", "DNMT3a",
              "AR", "ESR1", "CYP19", "PR", "DNMT1", "DNMT3a")
str(MyLabCol)

col.color.map <- c(rep("yellow", 6),rep("coral1", 6 ), rep("tan3", 6), rep("gray68", 6))
str(col.color.map)


par(xpd=T, mar=par()$mar+c(0,0,0,16))

par(oma=c(0, 0, 0, 0.5), xpd=TRUE)
heatmap.2(V1.sc, scale='none', labRow = v1.Id, trace = 'none', col= my_palette, cexRow = 1.5,
          cexCol=1.5, RowSideColors=sidebarcolors2, labCol=MyLabCol, ColSideColors = col.color.map, 
          keysize=1, key.title= "Color Key", margins = c(8, 8))

legend(locator(1), c("Normal", "Rapid", "Slow", "Sluggish"), cex = 0.8,
       col = c("magenta", "blue", "limegreen", "red"), title="Categories", 
       text.font=0.1, pch = 19, pt.cex = 2, box.lty = 0)

legend(locator(1), c("Average", "High", "Low", "Very High", "Very Low"), cex = 0.7,
       col = c("magenta", "limegreen", "cyan3", "blue", "red"), title="Categories", 
       text.font=0.1, pch = 19, pt.cex = 2, box.lty = 0)

legend(locator(1), legend = c("AMG", "OB", "MPOA", "VMH"), cex = 0.7,
       col = c("yellow", "coral1", "tan3", "gray68"), title = "Brain Regions", 
       text.font = 0.1, lty=1, lwd=8, box.lty = 0)

legend(locator(1), legend = c("AMG", "OB", "MPOA", "VMH"), cex = 0.9,
       col = c("yellow", "coral1", "tan3", "gray68"), title = "Brain Regions", 
       text.font = 0.1, pch = 19, pt.cex = 2, box.lty = 0)
