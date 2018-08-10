library(rvest)
library(tidyverse)
library(ggplot2)

test <- read_html(sprintf("https://www.pro-football-reference.com/years/%s/opp.htm", 2017)) %>% 
  html_nodes(css = '#passing') %>% 
  html_table(header = TRUE)
test[[1]]

getDefenseTable <- function(Year, nTeams=5) {
  ## Pull Data
  defense <- read_html(sprintf("https://www.pro-football-reference.com/years/%s/opp.htm", Year)) %>% 
    html_nodes(css = '#team_stats') %>% 
    html_table(header = TRUE)
  ## Get Defense from List
  defense <- defense[[1]]
  
  ## Fix Var Names
  pre <- gsub(" ", "", colnames(defense))
  pre <- gsub("TotYds&TO", "Tot", pre)
  colnames(defense) <- paste(pre, defense[1,], sep = ".")
  colnames(defense) <- gsub("^\\.", "", colnames(defense))
  ## Get rid of non matching Var
  expIndex <- grep("EXP|\\%",colnames(defense))
  if (any(expIndex)) {
    defense <- defense[,-1*expIndex]
  }
  
  ## Pull out columns we don't want
  defense <- defense[-1*c(1,34:36),]
  rownames(defense) <- NULL
  ## FIx Defenseive Year
  defense$Year <- rep(Year, nrow(defense))
  defense <- defense[1:nTeams,]
  rownames(defense) <- paste(defense$Tm, defense$Year, sep = "_")
  defense <- defense[,-1*grep("Penalties|^G$|FL|Year|Tm|Rk",colnames(defense))]
  defense<- data.matrix(defense)
  return(defense)
}


getAllDefenses <- function(start, end, raw = F, Cores = 3) {
  ## Multiple cores for speed
  defenseList <- mclapply(end:start, FUN = getDefenseTable, mc.cores = Cores)
  defense_ALL <- do.call(rbind, defenseList)
  
  ## Return raw or scaled
  if (raw == T) {
    return(defense_ALL)
  } else {
    scaled_defense_ALL <- scale(defense_ALL, scale = T, center = T)
    return(scaled_defense_ALL)
  }
}




plotMDS <- function(matrix, distMethod = "euclidian", numClusters=6, returnClusters=T) {
  ### Get dist matrix for MDS and get MDS coord
  Dmat = dist(matrix,method="euclidean")
  mdsres = cmdscale(Dmat,k = 2)
  mdsres<- data.frame(mdsres)
  colnames(mdsres) <- c("MDS1", "MDS2")
  
  ### Cluster 
  Dmat = dist(matrix,method=distMethod)
  com.hc = hclust(Dmat,method="ward.D")
  res.com = cutree(com.hc,numClusters)
  clusters <-data.frame(res.com)
  clusters$Name <- factor(sprintf("Cluster%s", clusters$res.com))
  clusters <- clusters[,c(2), drop=FALSE]
  # Return Clusters
  if (returnClusters ==T) {
    assign("clusters", clusters, envir = .GlobalEnv)
  }
  
  ### Plot MDS with Clusters
  
  ## Get out year for title
  years <- as.numeric(gsub(".*_([0-9]+$)","\\1",rownames(matrix)))
  startYear <- min(years)
  endYear <- max(years)
  plotTitle <- sprintf("Top 5 defenses every year from %s-%s", startYear, endYear)
  return(plot(ggplot(mdsres, aes(x=MDS1, y = MDS2, label = row.names(mdsres), colour = factor(res.com))) +
    geom_text(size=2.2) + 
    theme_bw() +
    ggtitle(plotTitle) +
    labs(colour = "Clusters")))
}


scaled_defense_ALL <- getAllDefenses(start = 2017, end = 1985, raw = F, Cores = 3)
rownames(scaled_defense_ALL) <- gsub(".*[[:space:]]([0-z]+_[0-9]+$)", "\\1",rownames(scaled_defense_ALL))
plotMDS(scaled_defense_ALL, returnClusters = T)
cluster1 <- clusters[clusters[,1] == "Cluster1",drop=FALSE]
plotMDS(scaled_defense_ALL[rownames(cluster1),], numClusters = 4)


scaled_defense_ALL_70 <- getAllDefenses(start = 2017, end = 1970, raw = F, Cores = 3)
plotMDS(scaled_defense_ALL_70)
cluster1 <- clusters[clusters[,1] == 1,,drop=FALSE]
plotMDS(scaled_defense_ALL[rownames(cluster1),], numClusters = 4)






getOffensiveTable <- function(Year, nTeams=5) {
  offense <-   returns <- read_html(sprintf("https://www.pro-football-reference.com/years/%s/#all_team_stats", Year)) %>% 
    html_nodes(xpath = '//comment()') %>%    # select comments
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to single string
    read_html() %>%    # reread as HTML
    html_node('table#team_stats') %>%    # select desired node
    html_table()
  pre <- gsub(" ", "", colnames(offense))
  pre <- gsub("TotYds&TO", "Tot", pre)
  colnames(offense) <- paste(pre, offense[1,], sep = ".")
  colnames(offense) <- gsub("^\\.", "", colnames(offense))
  expIndex <- grep("EXP|\\%",colnames(offense))
  if (any(expIndex)) {
    offense <- offense[,-1*expIndex]
  }
  offense <- offense[-1*c(1,34:36),]
  rownames(offense) <- NULL
  offense$Year <- rep(Year, nrow(offense))
  offense <- offense[1:nTeams,]
  rownames(offense) <- paste(offense$Tm, offense$Year, sep = "_")
  offense <- offense[,-1*grep("Penalties|^G$|FL|Year|Tm",colnames(offense))]
  offense<- data.matrix(offense)
  return(offense)
}

getAllOffenses <- function(start, end, Teams=5, raw = F, Cores = 3) {
  ## Multiple cores for speed
  offenseList <- mclapply(end:start, FUN = getOffensiveTable, nTeams = Teams, mc.cores = Cores)
  offense_ALL <- do.call(rbind, offenseList)
  
  ## Return raw or scaled
  if (raw == T) {
    return(offense_ALL)
  } else {
    scaled_offense_ALL <- scale(offense_ALL, scale = T, center = T)
    return(scaled_offense_ALL)
  }
}


scaled_offense_ALL <- getAllOffenses(start = 2017, end = 1985, raw = F, Cores = 3)
plotMDS(scaled_offense_ALL, distMethod = "manhattan")
cluster1 <- clusters[clusters[,1] == 1,,drop=FALSE]
plotMDS(scaled_offense_ALL[rownames(cluster1),], numClusters = 4)


library("gplots")
aa = grep("grey",colors())
bb = grep("green",colors())
cc = grep("red",colors())
gcol2 = colors()[c(aa[1:2],bb[1:25],cc[1:50])]
library(gplots)

heatmap.2(scaled_offense_ALL,
          col=jet.colors,
          dendrogram = "row", 
          distfun = function(x)dist(x,method="manhattan"),
          hclustfun = function(x)hclust(x,method="ward.D"), 
          cexRow = .25,
          cexCol =0.4,trace = "none",
          margins = c(3,3.9))

plotHeatmap <- function(matrix, distMethod = "euclidean", numClusters=6) {
  ## Calulate Distance and HClust
  distance <- dist(matrix, method=distMethod)
  hclustering <-hclust(distance, method="ward.D")
  res.com = cutree(hclustering,numClusters)
  clusters <-data.frame(res.com)
  clusters$Name <- factor(sprintf("Cluster%s", clusters$res.com))
  clusters <- clusters[,c(2), drop=FALSE]
  
  matrix[matrix < -3 ] <- -3
  ## plot heatmap
  jet.colors <- colorRampPalette(c("#b2182b", "#f7f7f7", "#2166ac"))(n=111)
  pheatmap(matrix,
           color = jet.colors,
           cellwidth =7,
           cellheight = 7, 
           fontsize_row = 7,
           fontsize_col = 7,
           cluster_rows = hclustering,
           annotation_row =  clusters,) 
}
plotHeatmap(scaled_defense_ALL,
            distMethod = "euclidean", 
            numCluster = 5)
plotHeatmap(scaled_offense_ALL[clusters$Name == "Cluster1",], 
            distMethod = "manhattan", 
            numClusters = 4)


Season2017 <- getDefenseTable(2017, nTeams = 30)
scaled_2017 <- scale(Season2017, scale = T, center = T)
plotMDS(scaled_2017, distMethod = "manhattan", numClusters = 5)
plotHeatmap(scaled_2017,
            distMethod = "manhattan", 
            numClusters = 4)

Season2017 <- getOffensiveTable(2017, nTeams = 30)
scaled_2017 <- scale(Season2017, scale = T, center = T)
plotMDS(scaled_2017, distMethod = "euclidean", numClusters = 4)
plotHeatmap(scaled_2017,
            distMethod = "euclidean", 
            numClusters = 4)

scaled_10_2017 <- getAllOffenses(2007, 2017, Teams=30)
plotMDS(scaled_10_2017, distMethod = "euclidean", numClusters = 8)
plotHeatmap(scaled_10_2017,
            distMethod = "euclidean", 
            numClusters = 5)

getAllDefenses <- function(start, end, raw = F, Cores = 3) {
  ## Multiple cores for speed
  defenseList <- mclapply(end:start, FUN = getDefenseTable, mc.cores = Cores)
  defense_ALL <- do.call(rbind, defenseList)
  
  ## Return raw or scaled
  if (raw == T) {
    return(defense_ALL)
  } else {
    scaled_defense_ALL <- scale(defense_ALL, scale = T, center = T)
    return(scaled_defense_ALL)
  }
}


  QB <- read_html(sprintf("https://www.pro-football-reference.com/years/%s/passing.htm", 2017)) %>%    # reread as HTML
  html_node('#passing') %>%    # select desired node
  html_table()
QB
QB <- QB[,-1*grep("^G|Cmp|TD|Int|^Yds$|Rk|Age|Pos|GS|Lng|Tm|4QC|GWD|QBR", colnames(QB))]
QB<- QB[which(as.numeric(QB$Att) > 150),]
QB <- QB[,-1*grep("^Att", colnames(QB))]

rec <- do.call(rbind, strsplit(QB$QBrec, "-"))
QB$QBrec <- as.numeric(rec[,1])/(as.numeric(rec[,1]) +as.numeric(rec[,2]))
rownames(QB)<- QB[,1]
QB <- QB[,-1]

QB <- scale(data.matrix(QB), center = T, scale = T)
plotMDS(QB, numClusters = 6, distMethod = "manhattan")

getOffensiveTable <- function(Year, nTeams=5) {
  offense <-read_html(sprintf("https://www.pro-football-reference.com/years/%s/#all_team_stats", Year)) %>% 
    html_nodes(xpath = '//comment()') %>%    # select comments
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to single string
    read_html() %>%    # reread as HTML
    html_node('table#team_stats') %>%    # select desired node
    html_table()
  pre <- gsub(" ", "", colnames(offense))
  pre <- gsub("TotYds&TO", "Tot", pre)
  colnames(offense) <- paste(pre, offense[1,], sep = ".")
  colnames(offense) <- gsub("^\\.", "", colnames(offense))
  expIndex <- grep("EXP|\\%",colnames(offense))
  if (any(expIndex)) {
    offense <- offense[,-1*expIndex]
  }
  offense <- offense[-1*c(1,34:36),]
  rownames(offense) <- NULL
  offense$Year <- rep(Year, nrow(offense))
  offense <- offense[1:nTeams,]
  rownames(offense) <- paste(offense$Tm, offense$Year, sep = "_")
  offense <- offense[,-1*grep("Penalties|^G$|FL|Year|Tm",colnames(offense))]
  offense<- data.matrix(offense)
  return(offense)
}




























getReturnInfo <- function(Year) {
  url <- sprintf("https://www.pro-football-reference.com/years/%s/", Year)
  returns <- read_html(url) %>% 
    html_nodes(xpath = '//comment()') %>%    # select comments
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to single string
    read_html() %>%    # reread as HTML
    html_node('table#returns') %>%    # select desired node
    html_table() 
  pre <- gsub(" ", "", colnames(returns))
  colnames(returns) <- paste(pre, returns[1,], sep=".")
  colnames(returns) <- gsub("^\\.", "", colnames(returns))
  returns <- returns[-1*c(1,34:36),]
  rownames(returns) <- NULL
  returns$Year <- rep(Year, nrow(returns))
  returns <- returns[,c(2,6,11)]
  return(returns)
}
defense <- getDefenseTable("2002")
colnames(defense[,grep()])
head(defense, 1)
returns <- getReturnInfo("2002")
merge(defense, returns, by = "Tm")




getNBAInfo <- function(Year) {
  url <- sprintf("https://www.basketball-reference.com/leagues/NBA_%s.html#all_team-stats-base", Year)
  returns <- read_html(url) %>% 
    html_nodes(xpath = '//comment()') %>%    # select comments
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to single string
    read_html() %>%    # reread as HTML
    html_node(css = '#team-stats-per_poss') %>%    # select desired node
    html_table() 
  returns <- returns[,-1*grep("MP|^G$", colnames(returns))]
  returns$Team <- gsub("\\*", "", returns$Team)
  returns$Year <- rep(Year, nrow(returns))
  return(returns)
}
library(useful)
nba2018 <-getNBAInfo(2018)
corner(nba2018)
colnames(nba2018)
head(nba2018)


for (year in 1990:2018) {
  if (year  == 1990) {
    message(sprintf("getting data for %s", year))
    Offense_ALL <- getNBAInfo(as.character(year))
    Offense_ALL <- Offense_ALL[1:5,]
  } else {
    message(sprintf("getting data for %s", year))
    tmp.Offense <- getNBAInfo(as.character(year))
    tmp.Offense <- tmp.Offense[1:5,]
    Offense_ALL <- rbind(tmp.Offense, Offense_ALL)
  }
}
rownames(Offense_ALL) <- paste(Offense_ALL$Team, Offense_ALL$Year, sep = "_")
Offense_ALL
Offense_ALL <- Offense_ALL[,-1*grep("Year|Team",colnames(Offense_ALL))]

Offense_ALL<- data.matrix(Offense_ALL)
head(X)
X<- (scale(Offense_ALL, scale = T, center = T))

X = t(scale(t(Offense_ALL),center=TRUE, scale=TRUE))

sv = svd(t(X));
U = sv$u
V = sv$v
D = sv$d
Z = t(X)%*%V;

plot(Z[,1],Z[,2],type="n")
text(Z[,1],Z[,2],colnames(Offense_ALL),cex=.75)

head(Offense_ALL)

Dmat = dist(X,method="manhattan")
mdsres = cmdscale(Dmat,k=2)
plot(mdsres[,1],mdsres[,2],type="n")
text(mdsres[,1],mdsres[,2],rownames(X), cex=.5)

library(ggplot2)
mdsres<- data.frame(mdsres)
colnames(mdsres) <- c("MDS1", "MDS2")
Dmat = dist(X,method="manhattan")
com.hc = hclust(Dmat,method="ward.D2")
res.com = cutree(com.hc,6)

clusters <-data.frame(res.com)
ggplot(mdsres, aes(x=MDS1, y = MDS2, label = row.names(mdsres), colour = factor(res.com))) +
  geom_text(size=3) + 
  theme_bw() +
  ggtitle("Top 5 Offenses 2017-2018") +
  labs(colour = "Clusters")

clusters[which(clusters$res.com == grep("Bulls_1996", rownames(clusters), value=T)),]
clusters
