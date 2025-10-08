#Functions to live outside of the reactive codes

#ETHIOPIA BASE MAP

# MapBase<-  ggplot(data=r4) +
#     geom_raster(aes(x = x, y = y, fill = KG3_CRUTS32_Hist_8110raster)) +
#     geom_sf(data=ethiopia_regions_sf, fill=NA, linewidth=.8)+
#     scale_fill_manual(values=rev(mypalette))+ 
#     theme_void() 


#ETHIOPIA INTERACTIVE MAP
plotlymap <- function(pointColor, name){
   data1 <- data %>%
    mutate(txt=paste("ID: ", ID, "\nlongitude: ", lon, " ", "\nlatitude: ", lat, "\nSite: ", site, "\n", pointColor))
  unit <- SoilData_units[,2][SoilData_units$Name == name]
  if (!is.na (unit) & unit!="") {name <- paste(name, unit)}
  ggplotly(MapBase +
             geom_point(data = data1, aes(x = lon, y = lat, color = pointColor, text=txt), size = 3, alpha = 0.3) +
             scale_color_viridis(option="F", name = name) + labs(color=name)+
             coord_map()  , tooltip = "text", source = "Bubble Map")
}

#ETHIOPIA INTERACTIVE NO-SCALE MAP
noscalemap <- function(){
  ggplotly(MapBase +
             geom_point(data = data, aes(x = lon, y = lat, text=ID), color = "black", size = 3, alpha = 0.3) +
             coord_map(),  tooltip = "text", source = "No Scale Map")
}

################################
 #COMPOSITIONS
##############################
physiopie<-function(){
  if (length(chosenid()) > 0){
    soilcomp1 <- c(data$organic, data$Carbonated_lime, data$Clay, data$Silt, data$Sand)[data$ID == chosenid()]
    soilcomp1[is.na(soilcomp1)] = 0
  }else{
    soilcomp1 <- c(data$organic, data$Carbonated_lime, data$Clay, data$Silt, data$Sand)[data$ID == defaultID]
    soilcomp1[is.na(soilcomp1)] = 0
  }
  pie(soilcomp1, labels = c("Organic matter", "Carbonated lime", "Clay (<2 um)", "Silt (2-50 um)", "Sand (>50 um)"), border="white", col = rainbow(length(soilcomp1)),
      main = "Physical Attributes")
}

nut1fun<- function(){
  mgkgchoices<-c("ID", "N", "S_avail",	"S_total",	"P_avail",	"P_total",	"K_avail",	"Mg_avail", "Na_avail")
  mgkg<-data[mgkgchoices]
  if (length(chosenid()) > 0){
    mgkg<-mgkg[mgkg$ID == chosenid(),]#comma after logical indicates rows
    mgkg[is.na(mgkg)] = 0
  }else{
    mgkg<-mgkg[mgkg$ID == defaultID,]#comma after logical indicates rows
    mgkg[is.na(mgkg)] = 0
  }
  Meltedmgkg<-reshape2::melt(mgkg, id=c("ID"), variable.name="Variable", value.name="Value")
  ggplot(Meltedmgkg, aes(x=Variable,y=Value, fill=Variable)) +geom_bar(stat = 'identity') + labs(y="mg/kg", x="") + theme(legend.position='none') + 
    ggtitle("Nutrient Composition") 
}

nut2fun<- function(){
  mmkgchoices<-c("ID", "K_total",	"Ca_total",	"Mg_total",	"Na_total")
  mmkg<-data[mmkgchoices]
  if (length(chosenid()) > 0){
    mmkg<-mmkg[mmkg$ID == chosenid(),]#comma after logical indicates rows
    mmkg[is.na(mmkg)] = 0
  }else{
    mmkg<-mmkg[mmkg$ID == defaultID,]#comma after logical indicates rows
    mmkg[is.na(mmkg)] = 0
  }
  Meltedmmkg<-reshape2::melt(mmkg, id=c("ID"), variable.name="Variable", value.name="Value")
  ggplot(Meltedmmkg, aes(x=Variable,y=Value, fill=Variable)) +geom_bar(stat = 'identity') + labs(y="mmol/kg", x="") + theme(legend.position='none')
}

nut3fun<-function(){
  mmlchoices<-c("ID", "Ca_avail")
  mml<-data[mmlchoices]
  if (length(chosenid()) > 0){
    mml<-mml[mml$ID == chosenid(),]#comma after logical indicates rows
    mml[is.na(mml)] = 0
  }else{
    mml<-mml[mml$ID == defaultID,]#comma after logical indicates rows
    mml[is.na(mml)] = 0
  }
  Meltedmml<-reshape2::melt(mml, id=c("ID"), variable.name="Variable", value.name="Value")
  ggplot(Meltedmml, aes(x=Variable,y=Value, fill=Variable)) +geom_bar(stat = 'identity') + labs(y="mmol/l", x="") + theme(legend.position='none')
}

#microbe
microbepie<-function(){
  microbes <- as.matrix(data[, which(colnames(data) == "Aphelidiomycota"):which(colnames(data) == "Zixibacteria")])
  rownames(microbes)<-data[,1]
  if (length(chosenid()) > 0){
    submicrobes<-microbes[chosenid(),, drop=FALSE] #subset for selected ID
    submicrobes<-submicrobes[ , ! apply(submicrobes , 2 , function(x) all(is.na(x)) ) ] #removes NA columns entirely
  } else {
    submicrobes<-microbes[defaultID,, drop=FALSE] #subset for selected ID
    submicrobes<-submicrobes[ , ! apply(submicrobes , 2 , function(x) all(is.na(x)) ) ] #removes NA columns entirely
  }
  pie(submicrobes, border="white", col = rainbow(length(submicrobes)), main = "Microbe composition") 
}


#############################
  #CORRELATIONS
###########################

#CORR
CorrelationFun<-function(CorrType){
  FilteredDF<-rdf()
  UnmeltedFDF <- dcast(FilteredDF, ID + lon + lat ~ DataType, value.var="Value")
  xx<-UnmeltedFDF[,-(1:3)] #remove ID, lon & lat columns
  xx[is.na(xx)] = 0 #change NAs to 0s. Need to think of a more appropriate solution for this as 49 & 50 are just missing microbe data ad aren't 0s.
  xx<-remove_constant(xx, na.rm = FALSE, quiet = TRUE)
  C <- cor(xx,  use="pairwise.complete.obs", method=paste(CorrType))
  cortest<-corr.test(xx,method = paste(CorrType))$p 
  corrplot(C,type='lower',  
           method = 'color', 
           diag = FALSE, 
           p.mat = cortest, 
           insig = 'label_sig', 
           sig.level = c(0.001, 0.01, 0.05), 
           order="hclust", 
           tl.col="black")
} 
CorTableFun<-function(CorrType){
  if (input$graph == "Correlation Matrix"){
    FilteredDF<-rdf()
    UnmeltedFDF <- dcast(FilteredDF, ID + lon + lat ~ DataType, value.var="Value")
    xx<-UnmeltedFDF[,-(1:3)]
    C <- cor(xx,  use="pairwise.complete.obs", method=paste(CorrType))
    C
  }}

#PCA
PCAFun<-function(){
  FilteredDF<-rdf()
  UnmeltedFDF <- dcast(FilteredDF, ID + lon + lat ~ DataType, value.var="Value")
  row.names(UnmeltedFDF)<-UnmeltedFDF[,1]
  DataMatPRC<-data.matrix(UnmeltedFDF[,-(1:3)]) #make the values except ID, lon & lat into a matrix
  DataMatPRC[is.na(DataMatPRC)] = 0 #change NAs to 0s. Need to think of a more appropriate solution for this as 49 & 50 are just missing microbe data ad aren't 0s.
  DataMatPRC<-remove_constant(DataMatPRC, na.rm = FALSE, quiet = TRUE)
  soilpca<-prcomp(DataMatPRC, scale = TRUE)
  autoplot(soilpca, data=UnmeltedFDF, label=TRUE, shape=FALSE)
}

#HEATMAP
HeatmapFun<-function(){
  FilteredDF<-rdf()
  UnmeltedFDF <- dcast(FilteredDF, ID + lon + lat ~ DataType, value.var="Value")
  row.names(UnmeltedFDF)<-UnmeltedFDF[,1]
  UnmeltedFDF<-UnmeltedFDF[,-(2:3)] #remove lon & lat
  DataMat<-data.matrix(UnmeltedFDF[,-(1)]) #do not include ID in matrix
  DataMat<-remove_constant(DataMat, na.rm = FALSE, quiet = TRUE)
  p<-pheatmap(DataMat, cluster_cols = TRUE, cluster_rows = TRUE, clustering_distance_rows = "manhattan",
              scale="column", border_color = "gray80", cellwidth = 10, cellheight = 10)
  dev.off()
  p
}

#PAIRS CORRELATION
panel.rr <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  rr <- summary(lm(x~y))$r.squared # Remove abs function if desired
  txt <- paste0(prefix, format(c(rr, 0.123456789), digits = digits)[1])
  text(0.5, 0.5, txt) # Resize the text by level of correlation
}
PairsFun<-function(){
  FilteredDF<-rdf()
  UnmeltedFDF <- dcast(FilteredDF, ID + lon + lat ~ DataType, value.var="Value")
  xx<-UnmeltedFDF[,-(1:3)] #remove ID, lon & lat columns
  xx[is.na(xx)] = 0
  pairs(xx, lower.panel=reg, upper.panel=panel.rr) #graphics package
}
reg <- function(x, y, ...) {
  points(x,y, ...)
  abline(lm(y~x)) 
}  #makes regression line


#############################
  #MODELING
############################  

DecisionTree<- function(){
  tree <- rpart(paste(input$ModDVariable, "~ ."), data=Modrdf(), control=rpart.control(cp=.0000000001))
  prp(tree,
      faclen=0, #use full names for factor labels
      extra=1, #display number of obs. for each terminal node
      roundint=F, #don't round to integers in output
      digits=5) #display 5 decimal places in output
}

treeimportanceFUN<-function(variable){
  set.seed(71)
  modd<-variable
  formula<-paste(modd, "~ .")
  formula<-as.formula(formula)
  rf <-randomForest(formula, data=Modrdf(), importance=TRUE, ntree=500)  #Evaluate variable importance 
  #importance(rf)
  #varImpPlot(rf)
  imp = as.data.frame(importance(rf))
  imp = cbind(vars=rownames(imp), imp)
  imp = imp[order(imp$"%IncMSE"),]
  imp$vars = factor(imp$vars, levels=unique(imp$vars))
  imp %>% 
    pivot_longer(cols=matches("Inc")) %>% 
    ggplot(aes(value, vars)) +
    geom_col() +
    geom_text(aes(label=round(value), x=0.5*value), size=3, colour="white") +
    facet_grid(. ~ name, scales="free_x") +
    scale_x_continuous(expand=expansion(c(0,0.04))) +
    theme_bw() +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title=element_blank())
}
