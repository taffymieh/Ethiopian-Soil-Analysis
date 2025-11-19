##GJAM analysis conducted by Marcio Fernandes Alves Leite. ##

# Load necessary package
if (!requireNamespace("gjam", quietly = TRUE)) {
  install.packages("gjam")
}
library(gjam)

# Assuming your data frames are:
# Seq16S - microbial abundance data (bacteria)
# SeqITS - microbial abundance data (fungi)
# xdata.Inf - explanatory data for model 1 (Striga infection)
# xdata - explanatory data for model 2 (Suicidal germination)
# xdata.Seed - explanatory data for model 3 (Striga Seedbank)

# Trim abundance data with minimum observations of 5
Seq16S_DF <- data.frame(Seq16S[,-c(1,ncol(Seq16S))])
rownames(Seq16S_DF) <- Seq16S$`#OTU ID`
SeqITS_DF <- data.frame(SeqITS[,-c(1,ncol(SeqITS))])
rownames(SeqITS_DF) <- SeqITS$`#OTU ID`
trimmed_16S <- gjamTrimY(t(Seq16S_DF), minObs = 5)
trimmed_ITS <- gjamTrimY(t(SeqITS_DF), minObs = 5)

rownames(SuicideStriga) <- SuicideStriga$Soil
rownames(SoilInfo) <- SoilInfo$`Sample code`
ydata <- cbind(data.frame(StrigaInfo[,6:7]),
               data.frame(SuicideStriga[,6:7]),
               data.frame(SoilInfo[,-1]),
               trimmed_16S$y[StrigaInfo$`Sample code`,],trimmed_ITS$y[StrigaInfo$`Sample code`,])  # for looking only at the microbes
# Define the model parameteres
typeNames    <- c(rep('CA',38),
                  rep('CC',ncol(y)),
                  rep('CC',ncol(yF)))   # composition count data
CCgroupsVar = c(rep(0,38),rep(1,ncol(trimmed_16S$y)),rep(2,ncol(trimmed_ITS$y)))
ml <- list(ng = 5000, burnin =1000, typeNames = typeNames, CCgroups =CCgroupsVar) 

# Fit Model 1 - Effect of Striga infection
model1_fit <- gjam(~ GroupVar, xdata = xdata.Inf, ydata = trimmed_ydata)

# Fit Model 2 - Effect of Suicidal germination
model2_fit <- gjam(~ GroupVar, xdata = xdata, ydata = trimmed_ydata)

# Fit Model 3 - Effect of Striga Seedbank
model3_fit <- gjam(~ GroupVar, xdata = xdata.Seed, ydata = trimmed_ydata)

# Summaries of models
summary(model1_fit)
summary(model2_fit)
summary(model3_fit)

# Optional: Save the models results for later use
# save(model1_fit, model2_fit, model3_fit, file = "GJAM_models_results.RData")
