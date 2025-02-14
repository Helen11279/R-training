#Performing Principal Components Analysis 
#Yu-Jing Chen
#ychen10@swarthmore.edu

otter <- read.csv(file = "data/otter-mandible-data.csv",
                  stringsAsFactors = TRUE)
otter <- na.omit(otter)

##renumber rows after dropping off rows with incomplete data
rownames(otter) = NULL

#PCA = Principal Component Analysis = a way of re-describing the variations in data
pca_fit <- prcomp(x = otter[,-c(1:3)], scale. = TRUE)
pcasummary <- summary(pca_fit)otter <- read.csv(file = "data/otter-mandible-data.csv",
                                                stringsAsFactors = TRUE)
otter <- na.omit(otter)

##renumber rows after dropping off rows with incomplete data
rownames(otter) = NULL

#PCA = Principal Component Analysis = a way of re-describing the variations in data
pca_fit <- prcomp(x = otter[,-c(1:3)], scale. = TRUE)
pcasummary <- summary(pca_fit)
ls(pcasummary)
pcasummary$importance
pcasummary$rotation
direction <- "positive"
if (sum(pcasummary$rotation) < 0) {
  direction <- "negative" 
}

#Defining color legends and displaying species names
species_names <- unique(otter$species)
species_names

legend_cols <- c("black", "green4", "red3", "cyan3")
pt_cols <- rep(x = legend_cols[1], length = nrow(otter))
pt_cols[otter$species == species_names[2]] <- legend_cols[2]
pt_cols[otter$species == species_names[3]] <- legend_cols[3]
pt_cols[otter$species == species_names[4]] <- legend_cols[4]


#Plot results
biplot(x = pca_fit)

plot(x = pca_fit$x[, 1],
     y = pca_fit$x[, 2],
     xlab = "PC 1",
     ylab = "PC 2",
     pch = 19,
     col = pt_cols,)
legend("bottomleft", 
       legend = species_names, 
       pch = 19, 
       col = legend_cols, 
       cex = 0.8)

#Plotting box plots
par(mfrow = c(2, 3), las = 2) 
boxplot(formula = m1 ~ species, data = otter, xlab = NA)
boxplot(formula = m2 ~ species, data = otter, xlab = NA)
boxplot(formula = m3 ~ species, data = otter, xlab = NA)
boxplot(formula = m4 ~ species, data = otter, xlab = NA)
boxplot(formula = m5 ~ species, data = otter, xlab = NA)
boxplot(formula = m6 ~ species, data = otter, xlab = NA)



