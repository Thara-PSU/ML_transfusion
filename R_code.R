### Decision curve analysis 
#############################################
rm(list=ls())
Df<- read.csv("D:/MyWork/R/class21DCA/DCA_CT_ratio/probs_total.csv")

head(Df) # µÃÇ¨ÊÍºdataframe ·Õè 2 testing dataset
# Null column ID
#Df$ID <- NULL


#################### BOOK by rmda
library(rmda)



nb.model <- decision_curve(T_PRC~ probs_nb1,
                             data = Df,
                             thresholds = seq(0, .5, by = .01),
                             study.design = 'cohort',
                             bootstraps = 10) #number of bootstraps should be higher


GBC.model <- decision_curve(T_PRC~ probs_GBC,
                           data = Df,
                           thresholds = seq(0, .5, by = .01),
                           study.design = 'cohort',
                           bootstraps = 10) #number of bootstraps should be higher

svm.model <- decision_curve(T_PRC~ probs_svm1,
                            data = Df,
                            thresholds = seq(0, .5, by = .01),
                            study.design = 'cohort',
                            bootstraps = 10) #number of bootstraps should be higher


ann.model <- decision_curve(T_PRC~ probs_ann1,
                            data = Df,
                            thresholds = seq(0, .5, by = .01),
                            study.design = 'cohort',
                            bootstraps = 10) #number of bootstraps should be higher

knn.model <- decision_curve(T_PRC~ probs_knn1,
                            data = Df,
                            thresholds = seq(0, .5, by = .01),
                            study.design = 'cohort',
                            bootstraps = 10) #number of bootstraps should be higher

dt.model <- decision_curve(T_PRC~ probs_DT1,
                           data = Df,
                           thresholds = seq(0, .5, by = .01),
                           study.design = 'cohort',
                           bootstraps = 10) #number of bootstraps should be higher

rf.model <- decision_curve(T_PRC~ probs_rf1,
                           data = Df,
                           thresholds = seq(0, .5, by = .01),
                           study.design = 'cohort',
                           bootstraps = 10) #number of bootstraps should be higher

LR.model <- decision_curve(T_PRC~ probs_LR,
                           data = Df,
                           thresholds = seq(0, .5, by = .01),
                           study.design = 'cohort',
                           bootstraps = 10) #number of bootstraps should be higher




#plot the curve
plot_decision_curve(ann.model,  curve.names = "probs_ann1",confidence.intervals = FALSE,legend.position = "topright")

plot_decision_curve(GBC.model,  curve.names = "probs_GBC",confidence.intervals = FALSE,legend.position = "topright")

# add 95%CI
plot_decision_curve(GBC.model,  curve.names = "probs_GBC",confidence.intervals = TRUE,legend.position = "topright")
#########################################
## multi DCA 
#plot_decision_curve( list(baseline.model, nb.model),  
#                    curve.names =  c("Baseline model", "nb.model"),confidence.intervals = FALSE,legend.position = "none",cost.benefit.axis = FALSE)


plot_decision_curve( list(GBC.model, LR.model, nb.model, svm.model,ann.model,knn.model,rf.model,dt.model),  
                     curve.names =  c("GBC.model","LR.model", "nb.model","svm.model","ann.model","knn.model","rf.model","dt.model"),confidence.intervals = FALSE,
                     legend.position = "bottomright",cost.benefit.axis = FALSE)

plot_decision_curve( list(GBC.model, LR.model, nb.model, svm.model,ann.model,knn.model,rf.model,dt.model),  
                     curve.names =  c("GBC.model","LR.model", "nb.model","svm.model","ann.model","knn.model","rf.model","dt.model"),confidence.intervals = FALSE,
                     legend.position = "bottomright",cost.benefit.axis = TRUE)
###########################################################################
#########################################################################
### save tiff
tiff(filename = "Figure DCA_combine_label_ratio_NB_1 .tif",width = 10, height = 8, units = "in",res=600,compression = "lzw")
plot_decision_curve( list(ann.model,GBC.model,rf.model, LR.model, dt.model,nb.model,knn.model ,svm.model),  
                     curve.names =  c("ann.model","GBC.model","rf.model","LR.model","dt.model", "nb.model","knn.model","svm.model"),confidence.intervals = FALSE,
                     legend.position = "bottomright",cost.benefit.axis = FALSE)

dev.off()


############################################################################
###########################################################################
#standardizelogical (default TRUE) indicating whether 
#to use the standardized net benefit(NB/disease prevalence) or not

## stanardized Net Benefit
plot_decision_curve( list(CPR.model, nomo.model, rf.model, svm.model,ann.model,knn.model,nb.model),  
                     curve.names =  c("CPR.model","nomo.model", "rf.model","svm.model","ann.model","knn.model","nb.model"),confidence.intervals = FALSE,
                     legend.position = "bottomright",cost.benefit.axis = TRUE)

#plot_decision_curve( list(CPR.model, nb.model, svm.model,ann.model,knn.model,dt.model,rf.model),  
#                     curve.names =  c("Baseline model", "nb.model","svm.model","ann.model","knn.model","dt.model","rf.model"),confidence.intervals = FALSE,
#                     legend.position = "none",cost.benefit.axis = FALSE)

## non-standardized net benefit 
#plot Net benefit instead of standardized net benefit)
plot_decision_curve( list(CPR.model, nomo.model, rf.model, svm.model,ann.model,knn.model,nb.model),  
                     curve.names =  c("CPR.model","nomo.model", "rf.model","svm.model","ann.model","knn.model","nb.model"),confidence.intervals = FALSE,
                     legend.position = "bottomright",cost.benefit.axis = TRUE,
                     standardize = FALSE) 

#since we want to plot more than one curve, we pass a list of 'decision_curve' objects to the plot
plot_decision_curve( list(baseline.model, full.model), 
                     curve.names = c("Baseline model", "Full model"), xlim = c(0, 1), legend.position = "bottomright") 

#############

