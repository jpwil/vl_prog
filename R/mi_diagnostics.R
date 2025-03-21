########################## 
# IMPUTATION DIAGNOSTICS #
##########################

library(micemd)
library(tidyverse)

## CHOOSE IMPUTATION DATASETS
files <- list.files(path = "Analysis/MI/", pattern = "^[0-9]{10}_MICE\\.rdata$", ignore.case = TRUE)
files
file <- files[3]
load(paste0("Analysis/MI/", file))
mi_summarise(mice)
mice$fit$result
mice$fit
mice$var$result

## density plots
VS_BL_WEIGHT_MEAN     <- mean(mice$original$VS_BL_WEIGHT, na.rm = TRUE)
VS_BL_HEIGHT_MEAN     <- mean(mice$original$VS_BL_HEIGHT, na.rm = TRUE)

pdf("densityplot.pdf")
densityplot(mice$result)
dev.off()
system("open densityplot.pdf")

# trace plot
pdf("traceplot.pdf")
plot(mice$result)
dev.off()
system("open traceplot.pdf")

# strip plot
png("stripplot.png")
stripplot(mice$result)
dev.off()
#system("open stripplot.png")

pdf("densityplot_mb1.pdf")
densityplot(mice$result, ~ MB_COMBINEDs | .imp)
dev.off()
system("open densityplot_mb1.pdf")

pdf("xyplot1.pdf")
xyplot(mice$result, VS_BL_WEIGHTs ~ VS_BL_HEIGHTs | .imp)
dev.off()
#system("open xyplot1.pdf")

pdf("xyplot2.pdf")
xyplot(mice$result, VS_BL_WEIGHTs ~ DM_AGEs | .imp)
dev.off()
system("open xyplot2.pdf")

pdf("xyplot3.pdf")
xyplot(mice$result, VS_BL_HEIGHTs ~ DM_AGEs | .imp)
dev.off()
system("open xyplot3.pdf")

# passively imputed variables
pdf("xyplot4.pdf")
xyplot(mice$result, ZZ_BMIs ~ (VS_BL_WEIGHTs * 10 + VS_BL_WEIGHT_MEAN) / (((VS_BL_HEIGHTs * 10 + VS_BL_HEIGHT_MEAN) / 100)^2) | .imp)
dev.off()
#system("open xyplot4.pdf")

pdf("xyplot5.pdf")
xyplot(mice$result, ZZ_BMI_Z ~ (VS_BL_WEIGHTs * 10 + VS_BL_WEIGHT_MEAN) / (((VS_BL_HEIGHTs * 10 + VS_BL_HEIGHT_MEAN) / 100)^2) | .imp)
dev.off()
#system("open xyplot5.pdf")

pdf("xyplot6.pdf")
xyplot(mice$result, ZZ_AGEs2 ~ DM_AGEs | .imp)
dev.off()
#system("open xyplot6.pdf")

pdf("xyplot7.pdf")
xyplot(mice$result, ZZ_AGEs3 ~ DM_AGEs | .imp)
dev.off()
#system("open xyplot7.pdf")

pdf("xyplot8.pdf")
xyplot(mice$result, MP_BL_SPLEEN_LENGTHs2 ~ MB_COMBINEDs | .imp)
dev.off()
#system("open xyplot8.pdf")

## observations with models

# when modelling spleen size and parasite count as poisson variables 
# -- mice.impute.2stage.pois: they fail to converge (after removing studies with < 100 patients)
# -- mice.impute.2l.glm.pois: the first iteration fails! (after removing studies with < 100 patients)
# -- mice.impute.2l.2stage.pmm: this converges and gives reassuring distributions of imputed values (after removing studies < 100 patients)

# -- when including treatment effects in the imputation model, 2-stage modelling is not possible
# -- using one stage modelling (using glm), this is possible, however with the compromise of the homoscedasticity assumption
# -- using 2l.2stage.pmm for parasite and parasite size improves the density plots but does not preserve weight/height-age relationships!
