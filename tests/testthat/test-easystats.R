library(lme4)
library(modelsummary)

d <- as.data.frame(ChickWeight)
colnames(d) <- c("y", "x", "subj", "tx")
fit <<- lmer(y ~ tx * x + (x | subj), data = d)

modelsummary:::glance_easystats(fit)
modelsummary:::tidy_easystats(fit)


k = try(tidy(fit), silent=TRUE)
k = try(glance(fit), silent=TRUE)

