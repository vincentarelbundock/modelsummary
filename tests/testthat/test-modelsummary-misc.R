library(broom.mixed)
library(lme4)
library(modelsummary)

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
msummary(fm1, output = "markdown")
