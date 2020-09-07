context('glance_custom')

library(modelsummary)

# # for some reason the new method won't get picked up inside
# test_that("fixed value glance_custom.glm", {
#   glance_custom.glm <- function(x) {
#     data.frame("test"=1.54, "test2"="lkkd", "test3"=as.integer(2),
#                "test4"=TRUE)
#   }
#   mod <- glm(am ~ mpg, mtcars, family = binomial)
#   out <- msummary(mod, "data.frame")
#   expect_equal(dim(out), c(12, 4))
#   rm("glance_custom.glm")
# })

