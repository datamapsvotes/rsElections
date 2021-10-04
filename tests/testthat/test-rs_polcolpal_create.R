test_that("Polcolpal Generated",
         expect_is(object = rs_polcolpal_create(data.frame(a=c("ALP","COA"),c("#DE3533","#0047AB")), seq(0.5,0.95,0.05)), class = "rs_polcolpal")
)
