test_that("multiplication works", {
  expect_length(ksj_code_list,
                6L)
  expect_equal(
    dim(ksj_code_list$landuse$`2016`),
    c(17, 4)
  )
  expect_equal(
    c(na.omit(ksj_code_list$landuse$`2016`$color)),
    c("#FFFF00", "#FFCC99", "#00AA00", "#FF9900", "#FF0000", "#8C8C8C",
      "#B4B4B4", "#C8460F", "#0000FF", "#FFFF99", "#00CCFF", "#00FF00"
    )
  )
})
