test_that("ksj n03 works", {
  expect_equal(zip_n03_url(2018, "08"),
               "http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2018/N03-180101_08_GML.zip") # nolint
  expect_equal(zip_n03_url(2018, 8),
               zip_n03_url(2018, "8"))
})
