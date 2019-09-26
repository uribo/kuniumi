test_that("ksj n03 works", {
  expect_equal(zip_n03_url(2018, "08"),
               "http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2018/N03-180101_08_GML.zip") # nolint
  expect_equal(zip_n03_url(2018, 8),
               zip_n03_url(2018, "8"))
})
test_that("ksj a30 works", {
  expect_equal(zip_a30a5_url(meshcode = 4229),
               "http://nlftp.mlit.go.jp/ksj/gml/data/A30a5/A30a5-11/A30a5-11_4229-jgd_GML.zip") # nolint
})
test_that("ksj c23 works", {
  expect_equal(zip_c23_url(pref_code = 33),
               "http://nlftp.mlit.go.jp/ksj/gml/data/C23/C23-06/C23-06_33_GML.zip") # nolint
})

test_that("ksj n05 works", {
  expect_equal(zip_n05_url(year = 2013),
               "http://nlftp.mlit.go.jp/ksj/gml/data/N05/N05-13/N05-13.zip") # nolint
  expect_equal(zip_n05_url(year = 2016),
               "http://nlftp.mlit.go.jp/ksj/gml/data/N05/N05-16/N05-16_GML.zip") # nolint
})

test_that("ksj p09", {
  expect_equal(zip_p09(meshcode = "3927"),
               "http://nlftp.mlit.go.jp/ksj/gml/data/P09/P09-10/P09-10_3927-jgd_GML.zip") # nolint
  expect_error(zip_p09(meshcode = "6338"),
               "No data with selected meshcode.")
})

test_that("ksj p23", {
  expect_equal(zip_p23_url(33),
               "http://nlftp.mlit.go.jp/ksj/gml/data/P23/P23-12/P23-12_33_GML.zip") # nolint
  expect_error(zip_p23_url("09"),
               "There is no target prefecture data.")
})

test_that("don't works", {
  expect_error(
    read_ksj_n05(.year = 2019)
  )
})
