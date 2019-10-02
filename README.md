
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zipangu

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

このパッケージは、GISアプリケーションソフト等で利用される日本の国土に関する地理的データをR上で扱いやすく、tidyに処理することを目的としています。

データの用意は基本的にユーザが行ってください。パッケージは、対象のファイルを統一的なフォーマットで読み込む仕様です。

## インストール

パッケージのインストールはGitHubから行います。remotesパッケージが必要となるので、必要に応じてそちらをインストールしておきます。

``` r
# install.packages("remotes")
remotes::install_github("uribo/zipangu")
```

## 使い方

``` r
library(zipangu)
library(dplyr)

zipangu:::ksj_data_url("N03") %>% 
  filter(year == "2015", datum == "1", areaCode == "33") %>% 
  pull(zipFileUrl) %>% 
  download.file(url = .,
                basename(.))
  
files <- 
  list.files( pattern = ".zip$", full.names = TRUE)
unzip(files, exdir = stringr::str_remove(basename(files), ".zip"))
unlink(files)

stringr::str_remove(basename(files), ".zip") %>% 
  list.files(recursive = TRUE, 
             pattern = ".shp", 
             full.names = TRUE) %>% 
  read_ksj_n03()
```

## クレジット

このサービスは、「国土交通省 位置参照情報 <http://nlftp.mlit.go.jp/isj/index.html>
」および「国土交通省 国土数値情報
<http://nlftp.mlit.go.jp/ksj/index.html> 」をもとに加工者が作成したものです。
ダウンロードしたファイルを利用する際は、個別の利用約款をご確認ください。
