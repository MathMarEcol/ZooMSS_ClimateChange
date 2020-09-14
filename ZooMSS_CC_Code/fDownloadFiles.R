fDownloadFiles = function(files, base_url, out_dir){
  library(curl)
  library(tidyverse)
  urls <- paste0(base_url, files)
  dest_files <- paste0(out_dir, str_extract(files, "[^//]+$"))
  map2(urls, dest_files, function(urls, dest_files) curl_download(urls, dest_files, FALSE))
}
