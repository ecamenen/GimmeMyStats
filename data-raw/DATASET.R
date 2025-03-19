temp_file <- tempfile(fileext = ".txt")
download.file(
  "https://raw.githubusercontent.com/kassambara/ggpubr/refs/heads/master/inst/demo-data/housetasks.txt",
  destfile = temp_file,
  mode = "w"
)
housetasks  <- read.delim(temp_file, row.names = 1)
usethis::use_data(housetasks, overwrite = TRUE)
