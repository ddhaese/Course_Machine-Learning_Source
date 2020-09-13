library(magrittr)

check_code_fences <- function(filename){
  print(filename)
  filetxt <- readChar(filename, file.info(filename)$size) %>%
    strsplit("\n") %>% unlist
  xfun::prose_index(filetxt)
}
 
# options(warn = 2)
# 
# tmp <- list.files(, pattern = "*.Rmd") %>% lapply(check_code_fences)

## From https://stackoverflow.com/questions/45591286/for-r-markdown-how-do-i-display-a-matrix-from-r-variable
# Blended, Student at Sungkyunkwan university
write_matex <- function(x, decimals = 3, prefix = "", suffix = "",
  replacements = c("9999" = "\\vdots")){
  begin <- paste0("$$", prefix, "\\begin{bmatrix}")
  end <- paste0("\\end{bmatrix}", suffix, "$$")
  xs <-
    apply(x, 1, function(xi) {
      paste(
        paste(ifelse(as.character(xi) %in% names(replacements),
          replacements[as.character(xi)],
          sprintf(paste0("%.", decimals, "f"), xi))
          
          
          , collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c(begin, xs, end))
}
