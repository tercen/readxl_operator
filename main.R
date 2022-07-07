library(tercen)
library(dplyr)
library(readxl)

is.POSIXct <- function(x) inherits(x, "POSIXct")

doc_to_data = function(df, sheet){
  filename = tempfile()
  writeBin(ctx$client$fileService$download(df$documentId[1]), filename)
  on.exit(unlink(filename))
  read_excel(filename, sheet = sheet) %>%
    mutate_if(is.POSIXct, as.character) %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(.ci= rep_len(df$.ci[1], nrow(.)))
}
 
ctx = tercenCtx()

if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 

sheet <- as.double(ctx$op.value('sheet'))

ctx$cselect() %>% 
  mutate(.ci= 1:nrow(.)-1L) %>%
  split(.$.ci) %>%
  lapply(doc_to_data, sheet = sheet) %>%
  bind_rows() %>%
  ctx$addNamespace() %>%
  ctx$save()
  
