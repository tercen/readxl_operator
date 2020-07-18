library(tercen)
library(dplyr)
library(readxl)

# http://localhost:53322/index.html#/alex/w/e11cea9d1119c8e6dbb0842925006475/ds/cb1e42fd-a9cb-459a-ab2f-86b297fa7163
# options("tercen.serviceUri"="http://172.17.0.1:5400/api/v1/")
# options("tercen.workflowId"= "e11cea9d1119c8e6dbb0842925006475")
# options("tercen.stepId"= "cb1e42fd-a9cb-459a-ab2f-86b297fa7163")
 
 
is.POSIXct <- function(x) inherits(x, "POSIXct")

doc_to_data = function(df){
  filename = tempfile()
  writeBin(ctx$client$fileService$download(df$documentId[1]), filename)
  on.exit(unlink(filename))
  read_excel(filename) %>%
    mutate_if(is.POSIXct, as.character) %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(.ci= rep_len(df$.ci[1], nrow(.)))
}
 
ctx = tercenCtx()

if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 
 
ctx$cselect() %>% 
  mutate(.ci= 1:nrow(.)-1) %>%
  split(.$.ci) %>%
  lapply(doc_to_data) %>%
  bind_rows() %>%
  ctx$addNamespace() %>%
  ctx$save()
  
