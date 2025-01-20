####
# Muted
####
colours_cb <- khroma::colour("vibrant")
colours_cb_245 <- khroma::colour("highcontrast")
colours_cb_585 <- khroma::colour("highcontrast")

colours_pale <- khroma::colour("light")

colours_cb_2 <- khroma::colour("vibrant")

####
# RData to onject - ricardo stack exchange!
####
loadRData <- function(fileName){
  # https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
