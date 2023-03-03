####
# Muted
####
muted <- khroma::colour("muted")

####
# RData to onject - ricardo stack exchange!
####
loadRData <- function(fileName){
  # https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
