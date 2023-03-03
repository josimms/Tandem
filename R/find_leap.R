####
# Find Leap
####

find_leap <-  function(x){
  lubridate::day(x) == 29 & lubridate::month(x) == 2
}
