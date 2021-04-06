## paste every two together
## A helper function for termTraverse, regress
## Args: vec - the vector to paste together
## Returns: a vector with every two elements pasted together
## Version: 2015 05 25
pasteTwo <- function(vec){
  if(length(vec)==2){
    return(paste(vec[1], vec[2], sep=""))
  } else {
    one <- pasteTwo(vec[1:2])
    two <- pasteTwo(vec[-c(1:2)])
    return(c(one, two))
  }
}