## Function which takes in a string and returns a vector of individual characters
## A helper function for regress, print.uRegress
## Args: str - the string
## Returns: A vector of the characters
## Version: 2015 05 25
explode <- function(str){
  chars <- strsplit(str, "")[[1]]
  return (chars)
}