## Function to split on parentheses
## Helper function for termTraverse and regress
## Args: x - the string
## Returns: a string with no parens
## Version: 2015 05 25
splitOnParen <- function(x){
  char <- explode(x)
  if(length(char)>0){
    if(char[1]=="("){
      if(char[length(char)]==")"){
        return(paste(char[-c(1, length(char))], collapse=""))
      } else {
        return(paste(char[-1], collapse=""))
      }
    } else {
      return(x)
    }
  } else {
    return(x)
  }
}