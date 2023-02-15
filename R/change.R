#' Kategorisiere dosage form
#'
#' \code{change} kategorisiert \code{dosage_form} aus dem data frame.
#'
#'
#' @return ein kategorisierte frame.
#'
#' @param a die attribut, die kategorisiert werden muss.
#'
#' @examples
#' change(c("TABLET, EXTENDED", "SOLUTION"))
#'
change<-function(a){
  result<-rep_len(0,length(a))
  for(i in 1:length(a)){
    if(startsWith(a[i], "INJECTION"))result[i]<-"INJECTION"
  else if (startsWith(a[i], "CAPSULE"))result[i]<-"CAPSULE"
  else if (startsWith(a[i], "LIQUID"))result[i]<-"LIQUID"
  else if (startsWith(a[i], "LOTION"))result[i]<-"LOTION"
  else if (startsWith(a[i], "TABLET"))result[i]<-"TABLET"
  else if (startsWith(a[i], "CREAM"))result[i]<-"CREAM"
  else result[i]<-"OTHER"
  }
  result
}
