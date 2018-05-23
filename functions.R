##########################  functions ##################################
########################## Multi-valued Attribute ######################


extract.value<- function(a,b) {
  flag = FALSE
  for (j in a) {
    for (i in b){
      if(sapply(i, grepl, j)){
        value = i
        flag = TRUE
        break()
      }
    }
    if(flag){
      break()
    }
  }
  if(!flag){
    value = "unknown"
  }
  print(value)
}


#######################Binary Attrubute ######################

positive.words <- c("yes","sure","will","have","had")
negative.words <- c("no","not","uncertain","unsure")

classify.sentence <- function(a,b) {
  neg.sentence <- c()
  for (i in a) {
    for (j in b){
      if(sapply(j, grepl, i)){
        neg.sentence <- append(neg.sentence,i)
        break()
      }
    }
  }
  print(neg.sentence)
}

check.value<- function(a,b) {
  flag = FALSE
  for (j in a) {
    for (i in b){
      if(sapply(i, grepl, j)){
        flag = TRUE
        break()
      }
    }
    if(flag){
      break()
    }
  }
  print(flag)
}

