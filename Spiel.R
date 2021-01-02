multSpiele <- function(anzahl, rabeWin=6, showText=FALSE, returnDataFrame=TRUE) {
  ergebnis <- NULL
  numTrue <- 0
  numFalse <- 0
  for(i in 1:anzahl) {
    erg <- obstgartenSpiel(rabeWin, showText)
    ergebnis <- c(ergebnis, erg)
    if(erg) {
      numTrue <- numTrue +1
    } else {
      numFalse <- numFalse +1
    }
  }
  if(returnDataFrame) {
    return(c(RabeVerliert=numTrue, RabeGewinnt=numFalse))
  } else {
    return(ergebnis)
  }
}

obstgartenSpiel <- function(rabeWin=5, showText=FALSE) {
  fruechte <<- c(4,4,4,4)
  rabe <<- 0
  rabeWin <<- rabeWin
  
  ende <- FALSE
  if(showText) {
    cat(paste("Ein neues Spiel beginnt! Fruechte:", paste(fruechte, collapse = ","), "Rabe:",rabe, "RabeWin:", rabeWin,"\n", sep=" "))
  }
  
  while(!ende) {
    fruechte=spielrunde(fruechte,showText=showText)
    ende <- spielende(fruechte, showText=showText)
  }
  return(gewonnen(fruechte, showText=showText))
}

spielrunde <- function(fruechte, showText=FALSE) {
  wurf <- sample(1:6, 1)
  if(showText) {
    cat(paste("Es wurde eine",wurf,"gewürfelt!\n", sep=" "))
  }
  if(wurf==6){
    rabe <<- rabe + 1
    if(showText) {
      cat(paste("Der Rabe bewegt sich und ist nun auf Position ", rabe,"!\n", sep=""))
    }
  } else if(wurf==5) {
    fruechte=obstkorb(fruechte, showText=showText)
  } else {
    fruechte=fruchtNehmen(fruechte, wurf, showText=showText)
  }
  return(fruechte)
}

fruchtNehmen <- function(fruechte, num, showText=FALSE) {
  if(fruechte[num]>0) {
    fruechte[num] <- fruechte[num]-1
    if(showText) {
      cat(paste("Eine Frucht wurde genommen. Verbleibende Früchte: ", paste(fruechte, collapse = ","), "\n", sep=""))
    }
  } else {
    if(showText) {
      cat(paste("Es wurde keine Frucht genommen. Verbleibende Früchte: ", paste(fruechte, collapse = ","), "\n", sep=""))
    }
  }
  return(fruechte)
}

obstkorb <- function(fruechte, showText=FALSE) {
  index <- indexOfMax()
  if(index>1) {
    index1 <- sample(index, 1)
    
  } else {
    index1 <- index
  }
  return(fruchtNehmen(fruechte, index1, showText=showText))
}

indexOfMax <- function() {
  maximum <- max(fruechte)
  index <- NULL
  for(i in 1:length(fruechte)) {
    if(fruechte[i]==maximum) {
      index <- c(index, i)
    }
  }
  return(index)
}

spielende <- function(fruechte, showText=FALSE) {
  if(rabe>=rabeWin || sum(fruechte)==0) {
    if(showText) {
      cat("Das Spiel ist zuende!")
    }
    return(TRUE)
  }
  return(FALSE)
}

gewonnen <- function(fruechte, showText=FALSE) {
  if(rabe>=rabeWin) {
    if(showText) {
      cat(" --> Der Rabe gewinnt! \n\n\n")
    }
    return(FALSE)
  } else {
    if(showText) {
      cat(" --> Der Rabe verliert! \n\n\n")
    }
    return(TRUE)
  }
}