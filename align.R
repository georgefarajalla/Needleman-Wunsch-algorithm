score <- function(a, b){
  
  gap_penalty <- -2
  mismatch <- -1
  match <- 1
  
  if(identical(a,b) == TRUE){return(match)}
  else if(identical(a, '-') == TRUE | identical(b, '-') == TRUE){return(gap_penalty)}
  else{return(mismatch)}
  
}

needle <- function(seq1, seq2){
  
  i <- length(seq1)
  j <- length(seq2)
  
  gap_penalty <- -2
  mismatch <- -1
  match <- 1
  
  mat <- matrix(0, nrow = j + 1, ncol = i + 1)
  
  mat[1,1] = 0
  
  ## Fill the first column
  for(ii in 2:nrow(mat)){
    mat[ii,1] <- (ii-1) * gap_penalty
  }
  
  ## Fill the first row
  for(ii in 2:ncol(mat)){
    mat[1,ii] <- (ii-1) * gap_penalty
  }
  
  for(ii in 2:nrow(mat)){
    for(jj in 2:ncol(mat)){
      
      match <- mat[ii - 1,jj - 1] + 
        score(seq1[jj - 1], seq2[ii - 1])
      
      delete <- mat[ii - 1, jj] + gap_penalty
      
      insert <- mat[ii, jj - 1] + gap_penalty
      
      mat[ii, jj] <- max(match, delete, insert)
      
    }
  }
  
  align1 <- ''
  align2 <- ''
  
  ## Starting from the bottom right cell
  m <- nrow(mat)
  n <- ncol(mat)
  
  while(m > 1 & n > 1){
    
    current <- mat[m, n]
    diagonal <- mat[m - 1, n - 1]
    up <- mat[m, n - 1]
    left <- mat[m - 1, n]
    
    if(current == diagonal + score(seq1[n-1],seq2[m-1])){
      align1 = paste(align1, seq1[n-1], sep='')
      align2 = paste(align2, seq2[m-1], sep='')
      m = m - 1
      n = n - 1
    }
    
    else if(current == up + gap_penalty){
      align1 = paste(align1, seq1[n-1], sep='')
      align2 = paste(align2, '-', sep='')
      n = n - 1
    }
    
    else if(current == left + gap_penalty){
      align2 = paste(align2, seq2[m-1], sep='')
      align1 = paste(align1, '-', sep='')
      m = m - 1
    }
    
  }
  
  while(n > 1){
    align1 = paste(align1, seq1[n-1], sep='')
    align2 = paste(align2, '-', sep='')
    n = n - 1
  }
  
  while(m > 1){
    align2 = paste(align2, seq2[m-1], sep='')
    align1 = paste(align1, '-', sep='')
    m = m - 1
  }
  
  revString <- function(string, index = 1:nchar(string)){
    paste(rev(unlist(strsplit(string, NULL)))[index], collapse = "")
  }
  
  align1 <- revString(align1)
  align2 <- revString(align2)
  
  return(c(align1, align2))
}

needle(c("A", "T", "T", "A", "C", "A"), c("A", "T", "G", "C", "T"))



