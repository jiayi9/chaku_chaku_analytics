

# This function take a vector keep the first or last unique values by sequence.
nontieIndex = function(x, mode ="keepfirst", RETURN ='negative_removal_index'){
  
  print('mode = keepfirst or keeplast. RETURN = negative_removal_index or positive_keep_index')
  print(paste('Current mode:', mode))
  print(paste('Current RETURN:', RETURN))
  
  N = length(x)
  
  unique_N = length(unique(x))
  
  if(N <= 1) stop('Not many values (<=1)')
  
  if(N == unique_N) return(rep(TRUE, N))
  
  tmpTies = c()
  L = list()
  tieNo = 1
  
  for(i in 2:N){
    if(x[i] == x[i-1]) {
      tmpTies = union(tmpTies,c(i-1,i))
      L[[tieNo]] = tmpTies
    } else{
      tieNo = tieNo + 1
      tmpTies = c()
    }
  }
  
  if(mode == "keepfirst"){
    # remove the first element to keep it
    L = lapply(L, function(x) x[-1])
  } else if( mode == "keeplast"){
    L = lapply(L, function(x) x[-length(x)])
  } else {
    stop("it's either keepfirst or keeplast. check your code.")
  }
  
  tmpINDEX = do.call(c,L)
  
  if(RETURN == 'negative_removal_index'){
    R = -tmpINDEX
  } else if(RETURN == 'positive_keep_index'){
    R = (1:length(x))[-tmpINDEX]
  }
  
  return(R)
}

           
# add zeros for special ids
add_zeros = function(x, n=2){
  
  x = as.character(x)
  LEN = nchar(x)
  zeros = sapply(LEN, function(i){
    
    if(i >=  n) {
      RR = ''
    } else {
      RR = paste(rep(0,n-i), collapse = '')
    }
    RR
  })
  R = paste0(zeros,x)
  R
}

# remove zeros for special ids               
removeLeadingZero = function(t){
  substr(t,regexpr("[^0]",t),nchar(t))
}

               
