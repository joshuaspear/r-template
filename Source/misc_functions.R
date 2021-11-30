
#### Miscelaneous functions

merge_wrapper <- function(df1, df2, ...) {
  # Wrapper funcion around the standard R merge function. This function prints 
  # a warning if the output table is a different dimension to the df1 input 
  # table  
  
  # df1: dataframe object
  # df2: dataframe object
  # ... : optional paramaters which are sent to the merge object
  
  pre_join_dim = dim(df1)[1]
  res = merge(df1, df2, ...)
  post_join_dim = dim(res)[1]
  if (pre_join_dim != post_join_dim) {
    log_warn("Dimensions are different post join")
  }
  return(res)
}


delete_create_dirs <- function(dir_to_create, drop_dirs){
  # Function to drop and create a director depending on a switch
  
  # dir_to_create: string representing the directory to create
  # drop_dirs: boolean controlling whether to overwrite the directory if it 
  # exists
  
  # returns: TRUE
  log_debug(dir.exists(dir_to_create))
  if (dir.exists(dir_to_create)) {
    
    if (drop_dirs){
      unlink(dir_to_create, recursive = TRUE)
      # Create new directory 
      dir.create(dir_to_create)
    } else {
      log_warn(glue::glue("{dir_to_create} not dropped and re created as the directory already exists.
                        Set drop_dirs = TRUE to re create it"))
    }
  } else {
    unlink(dir_to_create, recursive = TRUE)
    # Create new directory 
    dir.create(dir_to_create)
    
  }
  
  
  return(TRUE)
}

### Derive standard errors from confidence intervals
ci2se <- function(uCI, lCI){
  
  SE = (uCI-lCI) / 3.92
  
  return(SE)
}

### Derive standard errors from correlations (Pearson's r)
cor2se <- function(n, r) {
  
  SE = (1 - r^2) / sqrt(n - 2)
  
  return(SE)
}

### Derive standardised betas from unstandardised b
b2beta <- function(b, sd_exposure, sd_outcome){
  
  B = (b*sd_exposure)/sd_outcome
  
  return(B)
}

### Derive standardised standard errors from unstandardised standard errors
unes2es <- function(b_se, sd_exposure, sd_outcome){
  
  x = (sd_exposure/sd_outcome)*b_se
  
  return(x)
}
