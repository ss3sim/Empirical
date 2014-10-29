
# head(wtatage)


# #Function to Fill in NaNs
# minYear <- datfile$styr
# maxYear <- datfile$endyr

# mat <- as.data.frame(matrix(nrow = 3, ncol = 32))
# names(mat) <- names(wtatage)

# mat[1, ] <- wtatage[4, ]
# mat[2, ] <- wtatage[4, ]
# mat[3, ] <- wtatage[5, ]

# mat[2, 1] <- 78
# mat[3, 1] <- 80

# mat[2, c(15, 28, 29, 30, 31, 32)] <- NaN
# mat[3, c(17, 27, 28, 29, 30, 31, 32)] <- NaN

# mat[2, c(7, 8, 14)] <- .858
# mat[2, 9] <- .310

# mat[3, c(9, 11, 13, 15)] <- .555
# mat[3, 14] <- .666

# fill_from_top(mat = mat, minYear = 1, maxYear = 100)

##Function that fills in matrix using first row of wtatage data


fill_from_top <- function(mat, minYear, maxYear)
{
  mat$yr <- abs(mat$yr)
  
  #input matrix must have value for year 1
  if(mat[1, 'yr'] %in% 1 == FALSE) stop('Provide Initial Values')
  if(unique(mat$fleet) != 1) stop('Too Many Fleets')
  
  #Replace NaN Values with values from first row
  for(ii in 1:nrow(mat))
  {
    for(jj in 1:ncol(mat))
    {
      temp <- mat[ii, jj]
      if(is.na(temp))
      {
        mat[ii, jj] <- mat[1, jj]
      }
    }
  }

  #Create Temporary Data frame
  temp.df <- as.data.frame(matrix(nrow = length(seq(minYear, maxYear)), ncol = ncol(mat) ))
  temp.df[mat$yr, ] <- mat
  names(temp.df) <- names(mat)
  temp.df$yr <- seq(minYear, maxYear)

  for(ii in 1:nrow(temp.df))
  {
    if(is.na(temp.df[ii, 'age0']))
    {
      temp.df[ii, -1] <- temp.df[ii - 1, -1] 
    }
  }
  return(temp.df)
}

