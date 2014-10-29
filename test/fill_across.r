
##Function that fills in matrix using across rows of wtatage data
##Missing Rows are backfilled
fill_across <- function(mat, minYear, maxYear)
{
  ##Initial Checks
  mat$yr <- abs(mat$yr)
  #input matrix must have value for year 1
  if(unique(mat$fleet) != 1) stop('Too Many Fleets')
  
  #Interpolate Values across Rows
  for(ii in 1:nrow(mat))
  {
    temp <- mat[ii, ]
    na.index <- which(is.na(temp))

    for(jj in na.index)
    {
      if(is.na(temp[jj]))
      {
        start.index <- jj - 1
        start <- temp[start.index]

        find.end <- jj
        
        #Find the end of the NA string
        while(is.na(temp[find.end]))
        {
          if(find.end == ncol(temp)) break
          find.end <- find.end + 1
        }

        # Fill across if end value missing
        if(find.end == ncol(temp))
        {
          end.index <- ncol(temp)
          temp[(start.index + 1):end.index] <- start
        }

        end.index <- find.end
        end <- temp[end.index]

        #Linearly Interpolate Between Start and End
        for(fill in (start.index+1):(end.index-1))
        {
          val <- start + (end - start) * (fill - start.index) / (end.index - start.index)
          temp[fill] <- val
          # print(fill)
        }
      } 
    }
    mat[ii, ] <- temp
  }

  #Create Temporary Data frame
  temp.df <- as.data.frame(matrix(nrow = length(seq(minYear, maxYear)), ncol = ncol(mat) ))
  temp.df[mat$yr, ] <- mat
  names(temp.df) <- names(mat)
  temp.df$yr <- seq(minYear, maxYear)

  #Back Fill Rows
  fill.index <- c(1, which(is.na(temp.df$age0) == FALSE))

  for(ii in 1:length(fill.index)){
    curr <- fill.index[ii]

    if(curr == 1) next

    prev <- fill.index[ii - 1]

    if(ii == 2)
    {
      temp.df[prev:(curr - 1), -1] <- temp.df[curr, -1]  
    } else {
      temp.df[(prev + 1):(curr -1), -1] <- temp.df[curr, -1]
    }
  }
  return(temp.df)
}

#Used For Testing
# setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical/test')
# test.dat <- read.csv('preFillMat.csv')

# mat <- test.dat
# mat$X <- NULL

# minYear <- 1
# maxYear <- 100

# xx <- fill_across(mat=mat, minYear = 1, maxYear = 100)
# write.csv(xx, file = 'check_fill_across.csv')