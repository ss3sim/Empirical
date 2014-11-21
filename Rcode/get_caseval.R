get_caseval <- 
function (scenario, case) 
{
    if (!grepl("-", scenario)) 
        stop("Your case string doesn't contain your delimiter.")
    if (!is.character(scenario)) 
        stop("cases must be of class character")
    if (!is.character(case)) 
        stop("case must be of class character")
    x <- strsplit(scenario, "-")[[1]]
    as.numeric(substr(x[grep(case, x)], 2, 9))
}

get_args <- 
function (file) 
{
    x <- read.csv(file, stringsAsFactors = FALSE, col.names = c("arg", 
        "val"), header = FALSE, strip.white = TRUE, sep = ";", 
        comment.char = "#", quote = "")
    y <- as.list(x$val)
    names(y) <- x$arg
    lapply(y, function(z) {
        if (!is.na(z)) {
            y <- tryCatch(eval(parse(text = z)), error = function(e) as.character(z))
            if (is.function(y)) {
                as.character(z)
            }
            else {
                y
            }
        }
        else {
            NA
        }
    })
}
