get_caseargs <- 
function (folder, scenario, ext = ".txt", case_files = list(M = "M", 
    F = "F", D = c("index", "lcomp", "agecomp"), R = "R", E = "E")) 
{
    case_vals <- names(case_files)
    spp <- substr(scenario, max(grep("-", strsplit(scenario, 
        NULL)[[1]])) + 1, nchar(scenario))
    if (grepl("^[A-Z][0-9]+$", spp)) {
        message(paste("Using", spp, "as the stock ID.\n"))
    }
    scenario <- substr(scenario, 1, nchar(scenario) - nchar(spp) - 
        1)
    out_sink <- sapply(case_vals, function(x) {
        if (!grepl(x, scenario)) 
            stop(paste("Case", x, "isn't contained in scenario", 
                scenario))
    })
    scenario_cases <- gsub("[0-9]*", "", strsplit(scenario, "-")[[1]])
    missing_casefiles <- scenario_cases[!scenario_cases %in% 
        case_vals]
    if (length(missing_casefiles) > 0) {
        stop(paste("The case", missing_casefiles, "is declared in your scenario ID but not in the argument case_files.\n"))
    }
    case_vals <- sapply(case_vals, function(x) get_caseval(scenario, 
        x))
    args_out <- vector("list", length = length(case_files))
    names(args_out) <- names(case_files)
    for (i in 1:length(case_files)) {
        args_out[[i]] <- paste0(case_files[[i]], case_vals[i], 
            "-", spp, ext)
    }
    args_out2 <- unlist(args_out)
    names(args_out2) <- unlist(case_files)
    argvalues_out <- lapply(args_out2, function(x) get_args(pastef(folder, 
        x)))
    change_param_args <- sapply(argvalues_out, function(x) {
        if ("function_type" %in% names(x)) {
            if (x$function_type == "change_tv") {
                y <- list(temporary_name = x$dev)
                names(y) <- x$param
                y
            }
        }
    })
    args_null <- sapply(change_param_args, function(x) is.null(x))
    if (!length(which(args_null)) == length(args_null)) {
        change_param_args[which(args_null)] <- NULL
        change_param_args_short <- lapply(change_param_args, 
            "[[", 1)
        names(change_param_args_short) <- sapply(change_param_args, 
            function(x) names(x))
    }
    else {
        change_param_args_short <- NULL
    }
    argvalues_out <- argvalues_out[which(args_null)]
    for (i in seq_along(argvalues_out)) {
        change_case_function <- paste0("change_", tolower(names(argvalues_out)[i]))
        sample_case_function <- paste0("sample_", tolower(names(argvalues_out)[i]))
        if (change_case_function == "change_r") 
            change_case_function <- "change_retro"
        fxn_name <- change_case_function
        fxn_formals <- tryCatch(names(formals(change_case_function)), 
            error = function(e) "")
        if (fxn_formals[1] == "..." | fxn_formals[1] == "") {
            fxn_formals <- names(formals(sample_case_function))
            fxn_name <- sample_case_function
        }
        matches <- names(argvalues_out[[i]]) %in% fxn_formals
        if (sum(matches) != length(matches)) {
            stop(paste0(names(argvalues_out[[i]])[!matches], 
                " is not an argument in the function ", fxn_name))
        }
    }
    c(argvalues_out, list(tv_params = change_param_args_short))
}
