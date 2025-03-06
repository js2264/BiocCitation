#' Check all installed Bioconductor packages for missing CITATION files or DOIs.
#' 
#' @param retrieve_missing_DOIs A logical value indicating whether to 
#'   retrieve missing DOIs from CrossRef.
#' 
#' @return A data.frame of packages with CITATION files and DOIs.
#' 
#' @examples
#' \dontrun{
#' check_citations()
#' }
#' 
#' @importFrom devtools package_info
#' @importFrom S4Vectors DataFrame
#' @importFrom rcrossref cr_works
#' @importFrom stringr str_subset
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' 
#' @export

check_citations <- function(retrieve_missing_DOIs = TRUE) {
    
    # List all installed Bioconductor packages
    installed <- rownames(installed.packages()) |> devtools::package_info()
    bioc_pkg <- S4Vectors::DataFrame(
        pkg = installed[grepl("Bioconductor", installed$source), 'package']
    )

    # Check for CITATION files and DOIs
    citation <- logical(nrow(bioc_pkg))
    dois <- character(nrow(bioc_pkg)) |> as("SimpleList")
    names(citation) <- bioc_pkg$pkg
    names(dois) <- bioc_pkg$pkg
    for (pkg in bioc_pkg$pkg) {
        citationf <- system.file('CITATION', package = pkg)
        if (citationf != "") {
            citation[pkg] <- TRUE
            doi <- readLines(citationf) |>
                stringr::str_subset("doi\\s+=") |>
                stringr::str_replace(".*\\s+", "") |> 
                stringr::str_extract("10\\.\\d{4,}/.*") |> 
                stringr::str_replace_all("\"", "") |> 
                stringr::str_replace_all(",", "") |> 
                # Extra filters for individual packages
                stringr::str_subset("10.1101/TODO", negate = TRUE) # biocthis
            message(pkg, ' --- ', doi)
            if (length(doi) == 0) {
                dois[[pkg]] <- ""
            } 
            else if (all(is.na(doi))) {
                dois[[pkg]] <- ""
            }
            else {
                dois[[pkg]] <- doi
            }
        }
    }
    bioc_pkg$citation <- citation
    bioc_pkg$doi <- dois
    message(
        paste0(
            sum(bioc_pkg$citation & 
                {lapply(bioc_pkg$doi, function(x) {any(x != "")}) |> unlist()}
            ), " packages with CITATION file and DOI"),
        "\n", 
        paste0(
            sum(bioc_pkg$citation & 
                {lapply(bioc_pkg$doi, function(x) {all(x == "")}) |> unlist()}
            ), " packages with CITATION file but no DOI"),
        "\n", 
        paste0(sum(!bioc_pkg$citation), " packages with missing CITATION files")
    )

    if (retrieve_missing_DOIs) {
        has_no_doi <- lapply(bioc_pkg$doi, function(x) {all(x == "")}) |> unlist()
        has_cit_and_no_doi <- bioc_pkg$citation & has_no_doi
        i <- 0
        for (pkg in bioc_pkg$pkg[has_cit_and_no_doi]) {
            cit_str <- .processRecords(pkg)
            doi <- lapply(cit_str, .getDoi)
            if (any(doi != "")) {
                i <- i + 1
                bioc_pkg$doi[[pkg]] <- doi
            }
            message(paste0(i, " DOIs retrieved from CrossRef"))
        }
    }
    return(bioc_pkg)
}

.processRecords <- function(pkg) {
    format(citation(pkg), style = 'text') |> 
        strsplit("\n\n") |> 
        unlist() |> 
        stringr::str_replace("^\\s+", "") |>
        tolower() |> 
        stringr::str_replace_all("\\s+|\n|</*.>", " ") |> 
        stringr::str_replace_all("<.*>", "")
}

.getDoi <- function(cit_str) {
    tryCatch({
        tmp <- rcrossref::cr_works(flq = c(query.bibliographic = cit_str), limit = 1)$data
        if (agrepl(
            tmp$title, 
            cit_str, 
            fixed = TRUE, 
            max.distance = 0.1, 
            ignore.case = TRUE
        )) {
            return(tmp$doi)
        } 
        else {return("")}
    })
}
