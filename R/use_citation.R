#' Create or update CITATION file using a DOI.
#'
#' This function creates a CITATION file from a DOI. 
#' The CITATION file is created in the inst/ directory of the package.
#' 
#' @param doi A character string representing the DOI of the article.
#' @param output A character string representing the path to the output file.
#'   Default is "inst/CITATION".
#' 
#' @return A CITATION file is created in the inst/ directory of the package.
#' 
#' @examples
#' \dontrun{
#' use_citation("10.1038/s41586-020-2818-3")
#' }
#' 
#' @importFrom rcrossref cr_cn
#' @importFrom stringr str_glue
#' 
#' @export

use_citation <- function(doi, output = "inst/CITATION") {
    
    # Check that output folder exists
    if (!dir.exists(dirname(output))) {
        response <- readline(prompt = paste(dirname(output), "directory does not exist. Do you want to create it? (y/n): "))
        if (tolower(response) == "y") {
            dir.create(dirname(output), recursive = TRUE)
        } else {
            stop("Operation cancelled by the user.")
        }
    }
    # Ask if it's ok to replace current CITATION
    if (file.exists(output)) {
        response <- readline(prompt = paste(output, "already exists. Do you want to replace it? (y/n): "))
        if (tolower(response) != "y") {
            stop("Operation cancelled by the user.")
        }
    }

    # Fetch citation information, abort if not found
    citation_info <- tryCatch({
        rcrossref::cr_cn(dois = doi, format = "citeproc-json")
    }, warning = function(w) {
        stop("DOI not found: ", conditionMessage(w))
    })

    # If only one DOI, convert to list
    if (length(citation_info) != length(doi)) {
        citation_info <- list(citation_info)
    }
    citation_content <- character(length(citation_info)) |> as.list()

    # Format citation information
    for (i in seq_along(doi)) {
        type <- citation_info[[i]]$type
        cit_info <- citation_info[[i]]
        title <- cit_info$title
        authors <- apply(cit_info$author, 1, function(aut) {
            paste(
                "as.person(\"", 
                paste(aut$given, aut$family), 
                "\")", 
                sep = ""
            )
        }) |> paste(collapse = ",\n        ")
        journal <- ifelse(type == 'posted-content', cit_info$institution[[1]], cit_info$`container-title`)
        year <- cit_info$issued$`date-parts`[[1]][1]
        volume <- ifelse(is.na(cit_info$volume) || is.null(cit_info$volume), "", cit_info$volume)
        issue <- ifelse(is.na(cit_info$issue) || is.null(cit_info$issue), "", cit_info$issue[])
        issue <- ifelse(type == 'posted-content', "", issue)
        pages <- ifelse(is.na(cit_info$page) || is.null(cit_info$page), "", cit_info$page)
        
        citation_content[[i]] <- stringr::str_glue(
            "bibentry(\n",
            "  bibtype = \"Article\",\n",
            "  author = c(\n",
            "    {authors}\n",
            "  ),\n",
            "  title = \"{title}\",\n",
            "  journal = \"{journal}\",\n",
            "  year = \"{year}\",\n",
            "  volume = \"{volume}\",\n",
            "  issue = \"{issue}\",\n",
            "  pages = \"{pages}\",\n",
            "  doi =  \"{doi[[i]]}\"\n",
            ")\n"
        )
    }

    # If multiple DOIs: paste them together
    if (length(citation_content) > 1) {
        citation_content <- c(
            "c(",
                gsub("\n", "\n  ", citation_content) |> 
                    stringr::str_replace_all("^bibentry", "  bibentry") |> 
                    paste(collapse = ",\n"),
            ")"
        )
    }
    else {
        citation_content <- citation_content[[1]]
    }

    # Write to file
    writeLines(citation_content, con = output)
    message(output, " file created successfully.")
}
