#' Create or update CITATION file using a DOI.
#'
#' This function creates a CITATION file from a DOI. 
#' The CITATION file is created in the inst/ directory of the package.
#' 
#' @param doi A character string representing the DOI of the article.
#' @param output_file A character string representing the path to the output file.
#'   Default is "inst/CITATION".
#' 
#' @return A CITATION file is created in the inst/ directory of the package.
#' 
#' @examples
#' \dontrun{
#' updateCitation("10.1038/s41586-020-2818-3")
#' }
#' 
#' @importFrom rcrossref cr_cn
#' @importFrom stringr str_glue
#' 
#' @export

updateCitation <- function(doi, output_file = "inst/CITATION") {
    
    # Check that output folder exists
    if (!dir.exists(dirname(output_file))) {
        response <- readline(prompt = paste(dirname(output_file), "directory does not exist. Do you want to create it? (y/n): "))
        if (tolower(response) == "y") {
            dir.create(dirname(output_file), recursive = TRUE)
        } else {
            stop("Operation cancelled by the user.")
        }
    }
    # Ask if it's ok to replace current CITATION
    if (file.exists(output_file)) {
        response <- readline(prompt = paste(output_file, "already exists. Do you want to replace it? (y/n): "))
        if (tolower(response) != "y") {
            stop("Operation cancelled by the user.")
        }
    }

    # Fetch citation information
    citation_info <- rcrossref::cr_cn(dois = doi, format = "citeproc-json")

    # Check success
    if (is.null(citation_info)) {
        stop("DOI not found.")
    }

    # Format citation information
    title <- citation_info$title
    authors <- apply(citation_info$author, 1, function(aut) {
        paste(
            "as.person(\"", 
            paste(aut$given, aut$family), 
            "\")", 
            sep = ""
        )
    }) |> paste(collapse = ",\n        ")
    journal <- citation_info$`container-title`
    year <- citation_info$issued$`date-parts`[[1]][1]
    volume <- citation_info$volume
    issue <- citation_info$issue
    pages <- citation_info$page
    
    citation_content <- stringr::str_glue(
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
        "  doi =  \"{doi}\"\n",
        ")\n"
    )
    
    # Write to file
    writeLines(citation_content, con = output_file)
    
    message("CITATION file created successfully.")
}
