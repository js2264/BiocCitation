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
#' 
#' @export

updateCitation <- function(doi, output_file = "inst/CITATION") {
  
    citation_info <- rcrossref::cr_cn(dois = doi, format = "citeproc-json")
    title <- citation_info$title
    authors <- apply(citation_info$author, 1, function(aut) paste(aut$given, aut$family))
    authors <- paste(authors, collapse = ",\n        ")
    journal <- citation_info$`container-title`
    year <- citation_info$issued$`date-parts`[[1]][1]
    volume <- citation_info$volume
    issue <- citation_info$issue
    pages <- citation_info$page
    
    citation_content <- paste(
        "bibentry(\n",
        "  bibtype = \"Article\",\n",
        "  author = c(\n",
        "    ", authors, "\n",
        "  ),\n",
        "  title = \"", title, "\",\n",
        "  journal = \"", journal, "\",\n",
        "  year = \"", year, "\",\n",
        "  volume = \"", volume, "\",\n",
        "  issue = \"", issue, "\",\n",
        "  pages = \"", pages, "\",\n",
        "  doi =  \"", doi, "\"\n",
        ")\n",
        sep = ""
    )
  
    writeLines(citation_content, con = output_file)
  
    message("CITATION file created successfully.")
}
