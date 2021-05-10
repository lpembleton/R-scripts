library(RefManageR)
library(dplyr)
library(stringr)
library(stringi)
library(anytime)

#' Convert bib to markdown
#'
#' Imports a bibliography in .bib format from a reference manager (e.g. JabRef) and generates individual markdown files for each entry to be used in a blogdown bibliography
#'
#' @param bib_file .bib bibtex bibliography file containing entries to convert to markdown files. The .bib file can be exported from a reference manager such as JabRef
#' @param author_name vector of author names/variations to format as bold in markdown e.g. c("J Bloggs", "J. Bloggs", "Joe Bloggs", "J. M. Bloggs")
#' @return writes to the current working dir markdown files of each entry
#'
bib_to_md <- function(bib_file, author_name=""){
  my_pubs   <- ReadBib(bib_file, check = "warn", .Encoding = "UTF-8") %>%
    as.data.frame()
  my_pubs <- cbind(row.names(my_pubs), my_pubs)
  colnames(my_pubs)[1] <- "ref_id"
  
  my_pubs <- as_tibble(my_pubs)
  
  for(i in 1:nrow(my_pubs)){
    pub_info <- my_pubs %>% slice(i)
    
    out_file <- paste0(gsub("/", "_", pub_info$ref_id), ".md")
    write("+++", out_file)
    write(paste0("title = \"", gsub("\\}", "", gsub("\\{", "", pub_info$title)), "\""), out_file, append=TRUE)
    write(paste0("date = \"", paste0(pub_info$year, "-01-01"), "\""), out_file, append=TRUE)
    auth <- str_replace_all(pub_info$author, " and ", "\", \"")
    
    if((author_name)!=""){
      pos <- str_locate(auth, author_name)
      pos <- pos[!is.na(pos[,1])]
      if(!is_empty(pos)){
        stri_sub(auth, pos[1], pos[1]-1) <- "**"
        stri_sub(auth, pos[2]+3, pos[2]+2) <- "**"
      }
    }
    
    write(paste0("authors = [\"", auth, "\"]"), out_file, append=TRUE)
    write(paste0("doi = \"", gsub("https://doi.org/", "", pub_info$doi), "\""), out_file, append=TRUE)
    
    publication <- pub_info$journal
    if(!is.na(pub_info$volume)){
      publication <- paste0(publication, ", ", pub_info$volume)
    }
    if(!is.na(pub_info$number)){
      publication <- paste0(publication, "(", pub_info$number, ")")
    }
    write(paste0("publication = \"", publication, "\""), out_file, append=TRUE)
    write(paste0("publication_short = \"", publication, "\""), out_file, append=TRUE)
    
    write(paste0("abstract = \"", pub_info$abstract, "\""), out_file, append=TRUE)
    write(paste0("url_source = \"", pub_info$url, "\""), out_file, append=TRUE)
    write("+++", out_file, append=TRUE)
  }
}
