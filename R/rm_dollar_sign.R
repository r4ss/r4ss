#' Replace dollar sign operator in a file
#' 
#' The dollar sign is convienent to write, but allows for partial matching,
#' which we don't often want. This function takes a file and changes all dollar
#' signs to double brackets instead.
#' @param file A file containing R source code
#' @param out_file The name or path of a new file to write to. This is by
#' default the same as the original file.
#' @author Kathryn Doering

rm_dollar_sign <- function(file = "inst/extdata/test_rm_dollar_sign.txt", out_file = "new_test_file.R") {
  #read in the file
  lines <- readLines(file)
  # find starting position and length of all matches. Do this separately for
  # synactic and nonsynactic names
  matches_syn <- gregexpr(
      pattern = "\\S\\$[[:alnum:]]+([[:alnum:]]|\\.|\\_)*(\\s|[[:punct:]]|$)",
      text = lines)
  matches_nonsyn <- gregexpr("\\S\\$`[[:print:]]+`", lines)
  # merge the match lists, keeping the match.length attribute
  merge_matches <- function(x, y) {
    new_attr <- c(attr(x, "match.length"), attr(y, "match.length"))
    new <- c(x, y)
    attr(new, "match.length") <- new_attr 
    new
  }
  matches_all <-  mapply(merge_matches,
                         x = matches_syn, 
                         y = matches_nonsyn,
                         SIMPLIFY = FALSE)
  for(i in seq_along(lines)) {
    # skip the line if there are no matches
    if(all(matches_all[[i]] == -1)) {
      next
    }
    # get rid of -1's
    tmp_matches <- matches_all[[i]]
    to_rm <- which(matches_all == -1)
    tmp_matches <- matches_all[-to_rm]
    attr(tmp_matches, "match.length") <-
      attr(tmp_matches, "match.length")[-to_rm]
    
    # Break up the lines, this part still needs work. One way to do this:
    # 1. break the line up. have a key iding if each section needs to be 
    # changed or not.
    # 2. For each section to change, replace $name with [["name"]]
    # 3. piece the line back together
    # 4. replace the line in the original section
    
    # Some code that may be usable, but also may not be correct. based on an 
    # approach of breaking upt the lines, but possibly including some chars not
    # to modify in each of the substrings.
    # line_broken <- substring(lines[i], 
    #                 first = tmp_matches,
    #                 last = c((tmp_matches[-1]), nchar(lines[i])))
    # new_line_parts <- lapply(to_change, function(l) {
    #   # find start and length in each section
    #   matches_syn <- gregexpr("\\S\\$[[:alnum:]]+([[:alnum:]]|\\.|\\_)*(\\s|$)",
    #   l)
    #   matches_nonsyn <- gregexpr("\\S\\$`[[:print:]]+`", l)
    #   matches_all <-  mapply(merge_matches,
    #                          x = matches_syn,
    #                          y = matches_nonsyn,
    #                          SIMPLIFY = FALSE)
    #   # check working properly
    #   if(length(matches_all[[1]] != 1 & length(matches_all[[2]] != 1))) {
    #     stop("function not working properly. rewrite code.")
    #   }
    #   # replace the parts necessary
    #   # add [[]]
    #   l <- sub("\\$", "\\[\\[\\'", l)
    #   l <- sub("\\s|$", "'\\]\\]", l)
    #   l
    # })
    # # put together line
    # new_line <- paste0(c(line_broken[1], unlist(new_line_parts)))
    # lines[i] <- new_line
  }
  # read out the file to out_file
  lines
}