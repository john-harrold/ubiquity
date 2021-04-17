check_code   = function(test_code){
  tcres = tryCatch(
    { 
      invisible(
        capture.output(
          suppressWarnings(
            suppressMessages(source(test_code))
            )
          )
        )
      list(isgood = TRUE)},
    error = function(e) {
      list(isgood=FALSE, error=e)})
  tcres}