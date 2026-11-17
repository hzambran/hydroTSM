suppressPackageStartupMessages(library(hydroTSM))

# Extract every argument name from grouped or individual Rd argument items.
documented_arguments <- function(rd) {
  arguments.tag <- paste0("\\", "arguments")
  item.tag      <- paste0("\\", "item")
  arguments     <- NULL

  # Locate the arguments section without relying on its position in the Rd file.
  for (element in rd) {
    if (identical(attr(element, "Rd_tag"), arguments.tag)) {
      arguments <- element
      break
    } # IF end
  } # FOR end
  if (is.null(arguments)) return(character())

  # Split grouped labels such as "from, to" into individual formal names.
  labels <- character()
  for (element in arguments) {
    if (identical(attr(element, "Rd_tag"), item.tag)) {
      labels <- c(labels, paste(unlist(element[[1L]]), collapse=""))
    } # IF end
  } # FOR end
  labels <- trimws(unlist(strsplit(labels, ",", fixed=TRUE)))
  unique(gsub("[[:space:]]+", "", labels))
} # documented_arguments end

# Guard the public QC signatures against incomplete function-specific help.
qc.functions <- c("precipQC_daily"    , "precipQC_subdaily",
                  "tempQC_daily"      , "tempQC_subdaily",
                  "streamflowQC_daily", "streamflowQC_subdaily")
rd.database  <- tools::Rd_db("hydroTSM")

for (function.name in qc.functions) {
  expected   <- names(formals(getExportedValue("hydroTSM", function.name)))
  rd.name    <- paste0(function.name, ".Rd")
  documented <- documented_arguments(rd.database[[rd.name]])
  missing    <- setdiff(expected, documented)
  extra      <- setdiff(documented, expected)

  # Report the exact mismatch instead of a generic set-comparison failure.
  if (length(missing) || length(extra)) {
    stop(sprintf("%s Rd argument mismatch; missing: %s; extra: %s",
                 function.name,
                 if (length(missing)) paste(missing, collapse=", ") else "none",
                 if (length(extra)) paste(extra, collapse=", ") else "none"),
         call.=FALSE)
  } # IF end
} # FOR end
