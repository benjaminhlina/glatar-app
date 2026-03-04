# ----- Compress row_index

compress_row_index <- function(row_index_str, min_run = 10) {
  # Parse the numbers
  nums <- as.integer(trimws(strsplit(row_index_str, ",")[[1]]))
  nums <- sort(unique(nums))

  if (length(nums) == 0) {
    return(row_index_str)
  }

  # Group into consecutive runs
  groups <- list()
  start <- nums[1]
  end <- nums[1]

  for (i in seq_along(nums)[-1]) {
    if (nums[i] == end + 1) {
      end <- nums[i]
    } else {
      groups <- c(groups, list(c(start, end)))
      start <- nums[i]
      end <- nums[i]
    }
  }
  groups <- c(groups, list(c(start, end)))

  # Format each group
  parts <- sapply(groups, function(g) {
    run_length <- g[2] - g[1] + 1
    if (run_length >= min_run) {
      paste0(g[1], " - ", g[2]) # e.g. "9625-9708"
    } else if (run_length == 1) {
      as.character(g[1]) # single number
    } else {
      paste(g[1]:g[2], collapse = ", ") # short run, keep expanded
    }
  })

  paste(parts, collapse = ", ")
}
