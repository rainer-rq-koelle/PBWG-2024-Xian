# default libraries
library(tidyverse)

# load all utility functions from R sub-folder
purrr::walk(
     .x = list.files(path = "./R", full.names = TRUE)
    ,.f = ~ source(.x)
    )

# default settings
ggplot2::theme_set(ggplot2::theme_minimal())

# default colors
## tbd - check for other work
