# Convenient custom function to load all sheets in an excel file into a list in R

read_excel_allsheets <- function(filename, tibble = FALSE) {
            # I prefer straight data.frames
            # but if you like tidyverse tibbles (the default with read_excel)
            # then just pass tibble = TRUE
            sheets <- readxl::excel_sheets(filename)
            x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
            if(!tibble) x <- lapply(x, as.data.frame)
            names(x) <- sheets
            x
}