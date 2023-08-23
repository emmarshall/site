## Load packages
library(dplyr)
library(htmltools)
library(data.table)
library(reactable)
library(purrr)
library(tibble)
library(yaml)
library(tidyverse)
library(gt)
library(magick)
library(glue)

## Alternative option to add these to .Rprofile w/---
## if (file.exists('~/.Rprofile')) {
## sys.source('~/.Rprofile', envir = environment())
## }


# Function to make image carousel
# carousel displays a list of items w/ nav buttons
carousel <- function(id, duration, items) {
  index <- -1
  items <- lapply(items, function(item) {
    index <<- index + 1
    carouselItem(item$caption, item$image, item$link, index, duration)
  })
  
  indicators <- div(class = "carousel-indicators",
                    tagList(lapply(items, function(item) item$button))
  )
  items <- div(class = "carousel-inner",
               tagList(lapply(items, function(item) item$item))           
  )
  div(id = id, class="carousel carousel-dark slide", `data-bs-ride`="carousel",
      indicators,
      items,
      navButton(id, "prev", "Prevoius"),
      navButton(id, "next", "Next")
  )
}

# carousel item
carouselItem <- function(caption, image, link, index, interval) {
  id <- paste0("gallery-carousel-item-", index)
  button <- tags$button(type = "button", 
                        `data-bs-target` = "#gallery-carousel",
                        `data-bs-slide-to` = index,
                        `aria-label` = paste("Slide", index + 1)
  )
  if (index == 0) {
    button <- tagAppendAttributes(button,
                                  class = "active",
                                  `aria-current` = "true"                  
    )
  }
  item <- div(class = paste0("carousel-item", ifelse(index == 0, " active", "")),
              `data-bs-interval` = interval,
              a(href = link, img(src = image, class = "d-block  mx-auto border")),
              div(class = "carousel-caption d-none d-md-block",
                  tags$p(class = "fw-light", caption)
              )
  )
  list(
    button = button,
    item = item
  )                        
}

# Function to make nav button
navButton <- function(targetId, type, text) {
  tags$button(class = paste0("carousel-control-", type),
              type = "button",
              `data-bs-target` = paste0("#", targetId),
              `data-bs-slide` = type,
              span(class = paste0("carousel-control-", type, "-icon"),
                   `aria-hidden` = "true"),
              span(class = "visually-hidden", text)
  )
}

## Function to make inline lists of links
create_inline_object <- function(icon, text, url) {
  sprintf(
    '<span class="inline-object"><i class="fa fa-%s"></i><a href="%s">%s</a></span>',
    icon, url, text
  )
}

## Function to make info bars with icons
generate_info_bar <- function(info) {
  # Define CSS class names
  info_bar_class <- "info-bar"
  info_bar_wrapper_class <- "info-bar-wrapper"
  inner_container_class <- "inner-container"
  info_bar_icon_class <- "info-bar__icon"
  info_bar_text_class <- "info-bar__text"
  info_bar_title_class <- "info-bar__title"
  info_bar_center_class <- "info-bar__center"
  
  # Generate HTML code
  html <- paste0(
    "<div class='", info_bar_class, "'>",
    "<div class='", info_bar_wrapper_class, "'>",
    "<div class='", info_bar_icon_class, "'>",
    "<i class='", info$icon, "'></i>",
    "</div>",
    "<div class='", info_bar_text_class, "'>",
    "<h3 class='", info_bar_title_class, "'>", info$title, "</h3>",
    "<p class='lead'>", info$text, "</p>",
    "</div>",
    "<div class='", info_bar_center_class, "'>",
    "<a href='", info$link, "' class='btn btn-warning'>", info$ctr, "</a>",
    "</div>",
    "</div>",
    "</div>"
  )
  
  return(html)
}


## Function to make accordion objects
make_accordion <- function(titles, contents) {
  # Create a list of accordion items
  items <- lapply(seq_along(titles), function(i) {
    item_header <- tags$h2(class = "accordion-header", 
                           tags$button(class = "accordion-button", 
                                       type = "button", 
                                       `data-bs-toggle` = "collapse", 
                                       `data-bs-target` = paste0("#collapse", i), 
                                       `aria-expanded` = ifelse(i == 1, "true", "false"), 
                                       `aria-controls` = paste0("collapse", i), 
                                       titles[i]))
    item_body <- tags$div(class = ifelse(i == 1, "accordion-collapse collapse show", "accordion-collapse collapse"), 
                          id = paste0("collapse", i), 
                          `aria-labelledby` = paste0("heading", i), 
                          `data-bs-parent` = "#accordionExample", 
                          tags$div(class = "accordion-body", contents[i]))
    tags$div(class = "accordion-item", item_header, item_body)
  })
  
  # Combine the list of items into a single tag object
  accordion <- tags$div(class = "accordion", id = "accordionExample", items)
  
  # Return the accordion tag object
  return(accordion)
}



#Function to make rowwise_tables for 'data.table' credit to mlr3misc pkg 

#' Similar to the \CRANpkg{tibble} function `tribble()`, this function
#' allows to construct tabular data in a row-wise fashion.
#'
#' The first arguments passed as formula will be interpreted as column names.
#' The remaining arguments will be put into the resulting table.
#'
#' @param ... (`any`)\cr
#'   Arguments: Column names in first rows as formulas (with empty left hand side),
#'   then the tabular data in the following rows.
#' @param .key (`character(1)`)\cr
#'   If not `NULL`, set the key via [data.table::setkeyv()] after constructing the
#'   table.
#'
#' @return [data.table::data.table()].
#' @export
#' @examples
#' rowwise_table(
#'   ~a, ~b,
#'   1, "a",
#'   2, "b"
#' )
rowwise_table = function(..., .key = NULL) {
  
  dots = list(...)
  
  for (i in seq_along(dots)) {
    if (!inherits(dots[[i]], "formula")) {
      ncol = i - 1L
      break
    }
  }
  
  if (ncol == 0L) {
    stop("No column names provided")
  }
  
  n = length(dots) - ncol
  if (n %% ncol != 0L) {
    stop("Data is not rectangular")
  }
  
  tab = lapply(seq_len(ncol), function(i) simplify2array(dots[seq(from = ncol + i, to = length(dots), by = ncol)]))
  tab = setnames(setDT(tab), map_chr(head(dots, ncol), function(x) attr(terms(x), "term.labels")))
  if (!is.null(.key)) {
    setkeyv(tab, .key)
  }
  tab
}

# Function to create rowwise_table based on data from csv or excel file 
library(data.table)
library(readxl)

rowwise_table_from_file = function(..., .key = NULL, file_path = NULL, file_type = NULL) {
  
  if (!is.null(file_path) && !is.null(file_type)) {
    
    # Read data from the CSV or Excel file
    if (file_type == "csv") {
      raw_data <- fread(file_path, header = FALSE)
    } else if (file_type == "excel") {
      raw_data <- as.data.table(read_excel(file_path, col_names = FALSE))
    } else {
      stop("Invalid file type. Use 'csv' or 'excel'.")
    }
    
    # Extract column names and data
    col_names <- as.formula(paste0("~", paste(raw_data[1,], collapse = ", ~")))
    data <- as.list(raw_data[-1, ])
    
    # Call the original rowwise_table function with the extracted column names and data
    rowwise_table(col_names, data, ..., .key = .key)
  } else {
    rowwise_table(..., .key = .key)
  }
}

# Example usage:

# Manually creating the context-object in R
manual_data_table <- rowwise_table_from_file(
  ~a, ~b,
  1, "a",
  2, "b"
)



# Function to create hexwall from images in images/hex
library(magick)
library(purrr)
library(htmltools)

hexwall <- function(path, sticker_row_size = 5, sticker_width = 500, remove_small = TRUE, total_stickers = NULL, remove_size = TRUE,
                    coords = NULL, scale_coords = TRUE, sort_mode = c("filename", "random", "color", "colour")){
  sort_mode <- match.arg(sort_mode)
  
  # Load stickers
  sticker_files <- list.files(path)
  stickers <- file.path(path, sticker_files) %>% 
    map(function(path){
      switch(tools::file_ext(path),
             svg = image_read_svg(path),
             pdf = image_read_pdf(path),
             image_read(path))
    }) %>%
    map(image_transparent, "white") %>%
    map(image_trim) %>%
    set_names(sticker_files)
  
  # Low resolution stickers
  low_res <- stickers %>%
    map_lgl(~ remove_small && image_info(.x)$width < (sticker_width-1)/2 && image_info(.x)$format != "svg")
  which(low_res)
  
  stickers <- stickers %>%
    map(image_scale, sticker_width)
  
  # Incorrectly sized stickers
  bad_size <- stickers %>%
    map_lgl(~ remove_size && with(image_info(.x), height < (median(height)-2) | height > (median(height) + 2)))
  which(bad_size)
  
  # Remove bad stickers
  sticker_rm <- low_res | bad_size
  stickers <- stickers[!sticker_rm]
  
  if(any(sticker_rm)){
    message(sprintf("Automatically removed %i incompatible stickers: %s",
                    sum(sticker_rm), paste0(names(sticker_rm[sticker_rm]), collapse = ", ")))
  }
  if(is.null(total_stickers)){
    if(!is.null(coords)){
      total_stickers <- NROW(coords)      
    }
    else{
      total_stickers <- length(stickers)
    }
  }
  
  # Coerce sticker sizes
  sticker_height <- stickers %>%
    map(image_info) %>%
    map_dbl("height") %>%
    median
  stickers <- stickers %>%
    map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))
  
  # Repeat stickers sorted by file name
  stickers <- rep_len(stickers, total_stickers)
  
  if(sort_mode == "random"){
    # Randomly arrange stickers
    stickers <- sample(c(stickers, sample(stickers, total_stickers - length(stickers), replace = TRUE))) 
  }
  else if(sort_mode %in% c("color", "colour")){
    # Sort stickers by colour
    sticker_col <- stickers %>% 
      map(image_resize, "1x1!") %>%
      map(image_data) %>%
      map(~paste0("#", paste0(.[,,1], collapse=""))) %>%
      map(colorspace::hex2RGB) %>%
      map(as, "HSV") %>%
      map_dbl(~.@coords[,1]) %>%
      sort(index.return = TRUE) %>%
      .$ix
    
    stickers <- stickers[sticker_col]
  }
  
  if(is.null(coords)){
    # Arrange rows of stickers into images
    sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
    row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
    row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))
    sticker_rows <- map2(row_lens, cumsum(row_lens),
                         ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
      map(~ stickers[.x] %>% invoke(c, .) %>% image_append())
    
    # Add stickers to canvas
    canvas <- image_blank(sticker_row_size*sticker_width, 
                          sticker_height + (sticker_col_size-1)*sticker_height/1.33526, "white")
    reduce2(sticker_rows, seq_along(sticker_rows), 
            ~ image_composite(
              ..1, ..2,
              offset = paste0("+", ((..3-1)%%2)*sticker_width/2, "+", round((..3-1)*sticker_height/1.33526))
            ),
            .init = canvas)
  }
  else{
    sticker_pos <- coords
    if(scale_coords){
      sticker_pos <- sticker_pos %>% 
        as_tibble %>%
        mutate_all(function(x){
          x <- x-min(x)
          dx <- diff(sort(abs(x)))
          x / min(dx[dx!=0])
        }) %>%
        mutate(y = y / min(diff(y)[diff(y)!=0])) %>%
        mutate(x = x*sticker_width/2,
               y = abs(y-max(y))*sticker_height/1.33526)
    }
    
    # Add stickers to canvas
    canvas <- image_blank(max(sticker_pos$x) + sticker_width, 
                          max(sticker_pos$y) + sticker_height, "white")
    reduce2(stickers, sticker_pos%>%split(1:NROW(.)), 
            ~ image_composite(
              ..1, ..2,
              offset = paste0("+", ..3$x, "+", ..3$y)
            ),
            .init = canvas)
  }
}

# Insert location of hex below

hex_wall <- hexwall("images/hex")

# save image to file
image_write(hex_wall, "images/hex_wall.png")
