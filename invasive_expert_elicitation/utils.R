
# Create modal pop-up for entering name
popupModal <- function(failed = FALSE) {
  modalDialog(
    textInput("name", "Please enter your full name"),
    if (failed)
      div(tags$b("Your name is required", style = "color: red;")),

    footer = tagList(
      actionButton("ok", "OK")
    )
  )
}

#----------------------------------------

# Create modal pop-up for confirmation of submission
submitModal <- function() {
  modalDialog(
    h3("Your response has been submitted")
  )
}

#----------------------------------------

# Create modal to tell users what they are required to do for habitat suitability page
failedSubmitModal_habsuit <- function() {
  modalDialog(
    div(tags$b("You must first update the map by clicking the 'Update Suitability' button.",
               style = "color: red;")),

    footer = tagList(
      actionButton("ok", "OK")
    )
  )
}

#----------------------------------------

# Create modal to tell users what they are required to do for relative prevalence page
failedSubmitModal_occ <- function() {
  modalDialog(
    div(tags$b("You must first update the map by clicking on the raster layer and specifying your previous experience with this species.",
               style = "color: red;")),

    footer = tagList(
      actionButton("ok", "OK")
    )
  )
}

#----------------------------------------

# Function to add species photos and common names to UI

insertSppPhoto <- function(lab, photo_path) {
  column(width = 5,
         offset = 1,
         p(strong(lab[1])),
         style = 'padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:40px',  #to add extra space under image
         img(src = photo_path, width = "90%")
         )
}

#----------------------------------------

# Function to generate sliders for creating habitat suitability map
sliderInput01 <- function(id, lab) {
  sliderInput(inputId = id, label = lab, min = 0, max = 1, value = 0.5, step = 0.05)
}

#----------------------------------------

# Function to reset sliders for habitat suitability tab when changing species
updateSliderInput01 <- function(id) {
  updateSliderInput(inputId = id, value = 0.5)
}

#----------------------------------------

# Function to update the habitat suitability raster after adjusting the sliders
habsuit_update_fun <- function(id, lab, lulc, hab_suit, input1) {

  for (i in 1:length(id)) {

  tmp<- lab[i]
  ind<- which(terra::values(lulc) == id[i])
  terra::values(hab_suit)[ind]<- input1[[tmp]]

  }

  hab_suit
}


#----------------------------------------

# Function that modifies existing leaflet::addLegend by adding an option for decreasing order
addLegend_decreasing <- function (map,
                                  position = c("topright", "bottomright", "bottomleft",
                                                    "topleft"),
                                  pal,
                                  values,
                                  na.label = "NA",
                                  bins = 7,
                                  colors,
                                  opacity = 0.5,
                                  labels = NULL,
                                  labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins

      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")

    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }

    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2,
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}
