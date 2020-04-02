#' Picture Color Checker
#'
#' @description This function recalibrates colors of a list of pictures based on
#'   a color chart (also presents on the pictures) containing 'true' colors.
#'
#' @param images A character vector (>= 1) with the filename of pictures to
#'   calibrate.
#' @param color_chart A data frame containing 'true' color of the color chart.
#' @param output_path The folder to stored calibrated pictures.
#' @param cell_size A numeric between 0 and 1 representing the quantity of color
#'   to pick in each color pattern of the color chart.
#'
#' @return Nothing
#'
#' @import sp
#' @import cli
#' @import raster
#' @import imager
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}


color_checker <- function(images, color_chart, output_path = ".", cell_size = 0.2) {

  if (missing(images)) {
    stop("")
  }

  if (missing(color_chart)) {
    stop("")
  }

  if (cell_size > 1 || cell_size < 0) {
    stop("Argument cell_size must be 0 > x > 1.")
  }

  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

  for (col_name in c("trueR", "trueG", "trueB")) {
    if (max(color_chart[ , col_name]) <= 1) {
      color_chart[ , col_name] <- color_chart[ , col_name] * 255
    }
  }

  par(mfrow = c(1, 1))
  par(xaxs = "i", yaxs = "i", mar = rep(1, 4), family = "serif", bg = "black")


  for (z in 1:length(images)) {

    filename <- strsplit(images[z], "/")[[1]]
    filename <- filename[length(filename)]

    cli::cat_line()
    cli::cat_rule(
      left     = paste0(filename, " (", round(100 * z / length(images), 1), "%)"),
      line_col = "darkgrey"
    )
    cli::cat_line()

    usethis::ui_info("Importing image...")
    img <- imager::load.image(images[z])

    usethis::ui_info("Plotting image...")
    plot(img, axes = FALSE, ann = FALSE, bty = "n")

    usethis::ui_todo("Please click on the four corners...")
    xy <- locator(4)

    usethis::ui_info("Extracting colors...")
    xyDF <- data.frame(x = xy$x, y = xy$y)

    xleft   <- min(xyDF[ , "x"])
    xright  <- max(xyDF[ , "x"])
    ybottom <- max(xyDF[ , "y"])
    ytop    <- min(xyDF[ , "y"])

    nCols <- 4
    cols  <- numeric(nCols)
    for (i in 1:nCols) {
      cols[i] <- xleft + (i * (xright - xleft) / nCols) - ((xright - xleft) / nCols) / 2
    }

    nRows <- 6
    rows  <- numeric(nRows)
    for (i in 1:nRows) {
      rows[i] <- ybottom + (i * (ytop - ybottom) / nRows) - ((ytop - ybottom) / nRows) / 2
    }

    patterns <- expand.grid(y = rows, x = cols)[ , 2:1]

    rect(xleft, ybottom, xright, ytop, border = "green")
    text(patterns[ , 1:2], as.character(1:24), cex = .5)

    patterns <- data.frame(
      id   = 1:nrow(patterns),
      xmin = patterns[ , "x"] - (((xright - xleft) / nCols) / 2) * cell_size,
      xmax = patterns[ , "x"] + (((xright - xleft) / nCols) / 2) * cell_size,
      ymin = patterns[ , "y"] - (((ytop - ybottom) / nRows) / 2) * cell_size,
      ymax = patterns[ , "y"] + (((ytop - ybottom) / nRows) / 2) * cell_size,
      xctr = patterns[ , "x"],
      yctr = patterns[ , "y"]
    )

    for (i in 1:nrow(patterns)){
      rect(
        xleft   = patterns[i, "xmin"],
        ybottom = patterns[i, "ymin"],
        xright  = patterns[i, "xmax"],
        ytop    = patterns[i, "ymax"],
        border  = "red"
      )
    }

    matrixR <- as.matrix(imager::R(img)) * 255
    matrixG <- as.matrix(imager::G(img)) * 255
    matrixB <- as.matrix(imager::B(img)) * 255


    rasRGB <- raster::stack(
      raster::raster(matrixR),
      raster::raster(matrixG),
      raster::raster(matrixB)
    )

    raster::extent(rasRGB) <- c(0, dim(img)[2], 0, dim(img)[1])

    rasRGB         <- raster::flip(raster::t(rasRGB), direction = "y")
    names(rasRGB)  <- c("R", "G", "B")

    patCoords <- sp::SpatialPolygons(
      apply(patterns, 1, function(x){
        sp::Polygons(
          list(
            sp::Polygon(
              coords = data.frame(
                x = c(x[2], x[2], x[3], x[3], x[2]),
                y = c(x[4], x[5], x[5], x[4], x[4])
              ),
              hole = FALSE
            )
          ),
          ID = as.character(x[1])
        )
      })
    )

    patColors <- data.frame(
      id = patterns[ , "id"],
      raster::extract(rasRGB, patCoords, fun = mean)
    )

    usethis::ui_info("Recalibrating colors...")
    dat <- merge(patColors, color_chart, by = "id")

    modelR <- stats::lm(trueR ~ R + G + B, data = dat)
    modelG <- stats::lm(trueG ~ R + G + B, data = dat)
    modelB <- stats::lm(trueB ~ R + G + B, data = dat)

    ypred <- data.frame(
      R = matrix(matrixR, ncol = 1),
      G = matrix(matrixG, ncol = 1),
      B = matrix(matrixB, ncol = 1)
    )

    predR <- stats::predict(modelR, ypred)
    predG <- stats::predict(modelG, ypred)
    predB <- stats::predict(modelB, ypred)

    imCal <- array(dim = dim(img))

    imCal[ , , , 1] <- matrix(predR, nrow = dim(img)[1])
    imCal[ , , , 2] <- matrix(predG, nrow = dim(img)[1])
    imCal[ , , , 3] <- matrix(predB, nrow = dim(img)[1])

    imCal <- imager::as.cimg(imCal)

    plot(0, axes = FALSE, ann = FALSE, bty = "n")

    usethis::ui_done("Recalibrated image exported.")
    imager::save.image(
      im      = imCal,
      file    = file.path(
        output_path,
        paste0(
          gsub("\\.jpg|_original", "", filename, ignore.case = TRUE),
          "_calibrated.jpg"
        )
      ),
      quality = 0.7
    )
  }
  dev.off()
}
