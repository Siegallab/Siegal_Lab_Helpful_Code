# Functions to load images with scalebars and add arrows

library(grid)
library(magick)
library(cowplot)

# Set scalebar size based on scope zoom and digital image mag
set_scalebar_size <- function(microns, digital_image_mag = 1, scope_zoom=15){
  scope_im_scalebar <- microns * scope_zoom * 1/6.467195
  digital_im_scalebar <- scope_im_scalebar * digital_image_mag
  return(digital_im_scalebar)
}

# read  image and draw scalebar
read_im_with_scale <-
  function(image_path, scalebar_length_pixels, scalebar_height = 5, scalebar_color = 'black'){
    # read image, get width and height
    im <- image_read(image_path)
    im_props <- image_info(im)
    # draw scalebar
    scalebar <- rectGrob(
      x = unit(1, 'npc')-unit(0.2, "in"),
      y = unit(0.2, "in"),
      width = scalebar_length_pixels/im_props$width,
      height = unit(0.05, "in"),
      hjust = 1, vjust = 0,
      gp = gpar(fill = scalebar_color)
    )
    drawn_obj <-
      ggdraw() +
      draw_image(im) +
      draw_grob(scalebar)
    return(drawn_obj)
  }

# return grob object of arrow
# x_point and y_point are positions on the image, in pixels
# angle is arrow angle in degrees (with point at origin)
# arrow_prop is the ratio of length to width?
draw_arrow <- function(x_point, y_point, length, angle, arrow_prop){
  angle_in_rad = angle/180*pi
  x_end <- x_point + cos(angle_in_rad)*length
  y_end <- y_point + sin(angle_in_rad)*length
  
  arrow_object <-
    grid.lines(x = unit(c(x_end, x_point), "npc"),
               y = unit(c(y_end, y_point), "npc"),
               arrow = arrow(angle = 15, length = unit(arrow_prop*length, "npc"), type = 'closed'),
               gp = gpar(fill = 'black', lwd = 2))
  return(draw_grob(arrow_object))
}