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

# set offset position in image
set_offset <- function(justification_im, offset_val){
  if (justification_im){
    pos = unit(1, 'npc')-unit(offset_val, "in")
  }else{
    pos = unit(offset_val, "in")
  }
  return(pos)
}

# read  image and draw scalebar
read_im_with_scale <-
  function(image_path,
           scalebar_length_pixels,
           scalebar_height = .05,
           scalebar_color = 'black',
           x_offset = 0.2,
           y_offset = 0.2,
           right_just_im = TRUE,
           top_just_im = FALSE){
    # read image, get width and height
    im <- image_read(image_path)
    im_props <- image_info(im)
    # specify x and y position
    x_pos <- set_offset(right_just_im, x_offset)
    y_pos <- set_offset(top_just_im, y_offset)
    # draw scalebar
    scalebar <- rectGrob(
      x = x_pos,
      y = y_pos,
      width = scalebar_length_pixels/im_props$width,
      height = unit(scalebar_height, "in"),
      hjust = 1, vjust = 0,
      gp = gpar(fill = scalebar_color,
                col = scalebar_color)
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
# arrow_prop is the ratio of the arrow length taken up by the arrowhead
  # (needs to be ~1.25 to not see arrow stick)
draw_arrow <- function(x_point, y_point, length, angle, arrow_prop){
  angle_in_rad = angle/180*pi
  x_end <- x_point + cos(angle_in_rad)*length
  y_end <- y_point + sin(angle_in_rad)*length
  
  arrow_object <-
    grid.lines(x = unit(c(x_end, x_point), "npc"),
               y = unit(c(y_end, y_point), "npc"),
               arrow = arrow(angle = 15, length = unit(arrow_prop*length, "npc"), type = 'closed'),
               gp = gpar(fill = 'black', lwd = 2))
  arrow_grob <- draw_grob(arrow_object)
  return(arrow_grob)
}