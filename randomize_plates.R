randomize_plates <- function(output_directory, input_plateID_path,
                             plate_name_prefix, color_number = 8,
                             label_column = 'Comments',
                             plate_output_number = 26){
  #' Randomize plates from plateID csv file
  #' 
  #' @param output_directory The directory in which output folders containing
  #'   randomized plateID files, plateplots (images to be printed as visual aids
  #'   for plate randomization) and plateformats (csv files containing
  #'   randomized wells organized as plates).
  #' @param input_plateID_path The path to the location of the source plateID
  #'   file.
  #' @param plate_name_prefix The prefix for randomized files created as
  #'   outputs; e.g. the name of the experiment for which plateIDs are being
  #'   randomized.
  #' @param color_number The number of unique colors to be used in the
  #'   randomization visual aids. Exceeding the default of 8 is not recommended.
  #' @param label_column The column corresponding to the labels that will be
  #'   assigned to the wells in the plateplot and plateformats files. It is
  #'   recommended that this is a column that contains a very short (<= 4
  #'   characters) label, e.g. the 'Comments' column holding the original well
  #'   position used by every sample on the source plate.
  #' @param plate_output_number The number of randomized plates created.

  library(colorspace)
  library(dplyr)
  library(cowplot)
  
  plate_plot_dir <- file.path(output_directory, 'plate_plots')
  plateID_path <- file.path(output_directory, 'plateIDs')
  plateformat_path <- file.path(output_directory, 'plate_formats')
  
  # Only run this function if output_firectory doesn't already contain
  # randomization folders, to avoid accidentally overwriting existing
  # randomization
  if (dir.exists(plate_plot_dir) | dir.exists(plateID_path) | dir.exists(plateformat_path)){
    stop('output_directory already contains plate_plots, plateIDs, or plate_formats folder, and these cannot be overwritten.')
  }else{
    dir.create(plate_plot_dir)
    dir.create(plateID_path)
    dir.create(plateformat_path)
  }
  
  plate_name_vector<-paste0(plate_name_prefix,c(LETTERS, paste0(rep(LETTERS, each = 26), rep(LETTERS, times = 26))))
  if (plate_output_number > length(plate_name_vector)){
    stop('Number of required plates > 702, which is frankly a bit ridiculous')
  }
  
  input_plateID <- read.csv(input_plateID_path,header=T)

  # get rows and columns
  label_rows_and_columns <- function(df){
    df$row <- gsub('\\d*', '', df$Well)
    df$column <- as.numeric(gsub('[A-z]', '', df$Well))
    return(df)
  }
  input_plateID <- label_rows_and_columns(input_plateID)
  plate_rows<-length(unique(input_plateID$row))
  plate_columns<-length(unique(input_plateID$column))
  
  # sort input by well
  input_plateID <- input_plateID[order(input_plateID$row, input_plateID$column), ]
  
  row_number <- nrow(input_plateID)
  
  ###
  # organize unique labels
  unique_labels <- unique(input_plateID[, label_column])
  unique_label_number <- length(unique_labels)
  sheet_number <- ceiling(unique_label_number/color_number)
  colors_to_use <- c('#BBBBBB', '#666666', hex(HSV(seq(0, 360, length = (color_number-1))[-(color_number-1)], 1, 1)))
  
  label_df <- data.frame(
    sheet = rep(1:sheet_number, each = color_number)[1:unique_label_number],
    color_to_use = rep(colors_to_use, times = sheet_number)[1:unique_label_number]
    )
  label_df[,label_column] <- unique_labels
  input_plateID <- join(input_plateID, label_df)
  ###
  
  create_plate_plot <- function(df, current_sheet, label_col_name, plotname){
    df$color_to_use[which(df$sheet != current_sheet)] <- NA
    plot_output <- ggplot(df, aes(x = as.factor(column), y = row)) +
      geom_tile(aes(fill = color_to_use), color = 'black') +
      geom_text(aes(label = get(label_col_name))) +
      scale_fill_identity() +
      scale_y_discrete(limits = rev(levels(as.factor(df$row))))+
      theme_void()
    labeled_plot <- ggdraw(add_sub(plot_output, plotname, vjust = 0, y = .2, vpadding = grid::unit(0.25, 'inches')))
    return(labeled_plot)
  }
  
  create_all_plate_plots <- function(plateID_df, plate_name, plot_dir, label_col_name){
    for(current_sheet in unique(plateID_df$sheet)){
      plotname <- paste0(plate_name,'_sheet_', current_sheet)
      plot_filename <- file.path(plot_dir, paste0(plotname, '_plate_plot.pdf'))
      full_plot <- create_plate_plot(plateID_df, current_sheet, label_col_name, plotname)
      ggsave(plot_filename, plot = full_plot, width = 4.25, height = 3.4)
    }
  }
  
  create_all_plate_plots(input_plateID, paste0(plate_name_prefix,'source_plate'), plate_plot_dir, label_column)
  
  for(counter in 1:plate_output_number){
    current_plate_name<-plate_name_vector[counter]
    
    output_filename<-file.path(plateID_path,paste0(current_plate_name,'_plateID.csv'))
    plateformat_filename<-file.path(plateformat_path,paste0(current_plate_name,'_plateformat.csv'))
    
    random_vector <- sample(c(1:row_number),size=row_number,replace=F)
    
    output_plateID <- input_plateID[random_vector,]
    output_plateID$Well <- input_plateID$Well
    output_plateID <- label_rows_and_columns(output_plateID)
    
    plateformat_matrix <- t(matrix(output_plateID$Comments,nrow=plate_columns,ncol=plate_rows))
    
    create_all_plate_plots(output_plateID, current_plate_name, plate_plot_dir, label_column)
    
    write.csv(output_plateID[, !(names(output_plateID) %in% c('row', 'column', 'sheet', 'color_to_use'))],file=output_filename,row.names=F)
    write.csv(plateformat_matrix,file=plateformat_filename,row.names=F)
    
  }
}