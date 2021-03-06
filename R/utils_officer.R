
# === === === === === === === === === === 
# Some parameters used in the functions
# === === === === === === === === === === 

txt_page_width <- docx_dim(my_doc)$page[[1]] - docx_dim(my_doc)$margins[[3]] - docx_dim(my_doc)$margins[[4]]

inter_col_width <- cm_to_in
txt_col_width   <- (txt_page_width - inter_col_width)/2




# Headings and sections ---------------------------------------------------


add_heading1 <- function(heading_text){
  body_add_par(x = my_doc,
               style = 'heading 1',
               value = heading_text)
}



add_heading2 <- function(heading_text){
  body_add_par(x = my_doc,
               style = 'heading 2',
               value = heading_text)
}

add_heading3 <- function(heading_text){
  body_add_par(x = my_doc,
               style = 'heading 3',
               value = heading_text)
}



add_end_section_continuous <- function(){
  body_end_section_continuous(x = my_doc)
}


# To consider to unify the two function below by using (txt_page_width - 1 * cm_to_in)/2

add_end_section_2columns <- function(widths = rep(txt_col_width, 2)){
  body_end_section_columns(
    x = my_doc,
    widths = widths,
    sep = FALSE,
    space = 1 * cm_to_in)
}



add_end_section_3columns <- function(widths = rep(txt_col_width * 2/3, 3)){
  body_end_section_columns(
    x = my_doc,
    widths = widths,
    sep = FALSE,
    space = c(inter_col_width/2))
}



# Add figures -------------------------------------------------------------

add_figure_counts_trends_country <- function(object_name, 
                                             path,
                                             figure_title, 
                                             width = 12 * cm_to_in,
                                             height = 6.66 * cm_to_in){
  body_add_img(
    x = my_doc,
    style = 'Figure body',
    src = file.path(path, object_name),
    width = width,
    height = height) %>%
    body_add_par(
      style = 'Figure title',
      value = figure_title)
}



add_figure <- function(object_name, 
                       path,
                       figure_title, 
                       width = 9 * cm_to_in,
                       height = 7 * cm_to_in){
  body_add_img(
    x = my_doc,
    style = 'Figure body',
    src = file.path(path, object_name),
    width = width,
    height = height) %>%
    body_add_par(
      style = 'Figure title',
      value = figure_title)
}



# add_figure_map_world_grid <- function(object_name, figure_title, width = 14.71, height = 12.02){
#   body_add_img(
#     x = my_doc, 
#     style = 'Figure body', 
#     src = file.path(path.local.worldwide.graphs, object_name), 
#     width = width * cm_to_in, 
#     height = height * cm_to_in) %>% 
#     body_add_par(
#       style = 'Figure title', 
#       value = figure_title)
# }


# add_figure <- function(object_name, figure_title, folder, width = 6, height = 4){
#   body_add_img(
#     x = my_doc, 
#     style = 'Figure body', 
#     src = file.path(path.local.week, folder, 'graphs', object_name), 
#     width = width, 
#     height = height) %>% 
#     body_add_par(
#       style = 'Figure title', 
#       value = figure_title)
# }



# Add tables --------------------------------------------------------------


add_table <- function(object_name, 
                      path,
                      table_title, 
                      folder, 
                      width = 5, 
                      height = 5 * 1.414){
  body_add_par(
    x = my_doc,
    style = 'Table title',
    value = table_title) %>%
    body_add_img(
      style = 'Table as Figure',
      src = file.path(path, object_name),
      width = width,
      height = height)
}



# Paragraphs and lists ----------------------------------------------------


add_par_normal <- function(par_text = 'More text here...'){
  body_add_par(
    x = my_doc,
    style = 'Normal',
    value = par_text)
}



add_bullet_normal <- function(bullet_text = 'Bullet text here...'){
  body_add_par(
    x = my_doc,
    style = 'Normal bullet',
    value = bullet_text)
}

