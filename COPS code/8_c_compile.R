# Render R Markdown to Microsoft Word document
rmarkdown::render(params$path_rmd,
                  output_file = params$path_tmp_word,
                  envir = new.env())

# Replace embargo date in header with publication date
tmp_word <- officer::read_docx(params$path_tmp_word) %>%
  officer::headers_replace_all_text(
    old_value = "dd",
    new_value = as.character(publication_date, format = "%d")) %>%
  officer::headers_replace_all_text(
    old_value = "mm",
    new_value = as.character(publication_date, format = "%m")) %>%
  officer::headers_replace_all_text(
    old_value = "yyyy",
    new_value = as.character(publication_date, format = "%Y"))

# Calculate dimensions for charts
chart_ratio_width <- 16
chart_ratio_height <- 9
tmp_word_docx_dim <- tmp_word %>% officer::docx_dim()
tmp_word_docx_dim_page <- tmp_word_docx_dim$page
tmp_word_docx_dim_page_width <- tmp_word_docx_dim_page["width"] %>% as.numeric(.)
tmp_word_docx_dim_margins <- tmp_word_docx_dim$margins
tmp_word_docx_dim_margins_left <- tmp_word_docx_dim_margins["left"] %>% as.numeric(.)
tmp_word_docx_dim_margins_right <- tmp_word_docx_dim_margins["right"] %>% as.numeric(.)
chart_width_in <- tmp_word_docx_dim_page_width - tmp_word_docx_dim_margins_left - tmp_word_docx_dim_margins_right
chart_height_in <- (chart_ratio_height / chart_ratio_width) * chart_width_in

# Insert chart presenting number of COVID-19 infections in pregnancy, Scotland into the Microsoft Word document and save out
chart <- readRDS(params$path_chart)

tmp_word %>%
  officer::cursor_reach(keyword = "Figure x: Weekly number of confirmed cases of COVID-19 in pregnancy") %>%
  mschart::body_add_chart(chart = chart,
                          style = "Figure_Style",
                          pos = "after",
                          width = chart_width_in,
                          height = chart_height_in) %>%
  print(params$path_word)

# Remove Temporary Files
unlink(params$path_tmp_word)

