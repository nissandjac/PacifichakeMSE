create_pptx <- function(plot, path){
  if(!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_vg(code = print(plot), type = "body") %>% 
    print(target = path)
}