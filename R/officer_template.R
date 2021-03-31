library(flextable)
library(officer)

proj_path <- ""
setwd(proj_path)
getwd()

inmall <- "Indata/wordmall_enkel.docx"
utmall <- "Output/SAP/Rapport_deskriptiv_statistik.docx"

# RAPPORT: IMPORT --------------------------------------------------------------
mydoc <- read_docx(inmall)

# RAPPORT: TITLE PAGE ----------------------------------------------------------
mydoc <- body_replace_all_text(mydoc, "TITLE", "My title", only_at_cursor = F)
mydoc <- body_replace_all_text(mydoc, "DATE", as.character(Sys.Date()), only_at_cursor = F)

# RAPPORT: SECTION 1 -----------------------------------------------------------
mydoc <- body_add_par(mydoc, "Descriptive statistics", style = "heading 1", pos = "before")


# RAPPORT: EXPORT --------------------------------------------------------------
print(mydoc, utmall)
shell.exec(utmall)