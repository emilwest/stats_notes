
pxjob_helper_available_flags_df <- tribble(
  ~flag, ~job, ~txt,
  "-o -o0", "px", "output file format: px",
  "-o1", "px", "output file format: pxk (metadata file)",
  "-o2", "px", "px (sparse data format)",
  "-o3", "csv", "output file format: metadata-csv, verbose output",
  "-o4", "csv", "output file format: metadata-csv, repeated values included",
  "-o5", "csv", "output file format: semicolon separated, file extension is xls (see also !q)",
  "-o6", "csv", "output file format: semicolon separated csv, all variables in rows",
  "-o -o0", "xls", "output file format: xls (xlsx, if the table is too big for xls format)",
  "-o1", "xls", "output file format: xlsx",
  "-s2", NA_character_, "read the sub-directories but write to the output directory only",
  "-b=", NA_character_, "maintain case of filename",
  "-c0", NA_character_, "character coding for the output file: WinANSI",
  "-c1 -c", NA_character_, "character coding for the output file: Unicode (UTF-8)",
  "-c2", NA_character_, "character coding for the output file: ISO-8859",
  "-r0", NA_character_, "replace metadata (given by meta parameter): add only missing and suitable keywords (default)",
  "-r -r1", NA_character_, "replace metadata (given by meta parameter): replace all possible keyword values",
  "-r2", NA_character_, "replace metadata (given by meta parameter): add missing keywords and replace variable names from template file (if the number of variables matches)",
  "-r3", NA_character_, "replace metadata (given by meta parameter): replace all possible keyword values and variable names",
  "-s -s1", NA_character_, "read sub-directories (if there is only one input directory): reflect the input directory structure in output",
  "-s2", NA_character_, "read the sub-directories but write to the output directory only",
  "-u", NA_character_, "set the LAST-UPDATED keyword",
  "-uvvvvkkpp_hh:mm", NA_character_, "set the defined date and time (NB: the undescore)",
  "-ja", NA_character_, "Join: replace all metadata (surpasses -r)",
  "-jb", NA_character_, "Join: merge new values (between) the original ones after joining (if possible)",
  "-jc", NA_character_, "Join: do not use codes for variable matching",
  "-je", NA_character_, "Join: exact text matching (case sensitive, use leading zeroes)",
  "-jf", NA_character_, "Join: bypass fill items",
  "-jl", NA_character_, "Join: do not create multilingual table if possible",
  "-jm", NA_character_, "Join: do not try to match the variable names",
  "-jn", NA_character_, "Join: group the files to be joined without the last underscore separated part (overrided by options -j1..-j4)",
  "-jo", NA_character_, "Join: use only original values (i.e. do not add new ones)",
  "-jr", NA_character_, "Join: do not replace metadata (overrided by option -ja)",
  "-js", NA_character_, "Join: do not sort the variable values (bypasses options -jc and -je)",
  "-jt", NA_character_, "Join: replace value texts",
  "-ju", NA_character_, "Join: join unique SOURCE keywords to a #-separated list",
  "-j1", NA_character_, "Join: group the files to be joined without the last character",
  "-j2", NA_character_, "Join: group the files to be joined without the 2 characters",
  "-j3", NA_character_, "Join: group the files to be joined without the 3 characters",
  "-j4", NA_character_, "Join: group the files to be joined without the 4 characters"
) %>%
  separate_rows(flag, sep = " ")

pxjob_helper_available_flags_df

# Function for setting path to pxjob and what job to do, checks that selection is valid
pxjob_main <- function(pxjob = "C:\\Program Files (x86)\\PX-Edit 2019 ver 4.0\\PxJob.exe",
                       job) {
  # available jobs:
  jobs <-
    c(
      "px", # px file (default)
      "csv", # csv file (either semicolon or tabulator separated text file)
      "exp", # eXplorer file (txt)
      "htm", # html file
      "report", # 	database report (csv)
      "split", # partial table (px, csv or xhtm)
      "sql", # PxSQL macro file (sql)
      "translate", # 	create or read translation files (translate)
      "txt", # text table (either semicolon or tabulator-separated text file)
      "xls", # Excel table
      "xml" # CoSSI/XML file",
    )
  
  
  stopifnot(file.exists(pxjob))
  stopifnot(all(job %in% jobs), length(job) == 1)
  return(str_glue("\"{pxjob}\" job={job}"))
}


# wrapper function for building pxjob command
pxjob_wrapper <- function(in_path,
                          job,
                          out_path = NULL, # optional
                          err_path = NULL, # optional
                          log_path = NULL,
                          meta_path = NULL,
                          flags = NULL,
                          extra_flags = NULL # if you want to add more flags
) {
  stopifnot(!missing(in_path))
  x <- pxjob_main(job = job)
  x <- str_glue("{x} in=\"{in_path}\"")
  
  if (!is.null(out_path)) x <- str_glue("{x} out=\"{out_path}\"")
  if (!is.null(err_path)) x <- str_glue("{x} err=\"{err_path}\"")
  if (!is.null(log_path)) x <- str_glue("{x} log=\"{log_path}\"")
  if (!is.null(meta_path)) x <- str_glue("{x} meta=\"{meta_path}\"")
  if (!is.null(flags)) x <- str_glue("{x} {flags}")
  if (!is.null(extra_flags)) x <- str_glue("{x} {extra_flags}")
  
  return(x)
}


pxjob_helper_flag_translator <- function(x, jobs, printed = F) {
  x <- x %>%
    str_split(" ") %>%
    pluck(1)
  
  tmp <- pxjob_helper_available_flags_df %>%
    # mutate(tmpflag = ifelse(flag == x, flag, "unknown flag")) %>%
    filter(flag %in% x)
  
  if (nrow(tmp) > 1) {
    tmp_jobs <- tmp %>%
      filter(!is.na(job)) %>%
      filter(job %in% jobs)
    tmp_rest <- tmp %>% filter(is.na(job))
    
    res <- bind_rows(tmp_jobs, tmp_rest)
  } else {
    res <- tmp
  }
  
  unknown_flags <- setdiff(x, res$flag)
  if (length(unknown_flags) >= 1) {
    unknown_flags <- enframe(unknown_flags, name = NULL) %>%
      mutate(
        job = NA_character_,
        txt = "unknown flag"
      ) %>%
      dplyr::rename(flag = value)
    res <- bind_rows(res, unknown_flags)
  }
  
  if (printed) {
    res %>%
      str_glue_data("{txt} ({flag} {ifelse(!is.na(job), job, '')})")
  } else {
    res
  }
}




pxjob_extract_metadata <- function(in_path,
                                   extra_flags = NULL,
                                   promptme = T # if True, a popup window shows up
) {
  #stopifnot(dir.exists(in_path))
  paths <- pxjob_helper_create_output_dirs("extract_metadata")
  
  out_path <- paths$out_path
  err_path <- paths$err_path
  logfile_path <- paths$logfile_path
  job <- "csv"
  
  flags <- "-o3 -s2 -b="
  flags <- str_c(flags, extra_flags)
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    out_path = out_path,
    log_path = logfile_path,
    err_path = err_path,
    flags = flags,
    extra_flags = extra_flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Extract metadata?
                                    indata = {in_path}
                                    out_path = {out_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    print("Extracting...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Output: {out_path}"))
  }
}




# update meta directly (without output directory)
# it overwrites the metadata in in_path with new metadata from meta_path
pxjob_update_metadata_directly <- function(in_path,
                                           meta_path,
                                           base_path = str_glue("{getwd()}/out/Output/DMP/PX"),
                                           flags = "-r -b= -s1",
                                           promptme = T # if True, a popup window shows up
) {
  #stopifnot(dir.exists(in_path))
  
  paths <- pxjob_helper_create_output_dirs("update_metadata_directly",
                                           base_path = base_path,
                                           create_err = F,
                                           create_out = F
  )
  logfile_path <- paths$logfile_path
  job <- "px"
  
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    log_path = logfile_path,
    meta_path = meta_path,
    flags = flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Update metadata directly?
                                      Note that the metadata in indata will
                                      be directly replaced with new metadata and overwritten.
                                    indata = {in_path}
                                    meta = {meta_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    t0 <- Sys.time()
    print("Updating metadata directly...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}"))
    print(Sys.time()-t0)
  }
}





# validates metadata
pxjob_validate_metadata <- function(in_path,
                                    extra_flags = NULL,
                                    promptme = T # if True, a popup window asks if you want to validate
) {
  #  stopifnot(dir.exists(in_path))
  paths <- pxjob_helper_create_output_dirs("validate_metadata", create_out = F)
  
  err_path <- paths$err_path
  logfile_path <- paths$logfile_path
  job <- "px"
  
  flags <- "-s -b="
  flags <- str_c(flags, extra_flags)
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    log_path = logfile_path,
    err_path = err_path,
    flags = flags,
    extra_flags = extra_flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Validate metadata?
                                    indata = {in_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    print("Validating...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    
    res2 <- readr::read_tsv(logfile_path, col_names = F, col_types = cols(.default = "c"))
    
    if (any(str_detect(res2$X4, "error|warn"))) {
      warning("There are validation warnings in the px files, check the tibble log above")
      res2 %>%
        filter(str_detect(res2$X4, "error|warn")) %>%
        select(X5) %>%
        print()
      
      system(str_glue("powershell Invoke-Item {logfile_path}"))
    } else {
      print(str_glue("Looks good, no validation warnings.
                     Log: {logfile_path}"))
    }
  }
}


pxjob_xlsx_to_px <- function(in_path,
                             meta_path,
                             base_path = str_glue("{getwd()}/out/Output/DMP/PX"),
                             flags = "-b= -r2",
                             promptme = T
) {
  
  paths <- pxjob_helper_create_output_dirs("xlsx_to_px",
                                           base_path = base_path,
                                           create_err = T,
                                           create_out = T
  )
  logfile_path <- paths$logfile_path
  out_path <- paths$out_path
  err_path <- paths$err_path
  job <- "px"
  
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    out_path = out_path,
    err_path = err_path,
    meta_path = meta_path,
    log_path = logfile_path,
    flags = flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Convert xls(x) to px?
                                    indata = {in_path}
                                    meta = {meta_path}
                                    out = {out_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    t0 <- Sys.time()
    print("Converting xls(x) to px...")
    res <- system(commandp)
    print(res)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Out = {out_path}"))
    print(Sys.time()-t0)
  }
}

# pxjob_xlsx_to_px()
# pxjob_xlsx_to_px("C:/Users/emiwes/Desktop/EDUC12.xls",
#                  meta_path = path_to_meta_local
#                  )
#

#
# pxjob_update_metadata("L:/Norden/NCM_5807_Nordic_Statistics/NCM_5807_Nordic_Statistics_2021/Transfer/Import/2021-12-02 Ny leverans från Åland/bearbetade/chil06_ax.xlsx",
#                       meta_path = path_to_meta_local,
#                       flags = "-b= -r2"
#                       )

# Convert px to xls(x):
pxjob_px_to_xls <- function(in_path,
                            base_path = str_glue("{getwd()}/out/Output/DMP/PX"),
                            flags = "-b= -o1",
                            promptme = T # if True, a popup window shows up
) {
  #stopifnot(dir.exists(in_path))
  
  paths <- pxjob_helper_create_output_dirs("px_to_xlsx",
                                           base_path = base_path,
                                           create_err = T,
                                           create_out = T
  )
  logfile_path <- paths$logfile_path
  out_path <- paths$out_path
  err_path <- paths$err_path
  job <- "xls"
  
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    out_path = out_path,
    err_path = err_path,
    log_path = logfile_path,
    flags = flags
  )
  
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Convert px to xls(x)?
                                    indata = {in_path}
                                    out = {out_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    t0 <- Sys.time()
    print("Converting px to xls(x)...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Out: {out_path}"))
    print(Sys.time()-t0)
  }
}

# pxjob_px_to_xls("C:/Users/emiwes/Desktop/NCM_5807_Nordic_Statistics/out/Output/DMP/0.FINAL/ENER06.px")
#system('"C:\\Program Files (x86)\\PX-Edit 2019 ver 4.0\\PxJob.exe" job=xls in="C:/Users/emiwes/Desktop/NCM_5807_Nordic_Statistics/out/Output/DMP/0.FINAL/ENER06.px" out="C:/Users/emiwes/Desktop/NCM_5807_Nordic_Statistics/out/Output/DMP/PX/px_to_xlsx/2022-02-01/out/" err="C:/Users/emiwes/Desktop/NCM_5807_Nordic_Statistics/out/Output/DMP/PX/px_to_xlsx/2022-02-01/err/" log="C:/Users/emiwes/Desktop/NCM_5807_Nordic_Statistics/out/Output/DMP/PX/px_to_xlsx/2022-02-01/log/pxjob_20220201_154309.log" -b= -o1')

# updates LAST-UPDATED to target_date 09:00
# for example target_date = "2022-02-15"
pxjob_update_last_updated <- function(in_path,
                                      target_date = NULL,
                                      extra_flags = NULL,
                                      promptme = T # if True, a popup window shows up
) {
  # stopifnot(dir.exists(in_path))
  paths <- pxjob_helper_create_output_dirs("update_last_updated",
                                           create_out = F
  )
  
  err_path <- paths$err_path
  logfile_path <- paths$logfile_path
  job <- "px"
  
  if (is.null(target_date)) target_date <- lubridate::date(lubridate::now())
  
  flags <- str_glue("-u{target_date}_09:00 -b=")
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    log_path = logfile_path,
    err_path = err_path,
    flags = flags,
    extra_flags = extra_flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Set LAST-UPDATED to {target_date} 09:00 for all px-files in
                                    indata = {in_path}?"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    print("Updating...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Input: {in_path}"))
  }
}

# pxjob_update_last_updated(path_to_final, target_date = "2022-02-15")


# generates px report
pxjob_generate_px_report <- function(in_path,
                                     meta_path = "L:/Norden/NCM_5807_Nordic_Statistics/Document/DMP/Övrigt/control_report.csv",
                                     flags = "-s", # recursive
                                     extra_flags = NULL,
                                     custom_output_name = NULL,
                                     promptme = T # if True, a popup window shows up
) {
  #stopifnot(dir.exists(in_path))
  paths <- pxjob_helper_create_output_dirs("generate_px_reports")
  out_path <- paths$out_path
  err_path <- paths$err_path
  logfile_path <- paths$logfile_path
  job <- "report"
  
  if (!is.null(custom_output_name)) out_path <- str_glue("{out_path}{custom_output_name}.csv")
  
  # flags <- "-s"
  flags <- str_c(flags, extra_flags)
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    log_path = logfile_path,
    out_path = out_path,
    err_path = err_path,
    meta_path = meta_path,
    flags = flags,
    extra_flags = extra_flags
  )
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Generate px-report?
                                    indata = {in_path}
                                    output = {out_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    print("Generating px report...")
    res <- system(commandp)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Output: {out_path}"))
  }
}




pxjob_split <- function(
    .px_matris,
    cnt,
    use_existing = T,
    base_path = str_glue("{getwd()}/out/Output/DMP/PX"),
    flags = "-b -o0",
    promptme = F
) {
  
  in_path <- get_matrix_path_from_db(.px_matris = .px_matris)
  loc_var <- get_loc_var(.px_matris = .px_matris) %>%
    str_replace_all("\\.", " ")
  
  matrisnamn <- .px_matris %>% str_to_lower()
  
  if (!is.null(cnt)) countryname <- convert_cnt_to_country(cnt)
  
  matrisnamn <- str_c(matrisnamn, "_", cnt, ".px")
  
  paths <- pxjob_helper_create_output_dirs("split",
                                           base_path = base_path,
                                           create_err = T,
                                           create_out = T
  )
  logfile_path <- paths$logfile_path
  out_path <- paths$out_path
  out_path <- str_c(out_path, matrisnamn)
  err_path <- paths$err_path
  job <- "split"
  
  
  # Om splittade filen redan finns och om man vill använda den:
  if (file.exists(out_path) & use_existing) {
    return(out_path)
  }
  
  
  
  tmp_csv <- as.character(str_glue('STUB;HEADING;takevalues;
{loc_var};time;;{str_to_upper(cnt)};
'))
  con <- tempfile(fileext = '.csv' )
  writeLines(tmp_csv,con)
  #write_lines(tmp_csv, con)
  
  # print(tmp_csv)
  
  #shell.exec(con)
  
  print("Flags selected: ")
  print(pxjob_helper_flag_translator(flags, job))
  
  
  commandp <- pxjob_wrapper(
    in_path = in_path,
    job = job,
    out_path = out_path,
    err_path = err_path,
    meta_path = con,
    log_path = logfile_path,
    flags = "-b -o0"
  )
  print(commandp)
  
  
  if (promptme) {
    yesno <- utils::askYesNo(str_glue("Split px file for {countryname} on {loc_var}?
                                    indata = {in_path}
                                    out = {out_path}"))
  } else {
    yesno <- NA
  }
  
  if ((promptme & yesno == T & !is.na(yesno)) | promptme == F) {
    t0 <- Sys.time()
    print("Splitting...")
    res <- system(commandp)
    print(res)
    ifelse(res == 0, print("Success"), res)
    print(str_glue("Log: {logfile_path}
                   Out = {out_path}"))
    print(Sys.time()-t0)
    file.remove(con)
    return(out_path)
  }
  file.remove(con)
}


# keyword;varname;valname;value
# CHARSET;;;ANSI
# CODEPAGE;;;iso-8859-15
# AXIS-VERSION;;;2013
# LANGUAGE;;;sv
# CONTENTS;;;XXXX01: titel
# STUB;;;län,typ
# HEADING;;;år
# TIMEVAL;år;A1;2022
# UNITS;;;Antal
# SUBJECT-AREA;;;Hej
# SUBJECT-CODE;;;XXXX
# DECIMALS;;;0
# SOURCE;;;Källa
# CONTACT;;;Källa
# CREATION-DATE;;;20230531 15:31
# NOTE;;;Här ligger fotnoten för hela tabellen
# VALUENOTE;län;Östergötland;Här är en specifik fotnot för Östergötland
# ELIMINATION;län;;Riket



Q <- '"'
E <- ";"
e <- "="
comma <- ","
oq <- "("
eq <- ")"

addquotes <- function(txt) {
  str_c(Q, txt, Q)
}

splitlist <- function(txt) {
  str_split_1(txt, ",") %>% 
    addquotes() %>% 
    str_c(collapse=",")
}

meta <- meta %>% 
  mutate(value_parsed = map_chr(value, splitlist))

meta 



meta %>% 
  mutate(s = ifelse(is.na(varname) & is.na(valname),
                    str_c(keyword, e, value_parsed, E),
                    NA
  ),
  
  s = ifelse(!is.na(varname) & is.na(valname),
             str_c(keyword, oq, Q, varname, Q, eq, e, value_parsed, E),
             s
             
  ),
  
  s = ifelse(!is.na(varname) & !is.na(valname),
             str_c(keyword, oq, Q, varname, Q, comma, Q, valname, Q, eq, e, value_parsed, E),
             s
             
             
  )
  ) 









