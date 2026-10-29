# Functions to identify and record marks on the UO Score Answer form
# Jeremy Piger
# version 3.0
# Most recent version available at: https://github.com/jpiger/UO_Score
#
# Shared by UO_Score_OMR_v3.0.R (interactive) and tests/run_omr_test.R (non-interactive).
# process_form() handles one page of the scanned pdf and never stops on a bad page:
# it returns status "ok", "fail" (a form check failed) or "error" (R threw an error).

# Load Libraries
library(magick)
library(EBImage)
library(pdftools)
library(collapse)

# Parameters for UO Score Answer Form
omr_default_params <- list(
  density = 600, # Resolution used to read each pdf page. Column offsets below assume 600.
  length_SID = 9, # Length of UO Student ID
  length_form_number = 2, # Maximum number of digits in form version number
  length_question_option = 5, # Maximum Number of MC Question Options
  length_alphabet = 26, # Number of Options for Last Name and First Name Letter Entries
  length_last_name = 15, # Maximum length of last name entry
  length_first_name = 10, # Maximum length of first name entry
  max_questions = 120, # Maximum number of questions on the form
  col_min = 51, # Left pixel column of band searched for row guides
  col_max = 250, # Right pixel column of band searched for row guides
  num_guides = 61, # Number of row guides on the form
  threshold_method = "otsu", # How the guide threshold is chosen: "otsu" (default) or "max_gap" (v2.0 method)
  blank_threshold = 0.10, # Threshold to detect blanks - must be between 0 and 1.
                          # Lower numbers will detect less blanks, but may assign a random answer when none is given.
                          # Higher numbers will detect more blanks, but may miss more legitimate answers
  mult_answer_threshold = 0.05, # Threshold to detect multiple answers
                                # Lower numbers will detect less multiple answers
                                # Higher numbers will detect more multiple answers
  name_threshold = 0.05 # Gap between darkest and next darkest letter needed to record a name letter
)

# Labels for the columns of the output .csv file
omr_record_labels <- function(num_questions) {
  c("Page_Number", "Last_Name", "First_Name", "Student_ID", "Form Number", (1:1:num_questions))
}

# Define function that will identify marks
mark_identify <- function(row_start,row_end,col_start,col_end,study_image){
  threshold_image_check = study_image[row_start:row_end,col_start:col_end];
  num_pixels = (dim(threshold_image_check)[1])*(dim(threshold_image_check)[2]);
  percent_filled = sum(threshold_image_check)/num_pixels;
  return(percent_filled);
}

# Read one page of the pdf as a grayscale magick image
read_form_image <- function(input_file_name_path, page_num, density = 600) {
  fout <- tempfile(fileext = ".pdf")
  x <- pdf_subset(input_file_name_path, page_num, fout)
  image <- image_read(x, density = density)
  unlink(fout)
  image_convert(image, colorspace = 'gray')
}

# Calibrate Position of Left-Hand-Side Row Guides on UO Score
# Returns guide_mat (one row per guide: top, bottom, left, right) or a failure reason.
# diag is an environment that collects intermediate values for diagnosing failed forms,
# so they survive even if an error is thrown partway through.
detect_guides <- function(gray_image_EBImage, page_num, params = omr_default_params, diag = new.env()) {

  fail <- function(reason) list(ok = FALSE, reason = reason, guide_mat = NULL)

  # Pick left and right column indicators that you think will contain all the guides
  col_min <- params$col_min
  col_max <- params$col_max
  W <- col_max-(col_min-1)
  H <- dim(gray_image_EBImage)[2]
  diag$col_min <- col_min
  diag$col_max <- col_max
  diag$image_dim <- dim(gray_image_EBImage)

  # Determine a threshold to identify guide marks
  if (params$threshold_method == "otsu") {
    # Otsu's method: threshold that best separates dark and light pixels in the guide band
    threshold <- otsu(gray_image_EBImage[col_min:col_max,1:H], range = c(0, 1))
  } else {
    # Largest gap between consecutive sorted gray levels
    sorted_image <- sort(c(gray_image_EBImage[1:W,1:H]))
    max_diff_loc <- which.max(sorted_image[2:(W*H)]-sorted_image[1:((W*H)-1)])
    threshold <- mean(sorted_image[max_diff_loc:(max_diff_loc+1)])
  }
  diag$threshold <- threshold

  # Check col_min and col_max to make sure they contain all guides
  check_col_min <- sum(as.numeric((gray_image_EBImage[col_min,1:H]<threshold)))
  check_col_max <- sum(as.numeric((gray_image_EBImage[col_max,1:H]<threshold)))
  diag$check_col_min <- check_col_min
  diag$check_col_max <- check_col_max

  if (check_col_min>25) {
    return(fail(paste("col_min may not contain all guides for form", page_num)))
  }

  if (check_col_max>25) {
    return(fail(paste("col_max may not contain all guides, or may include other parts of form, for form", page_num)))
  }

  # Identify possible position of guides
  all_dark <- which(gray_image_EBImage[col_min:col_max,1:H]<threshold)
  diag$num_dark_pixels <- length(all_dark)

  if (length(all_dark)==0) {
    diag$num_guides <- 0
    return(fail(paste("No guide marks darker than threshold", round(threshold,4), "found in form", page_num)))
  }

  all_dark_coord <- matrix(NA, length(all_dark), 2)
  all_dark_coord[,1] <- (all_dark %/% W) + 1*((all_dark %% W)>0)
  all_dark_coord[,2] <- ((all_dark %% W)*((all_dark %% W)>0) + W*((all_dark %% W)==0)) + (col_min-1)

  all_dark_row_counts <- as.data.frame(table(all_dark_coord[,1]))

  all_dark_row_counts <- all_dark_row_counts[all_dark_row_counts[,2]>=70,]

  all_dark_coord = all_dark_coord[(all_dark_coord[,1] %in% all_dark_row_counts[,1]),]

  unique_row = unique(all_dark_coord[,1])

  if (length(unique_row)==0) {
    diag$num_guides <- 0
    return(fail(paste("Wrong number of guides detected in form", page_num)))
  }

  guide_mat_temp <- matrix(NA, length(unique(all_dark_coord[,1])), 4)

  num_guides=1

  for (iter in 1:length(unique_row)) {

    sub_mat = all_dark_coord[(all_dark_coord[,1]==unique_row[iter]),]

    guide_mat_temp[iter,1] = unique_row[iter]
    guide_mat_temp[iter,2] = min(sub_mat[,2])
    guide_mat_temp[iter,3] = max(sub_mat[,2])

    if (iter>1) {
      if ((unique_row[iter]-unique_row[iter-1])>5) {
        num_guides = num_guides+1
      }
    }

    guide_mat_temp[iter,4] = num_guides

  }

  diag$num_guides <- num_guides
  # Rough bounds of every detected guide (top, bottom, left, right), kept even when the count is wrong
  diag$guide_bounds <- cbind(tapply(guide_mat_temp[,1], guide_mat_temp[,4], min),
                             tapply(guide_mat_temp[,1], guide_mat_temp[,4], max),
                             tapply(guide_mat_temp[,2], guide_mat_temp[,4], min),
                             tapply(guide_mat_temp[,3], guide_mat_temp[,4], max))

  if (num_guides != params$num_guides) {
    return(fail(paste("Wrong number of guides detected in form", page_num)))
  }

  guide_mat = matrix(NA,num_guides,4)

  for (iter in 1:num_guides) {
    sub_mat = as.data.frame(guide_mat_temp[(guide_mat_temp[,4]==iter),1:3])
    guide_mat[iter,1] = min(sub_mat[,1])
    guide_mat[iter,2] = max(sub_mat[,1])
    guide_mat[iter,3] = fmode(sub_mat[,2])
    guide_mat[iter,4] = fmode(sub_mat[,3])
  }

  list(ok = TRUE, reason = NA, guide_mat = guide_mat)
}

#  Mapping of Guides and Columns to Objects of Interest

# Last Name: Guides #9-34; First Name: Guides #9-34; SID Number: Guides #15-24;
# Form Number: Guides #30-39; Questions in blocks of 20: Guides #42-61

# Column guides - Column #1 is column before first "Last Name" column.
# There are 46 total columns.

# Columns 1, 17 are blank; Columns 2-16 are Last Name; Columns 18-27 are First Name
# Column 29 is Middle Initial; Columns 31-39 is SID; Columns 33-34 are Form Version Number
# Columns 2-6 are Questions 1-20; Columns 10-14 are Questions 21-40; Columns 18-22 are Questions 41-60
# Columns 26-30 are Questions 61-80; Columns 34-38 are Questions 81-100; Columns 42-46 are Questions 101-120

# Read the marks on one form. Returns the recorded student information and answers.
# boxes records every bubble area sampled (left, right, top, bottom), for drawing debug images.
read_marks <- function(gray_image_EBImage, guide_mat, num_questions, params = omr_default_params) {

  boxes <- matrix(NA, 0, 4)

  # Darkness score of the bubble in column col_num on the row of guide guide_num
  bubble_score <- function(guide_num, col_num) {
    left_m <- guide_mat[guide_num,3]
    right_m <- guide_mat[guide_num,4]
    top_m <- guide_mat[guide_num,1]
    bottom_m <- guide_mat[guide_num,2]
    offset <- col_num*100 + 105
    boxes <<- rbind(boxes, c(left_m+offset, right_m+offset, top_m, bottom_m))
    mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
  }

  # Read a column of digit bubbles (0-9)
  read_digits <- function(guides_temp, cols_temp) {
    digits <- rep(NA,length(cols_temp))
    for (i in 1:length(cols_temp)) {
      rank_mat <- matrix(sapply(guides_temp, bubble_score, col_num = cols_temp[i]), ncol = 1)
      if (any((max(rank_mat)-rank_mat)>params$blank_threshold)==TRUE) {
        digits[i] <- (which.min(rank_mat[,1])-1);
      }
    }
    paste(digits[!is.na(digits)], collapse="")
  }

  # Read a column of letter bubbles (A-Z)
  read_letters <- function(guides_temp, cols_temp) {
    letters_out <- rep(NA,length(cols_temp))
    for (i in 1:length(cols_temp)) {
      rank_mat <- matrix(sapply(guides_temp, bubble_score, col_num = cols_temp[i]), ncol = 1)
      if ((min(rank_mat[-which.min(rank_mat[,1]),1]))-min(rank_mat[,1])>params$name_threshold) {
        letters_out[i] <- LETTERS[which.min(rank_mat[,1])];
      }
    }
    paste(letters_out[!is.na(letters_out)], collapse="")
  }

  # Capture Student ID Number
  SID_number_onenum <- read_digits(c(15:24), c(31:(31+params$length_SID-1)))

  # Capture Form Version Number
  Form_number_onenum <- read_digits(c(30:39), c(33:(33+params$length_form_number-1)))

  # Capture Student Last Name
  Last_Name_str <- read_letters(c(9:(9+params$length_alphabet-1)), c(2:(2+params$length_last_name-1)))

  # Capture Student First Name
  First_Name_str <- read_letters(c(9:(9+params$length_alphabet-1)), c(18:(18+params$length_first_name-1)))

  # Capture Student's Answers to Multiple Choice Questions
  Answers <- rep(NA,num_questions)
  guides_temp <- c(42:61) # Relevant Row Guides
  block_first_col <- c(2, 10, 18, 26, 34, 42) # First column of each block of 20 questions

  for (i in 1:num_questions) {

    guide_num <- guides_temp[((i-1) %% 20) + 1]
    cols_temp <- block_first_col[((i-1) %/% 20) + 1] + (0:(params$length_question_option-1))
    rank_mat <- matrix(sapply(cols_temp, bubble_score, guide_num = guide_num), ncol = 1)

    if (any((max(rank_mat)-rank_mat)>params$blank_threshold)==FALSE) {
      Answers[i]="BLANK"
    } else {
      Answer_temp = LETTERS[which((rank_mat-min(rank_mat))<params$mult_answer_threshold)]
      Answers[i] = paste(Answer_temp, collapse = ",")
    }

  }

  list(Last_Name = Last_Name_str, First_Name = First_Name_str, Student_ID = SID_number_onenum,
       Form_Number = Form_number_onenum, Answers = Answers, boxes = boxes)
}

# Process one page of the scanned pdf.
# Returns a list with:
#   status  - "ok", "fail" (form check failed) or "error" (R error while processing)
#   reason  - message for the failure file (NA when ok)
#   record  - vector for the output .csv (NULL unless ok)
#   names   - Last_Name, First_Name, Student_ID (for naming individual pdf files)
#   diag    - guide detection details; image and boxes are added when keep_image = TRUE
process_form <- function(input_file_name_path, page_num, num_questions,
                         params = omr_default_params, keep_image = FALSE) {

  result <- list(status = "error", reason = NA, record = NULL, names = NULL, diag = list(),
                 image = NULL, boxes = NULL)
  diag <- new.env()

  tryCatch({

    gray_image <- read_form_image(input_file_name_path, page_num, params$density)
    if (keep_image) result$image <- gray_image

    # Move from magick package to EBImage for remainder of analysis
    gray_image_EBImage <- as_EBImage(gray_image)

    guides <- detect_guides(gray_image_EBImage, page_num, params, diag)
    diag$guide_mat <- guides$guide_mat
    result$diag <- as.list(diag)

    if (!guides$ok) {
      result$status <- "fail"
      result$reason <- guides$reason
      return(result)
    }

    marks <- read_marks(gray_image_EBImage, guides$guide_mat, num_questions, params)
    if (keep_image) result$boxes <- marks$boxes

    result$status <- "ok"
    result$record <- c(page_num, marks$Last_Name, marks$First_Name, marks$Student_ID,
                       marks$Form_Number, marks$Answers)
    result$names <- c(marks$Last_Name, marks$First_Name, marks$Student_ID)
    result

  }, error = function(e) {
    result$diag <- as.list(diag)
    result$status <- "error"
    result$reason <- paste0("Error processing form ", page_num, ": ", conditionMessage(e),
                            " [in: ", paste(deparse(conditionCall(e)), collapse = " "), "]")
    result
  })
}
