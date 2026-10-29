# R script to identify and record marks on the UO Score Answer form
# Jeremy Piger
# version 3.0
# Most recent version available at: https://github.com/jpiger/UO_Score
#
# The mark identification code lives in omr_functions.R, which must be in the same folder as this script.

# Clear Environment and Console
rm(list = ls())
cat(rep("\n", 100))

# Load Libraries
library(svDialogs)

# Load mark identification functions from the folder holding this script
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
if (is.null(script_dir) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
}
if (is.null(script_dir) || !file.exists(file.path(script_dir, "omr_functions.R"))) {
  dlgMessage(paste("Select the omr_functions.R file that came with this script - press OK to browse for file."))$res
  script_dir <- dirname(file.choose())
}
source(file.path(script_dir, "omr_functions.R"))

# Set Exam Specific Parameters
dlgMessage(paste("Select .pdf file that holds scans of forms - press OK to browse for file."))$res
input_file_name_path = file.choose() # Location of single pdf file holding scanned forms to be processed. One page per form

dlgMessage(paste("Choose location and name for .csv file that will hold recorded student responses - press OK to browse."))$res
output_file_name_path <- file.choose(new=TRUE)
output_file_name_path = paste0(output_file_name_path,".csv")

dlgMessage(paste("Choose location and name for .txt file that will record any forms that can't be processed - press OK to browse."))$res
fail_file_name_path <- file.choose(new=TRUE)
fail_file_name_path = paste0(fail_file_name_path,".txt")

num_forms <- pdf_info(input_file_name_path)$pages # Number of exams to be graded. Assumes one scanned form per page in pdf file
num_questions <- dlg_input(message = "Enter number of questions on the exam, must be 120 or less")
num_questions = as.numeric(num_questions$res)
if (is.na(num_questions) || num_questions < 1 || num_questions > omr_default_params$max_questions) {
  stop("Number of questions must be between 1 and 120.")
}

omr_params <- omr_default_params
omr_params$blank_threshold <- 0.10  # Threshold to detect blanks - must be between 0 and 1.
                                    # Lower numbers will detect less blanks, but may assign a random answer when none is given.
                                    # Higher numbers will detect more blanks, but may miss more legitimate answers
omr_params$mult_answer_threshold <- 0.05  # Threshold to detect multiple answers
                                          # Lower numbers will detect less multiple answers
                                          # Higher numbers will detect more multiple answers
fail_list <- list() # List to hold forms that were not processed for various reasons

save_individual_pdf <- dlg_message(message="Do you want to save individual .pdf files of each student's form? Warning: This could create a large number of files.", type="yesno")
save_individual_pdf <- save_individual_pdf$res

# Location of directory to hold individual .pdf files of each processed form
if (save_individual_pdf=="yes") {
  root_dir="."
  output_dir = "individual_pdf_files"
  output_image_path <- file.path(root_dir,output_dir)
  if (file.exists(output_dir)=="FALSE") {
    dir.create(output_image_path)
  }
}

# Initialize storage matrix
num_rows <- 5 + num_questions; # Page number, Last Name, First Name, UO Student ID, and Form Version Number, then answers
store_mat <- matrix(nrow=num_rows,ncol=0)

form_count <- 0 # counter to count the number of forms processed. Useful if processing a subset of main file.
successful_form_count <- 0 # counter to count the number of forms sucessfully processed.

for (page_num in 1:num_forms) {

  form_count <- form_count + 1

  cat(rep("\n", 100))
  print_update <- paste("Processing for", page_num, "of", num_forms)
  print(print_update)

  result <- process_form(input_file_name_path, page_num, num_questions, omr_params)

  if (result$status != "ok") {
    fail_list = rbind(fail_list,result$reason)
    fail_list = rbind(fail_list,"")
    next
  }

  store_mat <- cbind(store_mat, as.matrix(result$record))

  if (save_individual_pdf=="yes") {
    output_file <- file.path(output_image_path, paste0(paste(result$names, collapse="_"), ".pdf"))
    pdf_subset(input_file_name_path, page_num, output_file)
  }

  successful_form_count = successful_form_count+1

}

store_vec_labels <- as.matrix(omr_record_labels(num_questions))
store_mat <- t(store_mat)
store_mat <- as.data.frame(store_mat);
names(store_mat) <- t(store_vec_labels)

write.csv(store_mat, output_file_name_path, row.names = FALSE)

if (length(fail_list)!=0){
  write(unlist(fail_list),fail_file_name_path)
  }

print_update1 <- paste("Processing Complete.", form_count, "forms attempted to be processed.")
print_update2 <- paste("There were", form_count-successful_form_count, "forms unable to be processed.")
cat(rep("\n", 100))
print(print_update1)
print(print_update2)
