# User Guide for version 3.0 of UO_Score_OMR

The R script `UO_Score_OMR_v3.0.R` identifies and records the entries on the front page of the "UO Score Answer Form." It reads:

1. Last Name
2. First Name
3. UO ID Number
4. Form Version Number (box 5, "TEST")
5. Answers to multiple choice questions, from question 1 up to a maximum of 120

The script produces:

1. A single `.csv` file holding all student responses.
2. A single `.txt` file listing forms that could not be processed. This file is not created if every form was processed.
3. If you want them, one `.pdf` file per student holding an image of their scanned form, named with the student's name and UO ID number. The script asks whether to create these files.

The `.csv` file can then be graded with `Grade_UO_Score_OMR_v2.0.R`.

## What's new in version 3.0

- **More forms are read successfully.** The script finds the black timing marks on the left edge of the form with a new method that is not thrown off by small dark specks on the scan. In testing on 267 scanned forms, version 3.0 read 266 forms and version 2.0 read 261. On every form both versions read, the recorded answers were identical.
- **One bad page no longer stops the run.** If a page causes an error, the script records it in the `.txt` file of unprocessed forms and moves on to the next page.
- **The script now comes in two files.** `UO_Score_OMR_v3.0.R` is the script you run. `omr_functions.R` holds the code that reads the marks. Keep both files in the same folder.

The dialog boxes and the format of the output `.csv` file are the same as in version 2.0.

## Setup (one time)

1. Install [R](https://cran.r-project.org/) and, if you like, [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the required packages by running the following in the R console:

   ```r
   install.packages(c("magick", "pdftools", "collapse", "svDialogs", "stringr"))
   install.packages("BiocManager")
   BiocManager::install("EBImage")
   ```

   `EBImage` is distributed through Bioconductor rather than CRAN, which is why it is installed with `BiocManager`.
3. Download `UO_Score_OMR_v3.0.R` and `omr_functions.R` and put them in the same folder.

## Using the script

1. **Scan the forms.** Create a single `.pdf` file holding the front page of every UO Score Answer Form you want to process, with one form per page. The script was tested with scans from a Xerox AltaLink C8155 copier using its default settings.
2. **Run the script.** Open `UO_Score_OMR_v3.0.R` in RStudio and either click "Source" or select all text and click "Run". From the R console you can also type `source("path/to/UO_Score_OMR_v3.0.R")`.
3. **Answer the dialog boxes.** They appear in this order:
   1. *Select the .pdf file that holds scans of forms.* Browse to your scanned `.pdf`.
   2. *Choose location and name for the .csv file of student responses.* Type a file name **without** an extension. The script adds `.csv`.
   3. *Choose location and name for the .txt file of forms that can't be processed.* Again type a name without an extension. The script adds `.txt`.
   4. *Enter the number of questions on the exam* (120 or fewer).
   5. *Do you want to save individual .pdf files of each student's form?* If you answer "yes", the files are saved in a folder named `individual_pdf_files` inside R's current working directory (run `getwd()` to see where that is).

   If the script cannot find `omr_functions.R` next to itself, it first asks you to browse to that file.
4. **Wait for processing.** Each page takes a few seconds. The console shows which page is being processed. When the run finishes, it reports how many forms were attempted and how many could not be processed.

## The output .csv file

The file has one row per successfully processed form, with these columns:

| Column | Contents |
|---|---|
| `Page_Number` | Page of the scanned `.pdf` the form came from |
| `Last_Name` | Last name as bubbled by the student |
| `First_Name` | First name as bubbled by the student |
| `Student_ID` | UO ID number |
| `Form Number` | Exam version from box 5 ("TEST"). Empty if not filled in. |
| `1`, `2`, ... | One column per question |

Each answer is one of:

- a single letter, such as `C`;
- several letters separated by commas, such as `A,C`, when the student marked more than one answer;
- `BLANK`, when no answer was marked.

Names and ID numbers are recorded exactly as bubbled. A column the student left empty is skipped, so check a name or ID that looks too short against the form itself. `Page_Number` tells you which page of the `.pdf` to look at.

## Forms that could not be processed

Each form the script could not read is listed in the `.txt` file with its page number and a reason. The most common reason is "Wrong number of guides detected". The script locates every row on the form using the black marks along its left edge, and it could not find all 61 of them. This usually happens when the page was scanned crooked, cut off at the edge, or has stray marks or tears along the left side.

For each listed page, you can:

- rescan the form and run the script again on a `.pdf` holding just the rescanned pages, or
- enter that student's answers by hand.

## Adjusting how marks are detected

Near the middle of `UO_Score_OMR_v3.0.R` are two settings that control how marks are read. The defaults work well for typical scans, and most users will not need to change them.

- `omr_params$blank_threshold` (default `0.10`) controls when a question is recorded as `BLANK`. Lower numbers produce fewer blanks, but may record a random answer for a question the student left empty. Higher numbers produce more blanks, but may miss light marks.
- `omr_params$mult_answer_threshold` (default `0.05`) controls when a question is recorded as having multiple answers. Lower numbers produce fewer multiple answers. Higher numbers produce more, and may report a partially erased answer as a second mark.

## Grading

To score the exams, use `Grade_UO_Score_OMR_v2.0.R` (see its user guide). It takes the `.csv` file produced here and an answer key, and it produces a file of scores and a file of alerts about blank answers, multiple answers, and missing or probably wrong version numbers.

You can add your own columns to the `.csv` file before grading, such as roster information, as long as you add them **before** the `Form Number` column and leave `Form Number` and the answer columns after it unchanged. The grading script finds the `Form Number` column by name and reads the answers from the columns that follow it.
