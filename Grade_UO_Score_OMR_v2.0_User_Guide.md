# User Guide for version 2.0 of Grade_UO_Score_OMR

The R script `Grade_UO_Score_OMR_v2.0.R` scores exams and flags possible problems with student entries on the "UO Score Answer Form." It uses the `.csv` file of student responses produced in a first step by `UO_Score_OMR_v3.0.R` (see the UO_Score_OMR v3.0 user guide). Output from `UO_Score_OMR_v2.0.R` also works.

The script produces:

1. A single `.csv` file holding, for each student, their last name, UO ID number, exam score, and the page number of their form in the scanned `.pdf`.
2. A single `.txt` file of alerts about irregular student responses:
   1. questions where a student entered no answer;
   2. questions where a student entered more than one answer;
   3. students who did not enter a valid version number;
   4. students who probably entered the wrong version number.

## Setup (one time)

1. Install [R](https://cran.r-project.org/) and, if you like, [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the required packages by running the following in the R console:

   ```r
   install.packages(c("stringr", "svDialogs"))
   ```

   These are already installed if you have set up `UO_Score_OMR_v3.0.R`.
3. Download `Grade_UO_Score_OMR_v2.0.R`.

## Using the script

1. **Record the student responses.** Run `UO_Score_OMR_v3.0.R` to produce a `.csv` file of student responses.
2. **Create the answer key.** Make a `.csv` file holding the answer key for every version of the exam (see "The answer key file" below).
3. **Run the script.** Open `Grade_UO_Score_OMR_v2.0.R` in RStudio and either click "Source" or select all text and click "Run".
4. **Answer the dialog boxes.** They appear in this order:
   1. *Enter the number of test versions.*
   2. *Select the .csv file that holds the answer key.*
   3. *Select the .csv file of student responses* produced by `UO_Score_OMR_v3.0.R`.
   4. *Choose location and name for the .csv file of student scores.* Type a file name **without** an extension. The script adds `.csv`.
   5. *Choose location and name for the .txt file of alerts.* Again type a name without an extension. The script adds `.txt`.

## The answer key file

The answer key is a `.csv` file with a header row, then one row per question:

- **Column 1** holds the question numbers, starting from 1.
- **Each following column** holds the answers for one exam version, in numerical order. Answers for version 1 go in column 2, answers for version 2 in column 3, and so on.

Students mark their version number in box 5 ("TEST") on the form, using integers starting at 1. If there is only one version of the exam, put its answers in column 2. In that case anything students entered in box 5 is ignored.

For example, a key for a four-question exam with two versions:

```
Question,Version 1,Version 2
1,A,C
2,D,B
3,B,B
4,E,A
```

Answers should be capital letters, matching how the response file records them. Spaces around an answer are ignored. The key should have one row for each question column in the response file.

## The scores .csv file

| Column | Contents |
|---|---|
| `Student` | Student's last name |
| `SIS User ID` | UO ID number |
| `Score` | Number of questions answered correctly |
| `Page Number` | Page of the scanned `.pdf` the form came from |

A question counts as correct only if the student's answer exactly matches the key. A question left blank or marked with more than one answer is counted wrong.

## The alerts .txt file

Each alert names the student (last name, first name, UO ID number) and the page number of their form, so you can check the original scan.

- **Blank answers** lists the questions the student left blank.
- **Multiple answers** lists the questions where the student marked more than one answer.
- **No version or invalid version selected** (exams with more than one version only). The student left box 5 empty or entered a number that isn't one of the versions. The student is scored against every version and gets the highest score. The alert reports that score and which version produced it.
- **Probable wrong version selected** (exams with more than one version only). The student scored below 50% on the version they marked, and another version gives a higher score. The alert reports both scores. **The student keeps the score from the version they marked.** Check the form, and change the score by hand if you decide the student marked the wrong version.

## Adding your own columns to the response file

You can add columns to the response `.csv` file before grading, such as roster information (major, grading option, email). Add them **after** the first four columns (`Page_Number`, `Last_Name`, `First_Name`, `Student_ID`) and **before** the `Form Number` column. Leave `Form Number` and the answer columns after it unchanged. The script finds the `Form Number` column by name (`Form_Number` also works) and grades every column after it as an answer.

## Changes in the October 2026 update

- **Works with added columns.** The script finds the version column by name rather than by position, so it grades both the plain output of `UO_Score_OMR_v3.0.R` and response files with extra columns added.
- **Probable wrong version no longer changes the score.** Earlier copies gave the student the higher score from the other version. The script now keeps the score from the version the student marked and reports both scores in the alert.
- **Stray spaces in the answer key are ignored**, so an entry like ` D` still matches `D`.
