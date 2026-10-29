# UO_Score
R scripts to read and grade the UO Score Answer Form from scanned images.

**Step 1: read the forms.** `UO_Score_OMR_v3.0.R` identifies and records the answers on a `.pdf` of scanned UO Score Answer Forms. It needs `omr_functions.R` in the same folder. See [UO_Score_OMR_v3.0_User_Guide.md](UO_Score_OMR_v3.0_User_Guide.md).

**Step 2: grade.** `Grade_UO_Score_OMR_v2.0.R` produces scores and flags problematic student responses using the output from `UO_Score_OMR_v3.0.R`. See `Grade_UO_Score_OMR_v2.0_User Guide.pdf`.
