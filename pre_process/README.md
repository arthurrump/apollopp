# Pre-processing scripts

This folder contains three scripts used to process student submissions:

- *anonymize.py* is used to anonymize student projects so that they could be used for the evaluation of this project. The scripts unpacks the submissions archive that is available through Canvas and replaces all names and student numbers with randomly generated values. To achieve full anonymization rather than pseudonization, the mapping is destroyed after the replacement. The script also links submissions to assessments, which can be filled in rubrics from Canvas or an Excel file where each submission is assessed on a separate worksheet.
- *copy_assessed.py* works with the assessments from Canvas rubrics and copies over only those submissions that have been assessed. The particular use case is when two group members both submit the project, but the rubric is only filled in for one of them.
- *pde_to_java.py* converts Processing projects to Java code using the *processing-java* command line tool, which needs to be on the PATH. If there is no valid Processing project or the conversion errors out, the submissions is deleted.

