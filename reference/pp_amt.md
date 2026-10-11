# Adaptive Matrices Test data

This dataset contains real data from the 'Adaptive Matrices Test' (AMT),
which is a computer-administered test. This power test assesses logical
reasoning as an indicator of general intelligence. The ability to
identify regularities and draw logical conclusions is a very good
predictor of long-term success at work. The dataset is sparse, because
the test tailors a specific set of items for each examinee's ability
level. (More information about adaptive testing:
<https://en.wikipedia.org/wiki/Computerized_adaptive_testing>)

## Format

A list with two data.frames. The first data.frame 'daten_amt' contains
298 columns and 710 rows. Each row contains responses from one examinee.
The second data.frame 'betas' contains the difficulty parameter (1PL)
(These parameters came with the raw-score extraction.).

## Source

Division of Psychological Assessment and Applied Psychometrics, Faculty
of Psychology, University of Vienna

- ID: id of person

- AGE: age in years (with ages below 18 and above 34 are collapsed)

- TE_GA:

  - TE: self-assessment. To pass a psychological assessment course, the
    students have to complete several hours self assessment on a bunch
    of tests, to get familiar with them.

  - GA: testing for an assessment report. The students also have to test
    other people (not psychologists nor psychology students) in order to
    write an assessment report.

- FORM: There are several different versions of this test, which differ
  in test length, duration etc ...

- TIME1: start time

- TIME2: end time

- REL: reliability for each person

- i: items

## Details

The data are provided from the University of Vienna, Faculty of
Psychology, Department of Psychological Assessment. Thanks to
Schuhfried.

## References

- Hornke, L. F., Etzel, S., & Rettig, K. (2003). Manual Adaptive
  Matrices Test (AMT). *Moedling: SCHUHFRIED GmbH*.

## See also

[PPass](https://manuelreif.github.io/PP/reference/PPass.md)
