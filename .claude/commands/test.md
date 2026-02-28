Run the testthat suite and report results. Optionally accepts a file or test
name to filter: `/test classify` runs only tests matching "classify".

## Steps
1. If an argument was provided, run:
   `Rscript -e "devtools::test(filter='$ARGUMENTS')"`
   Otherwise run: `Rscript -e "devtools::test()"`
2. Report how many tests passed, failed, and were skipped
3. For any failures, show the test name, the expectation that failed, and the
   actual vs. expected values
4. If all tests pass, say so explicitly and note the count
