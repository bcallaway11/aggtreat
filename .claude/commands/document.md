Regenerate roxygen2 documentation and check for gaps.

## Steps
1. Run: `Rscript -e "devtools::document()"`
2. Report any new or changed files in `man/`
3. Check that every exported function has: @param for all arguments,
   @return describing the output, @examples with a runnable example,
   and @references citing Caetano, Caetano, Callaway, and Dyal (2025)
4. List any exported functions missing any of the above — these are
   documentation gaps that must be fixed before the next commit
5. Report the updated NAMESPACE exports
