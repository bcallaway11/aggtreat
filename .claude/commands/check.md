Run `R CMD check` on the package and report results.

## Steps
1. From the repo root, run: `Rscript -e "devtools::check()"`
2. Collect all ERRORs, WARNINGs, and NOTEs from the output
3. Report them grouped by severity — ERRORs first, then WARNINGs, then NOTEs
4. If there are no ERRORs or WARNINGs, say so explicitly
5. Do not proceed with any other task if ERRORs are present
