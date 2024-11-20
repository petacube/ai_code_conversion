Rscript -e "library(cyclocomp); cyclocomp_package('tidyr')"
Rscript -e "library(lintr); lintr::lint_dir('../tidyr')"
Rscript -e "library(goodpractice); gp('../tidyr')"
