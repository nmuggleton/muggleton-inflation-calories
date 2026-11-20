#!/usr/bin/env bash
# compile.sh -- build manuscript/tex/manuscript.pdf and
# manuscript/tex/supplement.pdf with latexmk (pdflatex + bibtex; the class and
# style files are found through manuscript/tex/latexmkrc), then remove the
# auxiliary files. Usage: bash src/tools/compile.sh
set -euo pipefail
# The .tex files read ../tables and ../figures
cd "$(dirname "$0")/../../manuscript/tex"
for f in manuscript supplement; do
  latexmk -pdf -interaction=nonstopmode "$f.tex" >/dev/null
  latexmk -c "$f.tex" >/dev/null
  echo "built manuscript/tex/$f.pdf"
done
