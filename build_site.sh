#!/bin/bash
set -e

echo "Building English website..."
quarto render --profile english

echo "Building Czech website..."
quarto render --profile czech

echo "Building English CV (PDF)..."
quarto render cv_en.qmd --to pdf
mkdir -p docs
mv cv_en.pdf docs/

echo "Building Czech CV (PDF)..."
quarto render cv_cs.qmd --to pdf
mkdir -p docs
mv cv_cs.pdf docs/

echo "Build complete! PDFs moved to docs/"
