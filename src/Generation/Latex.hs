module Generation.Latex (generateLatex) where

import Syntax.Ast

import Generation.Latex.Dom
import Generation.Latex.Df

prologue = "\\documentclass{article}\n"
  ++ "%%%%% Add these packages and commands in your document\n\n"
  ++ "\\usepackage{amsmath}\n"
  ++ "\\usepackage{amssymb}\n"
  ++ "\\usepackage{semantic}\n"
  ++ "\\newcommand{\\sC}[1]{\\textrm{\\textup{\\fontfamily{cmss}\\selectfont \\textbf{#1}}}}\n"
  ++ "\\newcommand{\\sco}[1]{\\textrm{\\textup{\\fontfamily{cmss}\\selectfont \\textbf{#1}}}}\n"
  ++ "\\newcommand{\\sv}[1]{\\textrm{\\(\\mathtt{#1}\\)}}\n"
  ++ "\\newcommand{\\tsys}[1]{\\textrm{\\textbf{\\textit{#1}}}}\n"
  ++ "\\newcommand{\\ltrue}{\\textrm{\\(t\\kern-0.1em t\\)}}\n"
  ++ "\\newcommand{\\lfalse}{\\textrm{\\(f\\kern-0.3em f\\)}}\n"
  ++ "\\newcommand{\\namedRule}[2]{\n"
  ++ "\t\\textrm{\\footnotesize [#1]: \\ \\ \\ } &\n"
  ++ "\t#2\n"
  ++ "}\n\n"
  ++ "\\begin{document}\n\n"

epilogue = "\n\n\\end{document}\n\n"

generateLatex :: Spec -> String
generateLatex (Spec dfs evs) = prologue
  ++ printDomDfs dfs
  ++ printDataDfs dfs
  ++ printTSysDfs dfs
  ++ epilogue
