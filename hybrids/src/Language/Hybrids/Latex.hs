module Language.Hybrids.Latex where

import Data.BitString
import Data.Text(replace, unpack, pack)
import Language.Hybrids.AST
    ( Library(..),
      Block(..),
      Routine(..),
      HTerm(If, Assign, Gets, (:>), Return, Loop),
      HExpr(Append, Variable, Literal, Xor),
      HVar(HVar),
      Boolean(..) )
import qualified Data.Functor as String

libToLatex :: Library -> IO ()
libToLatex (Lib name block _blk) =
    putStrLn ( "\\[\n"
            ++ "\\titlecodebox{$\\lib{" ++ unpack (replace (pack "$") (pack "\\$") (pack name)) ++ "}^\\Sigma$}{\n"
            ++ blockToLatex block ++ "\n}"
            ++ "\n\\]")

blockToLatex :: Block -> String 
blockToLatex (Block []) = ""
blockToLatex (Block (x:xs)) = routToLatex x ++ blockToLatex (Block xs)

subsToLatex :: Block -> String 
subsToLatex (Block []) = ""
subsToLatex (Block (x:xs)) = subToLatex x ++ subsToLatex (Block xs)

subToLatex :: Routine -> String 
subToLatex rout = "\\link\n"
                ++ "\\hlcodebox{\n"
                ++ routToLatex rout
                ++ "}"

routToLatex :: Routine -> String 
routToLatex (Rout (Just initial) name sigs body) =
    bodyToLatex 0 initial ++ "\\\\\n"
 ++ "\\underline{$\\subname{" ++ name ++ "}(" ++ show sigs ++ ")$:} \\\\\n"
 ++ bodyToLatex 1 body
routToLatex (Rout Nothing name sigs body) =
    "\\underline{$\\subname{" ++ name ++ "}(" ++ show sigs ++ ")$:} \\\\\n"
 ++ bodyToLatex 1 body
    

bodyToLatex :: Int -> HTerm -> String
bodyToLatex lvl (Assign v e)  =
  (duplicateStr "\\>" lvl) ++ " $" ++ show v ++ " := " ++ exprToLatex e ++ "$"
bodyToLatex lvl (Gets bits v) =
  (duplicateStr "\\>" lvl) ++ " $" ++ show v ++ " \\gets " ++ bitsToLatex bits ++ "$"
bodyToLatex lvl ((:>) t1 t2)  =
  bodyToLatex lvl t1 ++ "\\\\\n" ++ bodyToLatex lvl t2
bodyToLatex lvl (Return e)    =
  (duplicateStr "\\>" lvl) ++ " return $" ++ exprToLatex e ++ "$"
bodyToLatex lvl (Loop b1 b2 t) =
  (duplicateStr "\\>" lvl) ++ " for $i=" ++ show b1 ++ "$ to $" ++ show b2 ++ "$:\\\\\n" ++ bodyToLatex (lvl+1) t
bodyToLatex lvl (If b t) =
  (duplicateStr "\\>" lvl) ++ " if (" ++ boolToLatex b ++ "):\\\\\n" ++ bodyToLatex (lvl+1) t
bodyToLatex _   _        = error "Routine print not defined"

duplicateStr :: String -> Int -> String
duplicateStr _ 0 = ""
duplicateStr str n = str ++ duplicateStr str (n-1)

exprToLatex :: HExpr -> String 
exprToLatex (Variable (HVar name)) = name
exprToLatex (Literal bs) = ppBits (toBits bs)
exprToLatex (Xor e1 e2) = exprToLatex e1 ++ " \\oplus " ++ exprToLatex e2
exprToLatex (Append e1 e2) = exprToLatex e1 ++ " \\| " ++ exprToLatex e2
exprToLatex c = show c

bitsToLatex :: BitWidth -> String 
bitsToLatex (BitWidth n) = "\\bits^" ++ "{" ++ show n ++ "}"

boolToLatex :: Boolean -> String 
boolToLatex (Eq expr1 expr2) = exprToLatex expr1 ++ " == " ++ exprToLatex expr2
boolToLatex (Gt expr1 expr2) = exprToLatex expr1 ++ " > " ++ exprToLatex expr2
boolToLatex (Lt expr1 expr2) = exprToLatex expr1 ++ " < " ++ exprToLatex expr2
boolToLatex (Neq expr1 expr2) = exprToLatex expr1 ++ " \\neq " ++ exprToLatex expr2
boolToLatex (Gte expr1 expr2) = exprToLatex expr1 ++ " \\geq " ++ exprToLatex expr2
boolToLatex (Lte expr1 expr2) = exprToLatex expr1 ++ " \\leq " ++ exprToLatex expr2
boolToLatex (Empty expr) = exprToLatex expr ++ " empty"
boolToLatex (Undef expr) = exprToLatex expr ++ " undefined"

getLatexHeader :: IO()
getLatexHeader = putStrLn "\\usepackage{xspace,graphicx,amsmath,amssymb,xcolor}\n%% operators\n\\newcommand{\\pct}{\\mathbin{\\%}}\n% makes \":=\" aligned better\n\\usepackage{mathtools}\n\\mathtoolsset{centercolon}\n% indistinguishability operator\n% http://tex.stackexchange.com/questions/22168/triple-approx-and-triple-approx-with-a-straight-middle-line\n\\newcommand{\\indist}{  \\mathrel{\\vcenter{\\offinterlineskip\n  \\hbox{$\\sim$}\\vskip-.35ex\\hbox{$\\sim$}\\vskip-.35ex\\hbox{$\\sim$}}}}\n\\renewcommand{\\cong}{\\indist}\n\\newcommand{\\K}{\\mathcal{K}}\n\\newcommand{\\M}{\\mathcal{M}}\n\\newcommand{\\C}{\\mathcal{C}}\n\\newcommand{\\Z}{\\mathbb{Z}}\n\\newcommand{\\Enc}{\\text{\\sf Enc}}\n\\newcommand{\\Dec}{\\text{\\sf Dec}}\n\\newcommand{\\KeyGen}{\\text{\\sf KeyGen}}\n% fancy script L\n\\usepackage[mathscr]{euscript}\n\\renewcommand{\\L}{\\ensuremath{\\mathscr{L}}\\xspace}\n\\newcommand{\\lib}[1]{\\ensuremath{\\L_{\\textsf{#1}}}\\xspace}\n\\newcommand{\\myterm}[1]{\\ensuremath{\\text{#1}}\\xspace}\n\\newcommand{\\bias}{\\myterm{bias}}\n\\newcommand{\\link}{\\diamond}\n\\newcommand{\\subname}[1]{\\ensuremath{\\textsc{#1}}\\xspace}\n%% colors\n\\definecolor{highlightcolor}{HTML}{F5F5A4}\n\\definecolor{highlighttextcolor}{HTML}{000000}\n\\definecolor{bitcolor}{HTML}{a91616}\n%%% boxes for writing libraries/constructions\n\\usepackage{varwidth}\n\\newcommand{\\codebox}[1]{%\n        \\begin{varwidth}{\\linewidth}%\n        \\begin{tabbing}%\n            ~~~\\=\\quad\\=\\quad\\=\\quad\\=\\kill % initialize tabstops\n            #1\n        \\end{tabbing}%\n        \\end{varwidth}%\n}\n\\newcommand{\\titlecodebox}[2]{%\n    \\fboxsep=0pt%\n    \\fcolorbox{black}{black!10}{%\n        \\begin{varwidth}{\\linewidth}%\n        \\centering%\n        \\fboxsep=3pt%\n        \\colorbox{black!10}{#1} \\\\\n        \\colorbox{white}{\\codebox{#2}}%\n        \\end{varwidth}%\n    }\n}\n\\newcommand{\\fcodebox}[1]{%\n    \\framebox{\\codebox{#1}}%\n}\n\\newcommand{\\hlcodebox}[1]{%\n    \\fcolorbox{black}{highlightcolor}{\\codebox{#1}}%\n}\n\\newcommand{\\hltitlecodebox}[2]{%\n    \\fboxsep=0pt%\n    \\fcolorbox{black}{black!15!highlightcolor}{%\n        \\begin{varwidth}{\\linewidth}%\n        \\centering%\n        \\fboxsep=3pt%\n        \\colorbox{black!15!highlightcolor}{\\color{highlighttextcolor}#1} \\\n        \\colorbox{highlightcolor}{\\color{highlighttextcolor}\\codebox{#2}}%\n        \\end{varwidth}%\n    }\n}\n%% highlighting\n\\newcommand{\\basehighlight}[1]{\\colorbox{highlightcolor}{\\\ncolor{highlighttextcolor}#1}}\n\\newcommand{\\mathhighlight}[1]{\\basehighlight{$#1$}}\n\\newcommand{\\highlight}[1]{\\raisebox{0pt}[-\\fboxsep][-\\fboxsep]{\\\nbasehighlight{#1}}}\n\\newcommand{\\highlightline}[1]{%\\raisebox{0pt}[-\\fboxsep][-\\fboxsep]{\n    \\hspace*{-\\fboxsep}\\basehighlight{#1}%\n%}\n}\n%% bits\n\\newcommand{\\bit}[1]{\\textcolor{bitcolor}{\\texttt{\\upshape #1}}}\n\\newcommand{\\bits}{\\{\\bit0,\\bit1\\}}\n%%%%"
