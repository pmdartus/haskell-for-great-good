main = interact respondPalendromes

respondPalendromes = unlines . map (\xs -> if isPalendrome xs 
  then "palindrome" else "not palendrome") . lines
    where isPalendrome xs = xs == reverse xs
