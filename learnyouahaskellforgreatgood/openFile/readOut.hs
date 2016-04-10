import System.IO

main = do
  handle <- openFile "text.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  putStr "heeh"
  hClose handle
