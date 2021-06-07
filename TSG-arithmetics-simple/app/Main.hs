module Main where
import Plus
import Mult
import Util
import Lib

main :: IO ()
main = putStrLn $ show $ ura' plusProg ([CVE 1, CONS one (CONS zero (CONS one empty))], RESTR []) (CONS one (CONS one (CONS zero empty)))
