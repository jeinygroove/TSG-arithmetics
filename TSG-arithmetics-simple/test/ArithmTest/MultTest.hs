module ArithmTest.MultTest where

import Util 
import Mult
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

unit_mult1 :: Assertion
unit_mult1 = int multProg [CONS (ATOM "1") (ATOM ""), CONS (ATOM "1") (ATOM "")] @?= CONS (ATOM "1") (ATOM "")

unit_mult2 :: Assertion
unit_mult2 = int multProg [decimalToBinaryExp 12, decimalToBinaryExp 30] @?= (decimalToBinaryExp 360)

unit_mult3 :: Assertion
unit_mult3 = int multProg [decimalToBinaryExp 12, decimalToBinaryExp 0] @?= (decimalToBinaryExp 0)

unit_mult4 :: Assertion
unit_mult4 = int multProg [decimalToBinaryExp 0, decimalToBinaryExp 0] @?= (decimalToBinaryExp 0)

unit_mult5 :: Assertion
unit_mult5 = int multProg [decimalToBinaryExp 0, decimalToBinaryExp 1] @?= (decimalToBinaryExp 0)

unit_mult6 :: Assertion
unit_mult6 = int multProg [decimalToBinaryExp 1, decimalToBinaryExp 42] @?= (decimalToBinaryExp 42)

unit_mult7 :: Assertion
unit_mult7 = int multProg [decimalToBinaryExp 120, decimalToBinaryExp 31] @?= (decimalToBinaryExp 3720)

unit_mult8 :: Assertion
unit_mult8 = int multProg [decimalToBinaryExp 31, decimalToBinaryExp 146] @?= (decimalToBinaryExp 4526)

unit_mult9 :: Assertion
unit_mult9 = int multProg [decimalToBinaryExp 100568453, decimalToBinaryExp 23592475285] @?= (decimalToBinaryExp 2372658741853184105)

unit_mult10 :: Assertion
unit_mult10 = int multProg [decimalToBinaryExp 23529, decimalToBinaryExp 234234] @?= (decimalToBinaryExp 5511291786)

unit_mult11 :: Assertion
unit_mult11 = int multProg [decimalToBinaryExp 2048, decimalToBinaryExp 4096] @?= (decimalToBinaryExp 8388608)

unit_mult1_ura :: Assertion
unit_mult1_ura = fst (head (ura multProg ([CVE 1, CONS (ATOM "1") (ATOM "")], RESTR []) (CONS (ATOM "1") (ATOM "")))) @?= [CVE 1 :-> CONS (ATOM "1") (ATOM "")]

unit_mult2_ura :: Assertion
unit_mult2_ura = fst (head (ura multProg ([decimalToBinaryExp 12, CVE 1], RESTR []) (decimalToBinaryExp 36))) @?= [CVE 1 :-> decimalToBinaryExp 3]

unit_mult3_ura :: Assertion
unit_mult3_ura = fst (head (ura multProg ([decimalToBinaryExp 12, CVE 1], RESTR []) (decimalToBinaryExp 0))) @?= [CVE 1 :-> decimalToBinaryExp 0]

unit_mult4_ura :: Assertion
unit_mult4_ura = fst (head (ura multProg ([decimalToBinaryExp 11, CVE 1], RESTR []) (decimalToBinaryExp 11))) @?= [CVE 1 :-> decimalToBinaryExp 1]