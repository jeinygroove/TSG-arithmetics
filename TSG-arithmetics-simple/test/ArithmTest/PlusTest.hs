module ArithmTest.PlusTest where

import Util 
import Plus
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

unit_plus1 :: Assertion
unit_plus1 = int plusProg [CONS (ATOM "1") (ATOM ""), CONS (ATOM "1") (ATOM "")] @?= CONS (ATOM "1") (CONS (ATOM "0") (ATOM ""))

unit_plus2 :: Assertion
unit_plus2 = int plusProg [decimalToBinaryExp 12, decimalToBinaryExp 30] @?= (decimalToBinaryExp 42)

unit_plus3 :: Assertion
unit_plus3 = int plusProg [decimalToBinaryExp 12, decimalToBinaryExp 0] @?= (decimalToBinaryExp 12)

unit_plus4 :: Assertion
unit_plus4 = int plusProg [decimalToBinaryExp 0, decimalToBinaryExp 0] @?= (decimalToBinaryExp 0)

unit_plus5 :: Assertion
unit_plus5 = int plusProg [decimalToBinaryExp 0, decimalToBinaryExp 1] @?= (decimalToBinaryExp 1)

unit_plus6 :: Assertion
unit_plus6 = int plusProg [decimalToBinaryExp 0, decimalToBinaryExp 42] @?= (decimalToBinaryExp 42)

unit_plus7 :: Assertion
unit_plus7 = int plusProg [decimalToBinaryExp 120, decimalToBinaryExp 31] @?= (decimalToBinaryExp 151)

unit_plus8 :: Assertion
unit_plus8 = int plusProg [decimalToBinaryExp 31, decimalToBinaryExp 146] @?= (decimalToBinaryExp 177)

unit_plus9 :: Assertion
unit_plus9 = int plusProg [decimalToBinaryExp 100568453, decimalToBinaryExp 23592475285] @?= (decimalToBinaryExp 23693043738)

unit_plus10 :: Assertion
unit_plus10 = int plusProg [decimalToBinaryExp 23529, decimalToBinaryExp 234234] @?= (decimalToBinaryExp 257763)

unit_plus11 :: Assertion
unit_plus11 = int plusProg [decimalToBinaryExp 2048, decimalToBinaryExp 4096] @?= (decimalToBinaryExp 6144)

unit_plus1_ura :: Assertion
unit_plus1_ura = fst (head (ura plusProg ([CONS (ATOM "1") (ATOM ""), CVE 1], RESTR []) (CONS (ATOM "1") (CONS (ATOM "0") (ATOM ""))))) @?= [CVE 1 :-> CONS (ATOM "1") (ATOM "")]

unit_plus2_ura :: Assertion 
unit_plus2_ura = fst (head (ura plusProg ([decimalToBinaryExp 12, CVE 1], RESTR []) (decimalToBinaryExp 42))) @?= [CVE 1 :-> decimalToBinaryExp 30]

unit_plus3_ura :: Assertion 
unit_plus3_ura = fst (head (ura plusProg ([CVE 1, decimalToBinaryExp 12], RESTR []) (decimalToBinaryExp 12))) @?= [CVE 1 :-> decimalToBinaryExp 0]

unit_plus4_ura :: Assertion 
unit_plus4_ura = fst (head (ura plusProg ([CVE 1, decimalToBinaryExp 0], RESTR []) (decimalToBinaryExp 0))) @?= [CVE 1 :-> decimalToBinaryExp 0]

unit_plus5_ura :: Assertion 
unit_plus5_ura = fst (head (ura plusProg ([decimalToBinaryExp 0, CVE 1], RESTR []) (decimalToBinaryExp 1))) @?= [CVE 1 :-> decimalToBinaryExp 1]

unit_plus6_ura :: Assertion 
unit_plus6_ura = fst (head (ura plusProg ([decimalToBinaryExp 0, CVE 1], RESTR []) (decimalToBinaryExp 42))) @?= [CVE 1 :-> decimalToBinaryExp 42]

unit_plus7_ura :: Assertion 
unit_plus7_ura = fst (head (ura plusProg ([CVE 1, decimalToBinaryExp 31], RESTR []) (decimalToBinaryExp 151))) @?= [CVE 1 :-> decimalToBinaryExp 120]

unit_plus8_ura :: Assertion 
unit_plus8_ura = fst (head (ura plusProg ([CVE 1, decimalToBinaryExp 146], RESTR []) (decimalToBinaryExp 177))) @?= [CVE 1 :-> decimalToBinaryExp 31]

unit_plus10_ura :: Assertion 
unit_plus10_ura = fst (head (ura plusProg ([decimalToBinaryExp 529, CVE 1], RESTR []) (decimalToBinaryExp 763))) @?= [CVE 1 :-> decimalToBinaryExp (763 - 529)]

unit_plus11_ura :: Assertion 
unit_plus11_ura = fst (head (ura plusProg ([CVE 1, decimalToBinaryExp 4096], RESTR []) (decimalToBinaryExp 6144))) @?= [CVE 1 :-> decimalToBinaryExp 2048]
