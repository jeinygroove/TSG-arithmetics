module Mult where
import Util

mult :: FDef
mult = 
  DEFINE "mult" [e_n1, e_n2]
    (ALT (CONS' e_n1 e_n1h e_n1t a_n1)
      (ALT (EQA' e_n1h zero)
        (ALT (CONS' e_n1t e_n1th e_n1tt a_n1t)
          (RETURN failure) -- forbid leading zeroes (if it's not CONS zero empty)
          (CALL "mult2'" [e_n1, e_n2])
        )
        (CALL "mult2'" [e_n1, e_n2])
      )
      (RETURN failure) -- number can not be empty atom
    )
  where
    e_n1  = PVE "n1"
    e_n2  = PVE "n2"
    e_n1h = PVA "n1_head"
    e_n1t = PVE "n1_tail"
    e_n1th = PVA "n1_tail_head"
    e_n1tt = PVE "n1_tail_tail"
    a_n1  = PVA "a_n1"
    a_n1t = PVA "a_nt1"
    failure = ATOM "invalid first number"

mult2' :: FDef 
mult2' =
  DEFINE "mult2'" [e_n1, e_n2] 
    (ALT (CONS' e_n2 e_n2h e_n2t a_n2)
      (ALT (EQA' e_n2h zero)
        (ALT (CONS' e_n2t e_n2th e_n2tt a_n2t)
          (RETURN failure) -- forbid leading zeroes (if it's not CONS zero empty)
          (CALL "mult'" [e_n1, e_n2, empty, empty]) -- numbers don't have leading zeroes and are not empty, let's mult them
        )
        (CALL "mult'" [e_n1, e_n2, empty, empty]) -- numbers don't have leading zeroes and are not empty, let's mult them
      )
      (RETURN failure) -- number can not be empty atom
    )
  where
    e_n1  = PVE "n1"
    e_n2  = PVE "n2"
    e_n2h = PVA "n2_head"
    e_n2t = PVE "n2_tail"
    e_n2th = PVA "n2_tail_head"
    e_n2tt = PVE "n2_tail_tail"
    a_n2  = PVA "a_n2"
    a_n2t = PVA "a_nt2"
    failure = ATOM "invalid second number"

{-|
  e_a_shifted - reversed first number, which is shifted according to column addition
  e_b_rest - "operations" for addition (if head is 0, there's nothing to add, if 1 -- we need to add e_a_shifted,
     in both cases shift e_a_shifted for next operations)
  e_a_add - reversed number for sum (analogous to e_a in plusR) (new row in column addition)
  e_res - current result from column addition, to which we add new row
  a_rem - reminder (analogous to e_b in plusR)
  e_new_res - result of addition (analogous to e_res in plusR)
-}
multR :: FDef
multR =    
  DEFINE "multR" [e_a_shifted, e_b_rest, e_a_add, e_res, a_rem, e_new_res]
    (ALT (CONS' e_a_add e_ah e_at a_a)
      (ALT (CONS' e_res e_rh e_rt a_r) -- addition branch
        (ALT (EQA' e_ah zero)
          (ALT (EQA' e_rh zero)
            (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, zero, CONS a_rem e_new_res])
            (ALT (EQA' e_rh one)
              (ALT (EQA' a_rem zero)
                (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, a_rem, CONS one e_new_res])
                (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, one, CONS zero e_new_res])
              )
              (RETURN failure)
            )
          )
          (ALT (EQA' e_ah one)
            (ALT (EQA' e_rh zero)
              (ALT (EQA' a_rem zero)
                (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, a_rem, CONS one e_new_res])
                (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, one, CONS zero e_new_res])
              )
              (ALT (EQA' e_rh one)
                (ALT (EQA' a_rem zero)
                  (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, one, CONS zero e_new_res])
                  (CALL "multR" [e_a_shifted, e_b_rest, e_at, e_rt, one, CONS one e_new_res])
                )
                (RETURN failure)
              )
            )
            (RETURN failure)
          )
        )
        (ALT (EQA' e_ah zero)
          (ALT (EQA' a_rem zero)
            (CALL "multR" [e_a_shifted, e_b_rest, e_at, empty, zero, CONS zero e_new_res])
            (CALL "multR" [e_a_shifted, e_b_rest, e_at, empty, zero, CONS one e_new_res])
          )
          (ALT (EQA' e_ah one)
            (ALT (EQA' a_rem zero)
              (CALL "multR" [e_a_shifted, e_b_rest, e_at, empty, zero, CONS one e_new_res])
              (CALL "multR" [e_a_shifted, e_b_rest, e_at, empty, one, CONS zero e_new_res])
            )
            (RETURN failure)
          )
        )
      )
      (ALT (CONS' e_res e_rh e_rt a_r)
        (CALL "multR" [e_a_shifted, e_b_rest, e_res, e_a_add, a_rem, e_new_res]) -- swap if e_res is longer and continue summation
        (ALT (EQA' a_rem zero) -- addition has finished, add reminder and continue multiplication
          (ALT (CONS' e_b_rest e_bh e_bt a_b)
            (ALT (EQA' e_bh zero)
              (CALL "multR" [CONS zero e_a_shifted, e_bt, empty, empty, zero, e_new_res]) -- nothing to add, just shift
              (ALT (EQA' e_bh one)
                (CALL "multR'" [CONS zero e_a_shifted, e_bt, CONS zero e_a_shifted, e_new_res, empty, zero, empty]) -- shift and add shifted e_a to e_new_res (and don't forget to reverse e_new_res)
                (RETURN failure)
              )
            )
            (ALT (CONS' e_new_res e_nrh e_nrt a_nr) -- no more operations, check if e_new_res is empty (it means that e_a or e_b = 0)
              (RETURN e_new_res)
              (RETURN (CONS zero empty))
            )
          )
          (ALT (CONS' e_b_rest e_bh e_bt a_b)
            (ALT (EQA' e_bh zero)
              (CALL "multR" [CONS zero e_a_shifted, e_bt, empty, empty, zero, CONS one e_new_res]) -- add reminder and shift, nothing to add
              (ALT (EQA' e_bh one)
                (CALL "multR'" [CONS zero e_a_shifted, e_bt, CONS zero e_a_shifted, CONS one e_new_res, empty, zero, empty]) -- add reminder, shift and add shifted e_a to e_new_res (and don't forget to reverse e_new_res)
                (RETURN failure)
              )
            )
            (RETURN (CONS one e_new_res)) -- no more operations, return result
          )
        )
      )
    )
  where
    e_a_shifted = PVE "a"
    e_a_add = PVE "a_for_addition"
    e_b_rest = PVE "b"
    a_rem = PVA "atom_reminder"
    e_res = PVE "current_result"
    e_new_res = PVE "result"
    e_ah = PVE "a_head_"
    e_at = PVE "a_tail_"
    e_bh = PVE "b_head_"
    e_bt = PVE "b_tail_"
    e_rh = PVE "res_head_"
    e_rt = PVE "res_tail_"
    e_nrh = PVE "nres_head_"
    e_nrt = PVE "nres_tail_"
    a_a = PVA "atom_a"
    a_b = PVA "atom_b"
    a_r = PVA "atom_res"
    a_nr = PVA "atom_nres"

{-|
  Reverses result of current addition for multR
-}
multR' :: FDef 
multR' = 
    DEFINE "multR'" [e_a_shifted, e_b_rest, e_a_add, e_res_tail, e_res_head_r, a_rem, e_new_res]
      (ALT (CONS' e_res_tail e_rh e_rt a_r)
        (CALL "multR'" [e_a_shifted, e_b_rest, e_a_add, e_rt, CONS e_rh e_res_head_r, a_rem, e_new_res])
        (CALL "multR" [e_a_shifted, e_b_rest, e_a_add, e_res_head_r, a_rem, e_new_res])
      )
    where
        e_a_shifted = PVE "a"
        e_a_add = PVE "a_for_addition"
        e_b_rest = PVE "b"
        a_rem = PVA "atom_reminder"
        e_res_tail = PVE "current_result_tail"
        e_res_head_r = PVE "current_result_head_r"
        e_new_res = PVE "result"
        e_rh = PVE "res_head_"
        e_rt = PVE "res_tail_"
        a_r = PVA "atom_res"
{-|
  Reverses numbers and calls multR
-}
mult' :: FDef
mult' =
  DEFINE "mult'" [e_tail_a, e_tail_b, e_head_r_a, e_head_r_b]
    (ALT (CONS' e_tail_a e_ah e_at a_a)
      (CALL "mult'" [e_at, e_tail_b, CONS e_ah e_head_r_a, e_head_r_b])  -- reverse first number
      (ALT (EQA' a_a empty)
        (ALT (CONS' e_tail_b e_bh e_bt a_b) 
          (CALL "mult'" [e_tail_a, e_bt, e_head_r_a, CONS e_bh e_head_r_b]) -- reverse second number
          (ALT (EQA' a_b empty)
            (ALT (CONS' e_head_r_b e_bh e_bt a_b)
              (ALT (EQA' e_bh zero)
                (CALL "multR" [e_head_r_a, e_bt, empty, empty, zero, empty]) -- if the last digit in e_b is zero than nothing to sum
                (ALT (EQA' e_bh one)
                  (CALL "multR" [e_head_r_a, e_bt, e_head_r_a, zero, zero, empty]) -- if the last digit in e_b is one than we need to add e_head_r_a and zero
                  (RETURN (ATOM "invalid atom in second number list"))
                )
              )
              (RETURN (ATOM "number can not be empty"))
            )
            (RETURN (ATOM "last atom in second number list must be empty"))
          )
        )
        (RETURN (ATOM "last atom in first number list must be empty"))
      )
    )
  where
    e_tail_a = PVE "a_tail__"
    e_tail_b = PVE "b_tail__"
    e_head_r_a = PVE "a_head_r"
    e_head_r_b = PVE "b_head_r"
    e_ah = PVA "a_head_"
    e_at = PVE "a_tail_"
    e_bh = PVA "b_head_"
    e_bt = PVE "b_tail_"
    a_a = PVA "atom_a_"
    a_b = PVA "atom_b_"

multProg = [mult, mult', mult2', multR, multR'] :: Prog 
