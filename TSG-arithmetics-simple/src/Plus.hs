module Plus where
import Util

plus :: FDef
plus = 
  DEFINE "plus" [e_n1, e_n2]
    (ALT (CONS' e_n1 e_n1h e_n1t a_n1)
      (ALT (EQA' e_n1h zero)
        (ALT (CONS' e_n1t e_n1th e_n1tt a_n1t)
          (RETURN failure) -- forbid leading zeroes (if it's not CONS zero empty)
          (CALL "plus2'" [e_n1, e_n2])
        )
        (CALL "plus2'" [e_n1, e_n2])
      )
      (RETURN failure)
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

plus2' :: FDef 
plus2' =
  DEFINE "plus2'" [e_n1, e_n2] 
    (ALT (CONS' e_n2 e_n2h e_n2t a_n2)
      (ALT (EQA' e_n2h zero)
        (ALT (CONS' e_n2t e_n2th e_n2tt a_n2t)
          (RETURN failure) -- forbid leading zeroes (if it's not CONS zero empty)
          (CALL "plus'" [e_n1, e_n2, empty, empty])
        )
        (CALL "plus'" [e_n1, e_n2, empty, empty])
      )
      (RETURN failure)
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

plusR :: FDef
plusR = 
  DEFINE "plusR" [e_a, e_b, a_rem, e_res]
    (ALT (CONS' e_a e_ah e_at a_a)
      (ALT (CONS' e_b e_bh e_bt a_b)
        (ALT (EQA' e_ah zero)
          (ALT (EQA' e_bh zero)
            (CALL "plusR" [e_at, e_bt, zero, CONS a_rem e_res])
            (ALT (EQA' e_bh one)
              (ALT (EQA' a_rem zero)
                (CALL "plusR" [e_at, e_bt, a_rem, CONS one e_res])
                (CALL "plusR" [e_at, e_bt, one, CONS zero e_res])
              )
              (RETURN failure)
            )
          )
          (ALT (EQA' e_ah one)
            (ALT (EQA' e_bh zero)
              (ALT (EQA' a_rem zero)
                (CALL "plusR" [e_at, e_bt, a_rem, CONS one e_res])
                (CALL "plusR" [e_at, e_bt, one, CONS zero e_res])
              )
              (ALT (EQA' e_bh one)
                (ALT (EQA' a_rem zero)
                  (CALL "plusR" [e_at, e_bt, one, CONS zero e_res])
                  (CALL "plusR" [e_at, e_bt, one, CONS one e_res])
                )
                (RETURN failure) 
              )
            )
            (RETURN failure)
          )
        )
        (ALT (EQA' e_ah zero)
          (ALT (EQA' a_rem zero)
            (CALL "plusR" [e_at, empty, zero, CONS zero e_res])
            (CALL "plusR" [e_at, empty, zero, CONS one e_res])
          )
          (ALT (EQA' e_ah one)
            (ALT (EQA' a_rem zero)
              (CALL "plusR" [e_at, empty, zero, CONS one e_res])
              (CALL "plusR" [e_at, empty, one, CONS zero e_res])
            )
            (RETURN failure)
          )
        )
      )
      (ALT (CONS' e_b e_bh e_bt a_b)
        (CALL "plusR" [e_b, e_a, a_rem, e_res])
        (ALT (EQA' a_rem zero)
          (ALT (CONS' e_res e_rh e_rt a_r)
            (RETURN e_res)
            (RETURN failure)
          )
          (RETURN (CONS one e_res))
        )
      )
    )
  where
    e_a = PVE "a"
    e_b = PVE "b"
    a_rem = PVA "atom_reminder"
    e_res = PVE "result"
    e_ah = PVA "a_head"
    e_at = PVE "a_tail"
    e_bh = PVA "b_head"
    e_bt = PVE "b_tail"
    e_rh = PVA "r_head"
    e_rt = PVE "r_tail"
    a_a = PVA "atom_a"
    a_b = PVA "atom_b"
    a_r = PVA "atom_r"
    failure = ATOM "invalid number"

plus' :: FDef
plus' =
  DEFINE "plus'" [e_tail_a, e_tail_b, e_head_r_a, e_head_r_b]
    (ALT (CONS' e_tail_a e_ah e_at a_a)
      (CALL "plus'" [e_at, e_tail_b, CONS e_ah e_head_r_a, e_head_r_b])  -- reverse first number
      (ALT (EQA' a_a empty)
        (ALT (CONS' e_tail_b e_bh e_bt a_b) 
          (CALL "plus'" [e_tail_a, e_bt, e_head_r_a, CONS e_bh e_head_r_b]) -- reverse second number
          (ALT (EQA' a_b empty)
            (CALL "plusR" [e_head_r_a, e_head_r_b, zero, empty]) -- call plusR on reversed numbers
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

plusProg = [plus, plus', plus2', plusR] :: Prog