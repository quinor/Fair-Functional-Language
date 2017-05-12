import Interpreter.Defs
import Interpreter.Eval
import Interpreter.Primitives
import Interpreter.Typecheck


run_test = if sum_test == 3 && let_test == 42 && lambda_test == 13 && if_test == 42 && fact_test == 120
  then True
  else False

DInt sum_test = run_program sum_exp
DInt let_test = run_program let_exp
DInt lambda_test = run_program lambda_exp
DInt if_test = run_program if_exp
DInt fact_test = run_program fact_exp




sum_exp = (EApply (EApply (EData $ DPrimitive $ p_add) (EData $ DInt 1)) (EData $ DInt 2))

let_exp = ELet "num"
 (EData $ DInt 42)
 (EVar "num")

lambda_exp = ELet "add_one"
  (ELambda "n"
    (EApply (EApply (EData $ DPrimitive $ p_add) (EData $ DInt 1)) (EVar "n"))
  )
  (EApply (EVar "add_one") (EData $ DInt 12))

if_exp = (ELet "if")
  (ELambda "b"
    (EApply (EApply (EApply (EData $ DPrimitive $ p_if)
      (EVar "b"))
      (EData $ DInt 42))
      (EData $ DInt 13))
  )
  (EApply (EVar "if") (EData $ DBool True))

fact_exp = ELetRec "factorial"
  (ELambda "n"
    (EApply (EApply (EApply (EData $ DPrimitive $ p_if)
      (EApply (EApply (EData $ DPrimitive $ p_eq) (EData $ DInt 0)) (EVar "n")))
      (EData $ DInt 1))
      (EApply (EApply (EData $ DPrimitive $ p_mul)
        (EVar "n"))
        (EApply
          (EVar "factorial")
          ((EApply (EApply (EData $ DPrimitive $ p_sub) (EVar "n")) (EData $ DInt 1)))
        )
      )
    )
  )
  (EApply (EVar "factorial") (EData $ DInt 5))
