data BinOp = Add | Sub | Mul | Div | GTh | LTh
        deriving Show

data Expression 
   = Number Int 
   | Variable String
   | Composite Expression BinOp Expression
        deriving Show

data Statement 
  = If Expression Statement Statement                -- If Stmt
	| While Expression Statement                       -- While loop
	| Compound [Statement]                             -- Sequence
  | Assign String Expression                           -- "a" = 5
	    deriving Show

type Environment = [([Char], Int)]

value :: Expression -> [([Char],Int)] -> Int
value (Number x) _ = x
value (Composite e1 op e2) env = aux op (value e1 env) (value e2 env)
  where aux Add v1 v2 = v1 + v2                      -- 4 + 2
        aux Sub v1 v2 = v1 - v2                      -- 4 - 2
        aux Mul v1 v2 = v1 * v2                      -- 4 * 2
        aux Div v1 v2 = v1 `div` v2                  -- 4 / 2
        aux GTh v1 v2 | v1>v2 = 1                    -- 4 > 2 = 1
                      | otherwise = 0                -- 1 > 2 = 0
        aux LTh v1 v2 | v1<v2 = 1                    -- 1 < 2 = 1
                      | otherwise = 0                -- 4 < 2 = 0

-- This will now lookup in the tree environment instead...
-- ((n,v):r) will be changed to use retrieve.
value (Variable name) ((n,v):r) =
	  if name == n then v 
	  else value (Variable name) r
	
-- Modify assign here to use insert.
eval :: Statement -> Environment -> Environment
eval (Assign x expr) env = (x, value expr env) : env

-- if value expr r != 0 then eval (Compound [While expr stmt])
-- otherwise continue until condition returns false
eval (While expr stmt) env 
                     | value expr env /= 0 
                       = eval (Compound [stmt, While expr stmt]) env
                     | otherwise = env
eval (Compound []) env = env
eval (Compound (stmt : stmts)) env 
    = eval (Compound stmts) (eval stmt env)

-- Programs for testing...
fact = (Compound 
  [
    (Assign "n" (Number 7)), 
    (Assign "fact" (Number 1)), 
    (While (Composite (Variable "n") GTh (Number 1)) 
      (Compound
        [
          (Assign "fact" (Composite (Variable "fact") Mul (Variable "n"))),
          (Assign "n" (Composite (Variable "n") Sub (Number 1)))
        ]
      )
    )
  ]
  )

twoToN = (Compound
  [
    (Assign "result" (Number 2)),
    (Assign "pow" (Number 5)),
    (Assign "z" (Number 1)),
    (While (Composite (Variable "z") LTh (Variable "pow"))
      (Compound
        [
          (Assign "result" (Composite (Variable "result") Mul (Number 2))),
          (Assign "z" (Composite (Variable "z") Add (Number 1)))
        ]
      )
    )
  ]
  )
