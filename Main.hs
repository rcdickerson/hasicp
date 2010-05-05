module Main
    where

import Data.Maybe as M
import SchemeParser as SP

data Expression = SelfEvaluating PrimitiveValue
                | Variable VarName
                | Quoted Symbol
                | Assignment VarName Expression
                | Definition VarName Expression
                | If Expression Expression Expression
                | Begin [Expression]
                | Cond [(Expression, Expression)]
                | PrimitiveProcedure ([PrimitiveValue] -> PrimitiveValue)
                | ComplexProcedure [VarName] [Expression] Environment
                | Application Expression [Expression]

instance Show Expression where
    show (SelfEvaluating val) = show val
    show (Variable name) = "Var: " ++ name
    show (Quoted symbol) = "'" ++ symbol
    show (Assignment name exp) = "Assignment: " ++ name ++ (show exp)
    show (Definition name exp) = "Definition: " ++ name ++ (show exp)
    show (If _ _ _) = "<if-statement>"
    show (Begin _) = "<begin>"
    show (Cond _) = "<conditional>"
    show (PrimitiveProcedure _) = "<primitive proc>"
    show (ComplexProcedure _ _ _) = "<complex proc>"
    show (Application _ _) = "<application>"

data PrimitiveValue = PrimNum Double
                    | PrimString String
                    | PrimBool Bool

instance Show PrimitiveValue where
    show (PrimNum d) = show d
    show (PrimString s) = s
    show (PrimBool True) = "true"
    show (PrimBool False) = "false"

type VarName = String
type Symbol = String

eval :: Expression -> Environment -> Expression
eval (SelfEvaluating val) _ = (SelfEvaluating val)
eval (Variable name) env = lookupBinding name env
eval (Quoted name) _ = SelfEvaluating (PrimString name)
--eval (Assignment var val) = evalAssignment var val
--eval (Definition var val) env = evalDefinition var val env
eval (If cond consq alt) env = evalIf cond consq alt env
eval (Begin exps) env = evalSequence exps env
--eval (Cond conds) = eval (condsToIf conds)
eval (Application operator operands) env = apply operator vals
    where
      vals = (listOfValues operands env)
eval _ _ = error "Unknown expression type -- EVAL"

evalInEnv :: Environment -> (Expression -> Expression)
evalInEnv = flip eval

apply :: Expression -> [Expression] -> Expression
apply (PrimitiveProcedure p) argExps = SelfEvaluating (p args)
    where 
      args = map toPrimVal argExps
      toPrimVal (SelfEvaluating val) = val
apply (ComplexProcedure params proc env) args = evalSequence proc newEnv
    where
      newEnv = extendEnvironment (zip params args) env

listOfValues :: [Expression] -> Environment -> [Expression]
listOfValues exps env = map (evalInEnv env) exps

evalIf :: Expression -> Expression -> Expression -> Environment -> Expression
evalIf cond consq alt env = if (expToBool.(evalInEnv env) $ cond)
                                then (eval consq env)
                                else (eval alt env)
    where
      expToBool (SelfEvaluating (PrimBool tOrF)) = tOrF

evalSequence :: [Expression] -> Environment -> Expression
evalSequence [] _ = Quoted "ok"
evalSequence (exp:[]) env = eval exp env
evalSequence (exp:exps) env = evalSequence exps env -- todo: fix

--evalAssignment :: VarName -> Expression -> Environment -> Expression
--evalAssignment var exp env = setVariableValue var (eval exp env) env

--evalDefinition :: VarName -> Expression -> Environment -> Expression
--evalDefinition var exp env = defineVariable var (expToBindable (eval exp env)) env


-- Environment --

type Binding = (VarName, Expression)
type Frame = [Binding]
type Environment = [Frame]

emptyEnvironment = []

lookupBinding :: VarName -> Environment -> Expression
lookupBinding name env = head $ M.mapMaybe (lookup name) env

extendEnvironment :: Frame -> Environment -> Environment
extendEnvironment = (:)

defineVariable :: VarName -> Expression -> Environment -> Environment 
defineVariable name val (f:fs) = ((name, val):f) : fs

--setVariableValue :: VarName -> Expression -> Environment -> Environment
--setVariableValue name val env =


-- Global Environment --
plus :: [PrimitiveValue] -> PrimitiveValue
plus ((PrimNum val1):(PrimNum val2):[]) = PrimNum (val1 + val2)

minus :: [PrimitiveValue] -> PrimitiveValue
minus ((PrimNum val1):(PrimNum val2):[]) = PrimNum (val1 - val2)

lt :: [PrimitiveValue] -> PrimitiveValue
lt ((PrimNum val1):(PrimNum val2):[]) = PrimBool (val1 < val2)

gt :: [PrimitiveValue] -> PrimitiveValue
gt ((PrimNum val1):(PrimNum val2):[]) = PrimBool (val1 > val2)

globalBindings :: [Binding]
globalBindings = [("+", PrimitiveProcedure plus),
                  ("-", PrimitiveProcedure minus),
                  ("<", PrimitiveProcedure lt),
                  (">", PrimitiveProcedure gt)]

globalEnvironment :: Environment
globalEnvironment = extendEnvironment globalBindings emptyEnvironment


-- REPL --

repl :: IO ()
repl = do
  putStr "> "
  input <- getLine
  if (input == "quit")
     then return ()
     else handleInput input

handleInput :: String -> IO ()
handleInput input = do

  let parsedExp = buildExpression $ SP.parseSexp input
  let evaledExp = eval parsedExp globalEnvironment
  let valueToShow = show evaledExp

  putStrLn $ valueToShow
  repl

buildExpression :: SP.SchemeVal -> Expression
buildExpression (SP.SchemeAtom var) = 
    lookupBinding var globalEnvironment
buildExpression (SP.SchemeNumber num) = 
    SelfEvaluating $ PrimNum num
buildExpression (SP.SchemeBool bool) =
    SelfEvaluating $ PrimBool bool
buildExpression (SP.SchemeString str) =
    SelfEvaluating $ PrimString str
buildExpression (SP.SchemeList ((SchemeAtom op):params)) = 
    case op of
      "if" -> If cond consq alt 
          where (cond:consq:alt:[]) = map buildExpression params
      "begin" -> Begin $ map buildExpression params
      otherwise -> Application proc (map buildExpression params)
          where proc = lookupBinding op globalEnvironment
