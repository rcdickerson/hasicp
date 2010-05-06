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
                | Lambda [VarName] [Expression]
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
    show (Lambda _ _) = "<lambda>"
    show (Begin _) = "<begin>"
    show (Cond _) = "<conditional>"
    show (PrimitiveProcedure _) = "<primitive procedure>"
    show (ComplexProcedure n exp env) = "<complex procedure>"
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

type Evaluation = (Expression, Environment)

eval :: Expression -> Environment -> Evaluation
eval (SelfEvaluating val) env = (SelfEvaluating val, env)
eval (Variable name) env = (lookupBinding name env, env)
eval (Quoted name) env = (Quoted name, env)
--eval (Assignment var val) = evalAssignment var val
eval (Definition var val) env = evalDefinition var val env
eval (If cond consq alt) env = evalIf cond consq alt env
eval (Lambda params body) env = (ComplexProcedure params body env, env)
eval (Begin exps) env = evalSequence exps env
--eval (Cond conds) = eval (condsToIf conds)
eval (Application operator operands) env = apply operator vals env
    where vals = map (fst.(evalInEnv env)) operands
eval _ _ = error "Unknown expression type -- EVAL"

evalInEnv :: Environment -> Expression -> Evaluation
evalInEnv = flip eval

apply :: Expression -> [Expression] -> Environment -> Evaluation
apply (PrimitiveProcedure p) argExps env = (SelfEvaluating (p args), env)
    where 
      args = map toPrimVal argExps
      toPrimVal (SelfEvaluating val) = val
apply (ComplexProcedure params proc _) args env = evalSequence proc newEnv
    where
      newEnv = extendEnvironment (zip params args) env

evalIf :: Expression -> Expression -> Expression -> Environment -> Evaluation
evalIf cond consq alt env = if (expToBool.(fst.(evalInEnv env)) $ cond)
                                then (eval consq env)
                                else (eval alt env)
    where expToBool (SelfEvaluating (PrimBool tOrF)) = tOrF

evalSequence :: [Expression] -> Environment -> Evaluation
evalSequence [] env = (Quoted "ok", env)
evalSequence (exp:[]) env = eval exp env
evalSequence (exp:exps) env = evalSequence exps newEnv
    where (_, newEnv) = eval exp env

--evalAssignment :: VarName -> Expression -> Environment -> Expression
--evalAssignment var exp env = setVariableValue var (eval exp env) env

evalDefinition :: VarName -> Expression -> Environment -> Evaluation
evalDefinition var exp env = (Quoted message, defineVariable var value newEnv)
    where 
      (value, newEnv) = eval exp env
      message =  "defined: " ++ var ++ " = " ++ (show value)


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

divide :: [PrimitiveValue] -> PrimitiveValue
divide ((PrimNum _):(PrimNum 0):[]) = error "Divide by zero."
divide ((PrimNum val1):(PrimNum val2):[]) = PrimNum (val1 / val2)

lt :: [PrimitiveValue] -> PrimitiveValue
lt ((PrimNum val1):(PrimNum val2):[]) = PrimBool (val1 < val2)

gt :: [PrimitiveValue] -> PrimitiveValue
gt ((PrimNum val1):(PrimNum val2):[]) = PrimBool (val1 > val2)

eq :: [PrimitiveValue] -> PrimitiveValue
eq ((PrimNum val1):(PrimNum val2):[]) = PrimBool (val1 == val2)

globalBindings :: [Binding]
globalBindings = [("+", PrimitiveProcedure plus),
                  ("-", PrimitiveProcedure minus),
                  ("/", PrimitiveProcedure divide),
                  ("<", PrimitiveProcedure lt),
                  (">", PrimitiveProcedure gt),
                  ("=", PrimitiveProcedure eq)]

globalEnvironment :: Environment
globalEnvironment = extendEnvironment globalBindings emptyEnvironment


-- REPL --

repl :: IO()
repl = acceptInput globalEnvironment

acceptInput :: Environment -> IO ()
acceptInput env = do
  putStr "> "
  input <- getLine
  if (input == "quit")
     then return ()
     else handleInput input env

handleInput :: String -> Environment -> IO()
handleInput input env = do

  let parsedExp = buildExpression env (SP.parseSexp input)
  let (evaledExp, newEnv) = eval parsedExp env
  let valueToShow = show evaledExp

  putStrLn valueToShow  
  acceptInput newEnv

buildExpression :: Environment -> SP.SchemeVal -> Expression
buildExpression _ (SP.SchemeAtom var)   = Variable var
buildExpression _ (SP.SchemeNumber num) = SelfEvaluating $ PrimNum num
buildExpression _ (SP.SchemeBool bool)  = SelfEvaluating $ PrimBool bool
buildExpression _ (SP.SchemeString str) = SelfEvaluating $ PrimString str
buildExpression env (SP.SchemeList ((SchemeAtom op):params)) = 
    case op of
      "if" -> If cond consq alt 
          where (cond:consq:alt:[]) = map (buildExpression env) params
      "begin" -> Begin $ map (buildExpression env) params
      "define" -> Definition var val
          where ((Variable var):val:[]) = map (buildExpression env) params
      "lambda" -> Lambda paramNames (map (buildExpression env) body)
          where
            ((SP.SchemeList paramList):body) = params
            extractVarName (SP.SchemeAtom var) = var
            paramNames = map extractVarName (paramList)
      otherwise -> Application proc (map (buildExpression env) params)
          where proc = lookupBinding op env
