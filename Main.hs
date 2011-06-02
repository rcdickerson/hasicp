module Main
    where

import Data.IORef

import Data.Maybe as M
import SchemeParser as SP
import Control.Monad as CM
import Text.ParserCombinators.Parsec as P

type EnvRef = IORef Environment

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
                | ComplexProcedure [VarName] [Expression] EnvRef
                | Application Expression [Expression]
                | Error P.ParseError

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
    show (Error parseError) = show parseError

data PrimitiveValue = PrimNum Double
                    | PrimString String
                    | PrimBool Bool

instance Show PrimitiveValue where
    show (PrimNum d) = show d
    show (PrimString s) = s
    show (PrimBool True) = "true"
    show (PrimBool False) = "false"

expToBool (SelfEvaluating (PrimBool tOrF)) = tOrF

type VarName = String
type Symbol = String

eval :: Expression -> EnvRef -> IO Expression
eval se@(SelfEvaluating val) _ = return se
eval (Variable name) envRef = do
    env <- readIORef envRef
    return $ lookupBinding name env
eval qu@(Quoted name) _ = return qu
eval (Assignment var val) envRef = evalAssignment var val envRef
eval (Definition var val) envRef = evalDefinition var val envRef
eval (If cond consq alt) envRef = evalIf cond consq alt envRef
eval (Lambda params body) envRef = 
    return $ ComplexProcedure params body envRef
eval (Begin exps) envRef = evalSequence exps envRef
--eval (Cond conds) = eval (condsToIf conds)
eval (Application operator operands) env = do
    vals <- CM.mapM (evalInEnv env) operands
    apply operator vals env
eval (Error parseError) _ = return (Error parseError)
eval _ _ = error "Unknown expression type -- EVAL"

evalInEnv :: EnvRef -> Expression -> IO Expression
evalInEnv = flip eval

apply :: Expression -> [Expression] -> EnvRef -> IO Expression
apply (PrimitiveProcedure p) argExps _ = return $ SelfEvaluating (p args)
    where 
      args = map toPrimVal argExps
      toPrimVal (SelfEvaluating val) = val
apply (ComplexProcedure params proc envRef) args _ = do
    env <- readIORef envRef
    extEnv <- newIORef $ extendEnvironment (zip params args) env
    evalSequence proc extEnv

evalIf :: Expression -> Expression -> Expression -> EnvRef -> IO Expression
evalIf cond consq alt envRef = do
    result <- eval cond envRef 
    if (expToBool result)
        then (eval consq envRef)
        else (eval alt envRef)

evalSequence :: [Expression] -> EnvRef -> IO Expression
evalSequence [] _ = return $ Quoted "ok"
evalSequence (exp:[]) envRef = eval exp envRef
evalSequence (exp:exps) envRef = evalSequence exps envRef

evalAssignment :: VarName -> Expression -> EnvRef -> IO Expression
evalAssignment var exp envRef = do
    value <- eval exp envRef
    modifyIORef envRef (setVariable var value)
    let message = "assigned: " ++ var ++ " = " ++ (show value)
    return $ Quoted message

evalDefinition :: VarName -> Expression -> EnvRef -> IO Expression
evalDefinition var exp envRef = do
    value <- eval exp envRef
    modifyIORef envRef (defineVariable var value)
    let message = "defined: " ++ var ++ " = " ++ (show value)
    return $ Quoted message


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

setVariable :: VarName -> Expression -> Environment -> Environment
setVariable name val frames = setVariable' [] frames (name, val)
  where
    setVariable' _ [] (name, _) = error $ "Undefined: " ++ name
    setVariable' pre (f:fs) binding = 
        case modifyBinding f binding of
            (Just newFrame) -> pre ++ (newFrame:fs)
            _ -> setVariable' (pre ++ [f]) fs binding

modifyBinding :: Frame -> Binding -> Maybe Frame
modifyBinding = modifyBinding' []
  where
    modifyBinding' _ [] _ = Nothing
    modifyBinding' pre (b@(bName, _):bs) binding@(name, value)
                  | bName == name = Just $ pre ++ (binding : bs)
                  | otherwise = modifyBinding' (pre ++ [b]) bs binding


-- Global Environment --
numBinop :: (Double -> Double -> Double) -> Expression
numBinop op = PrimitiveProcedure proc
    where proc ((PrimNum val1):(PrimNum val2):[]) = 
              PrimNum $ op val1 val2

numBoolBinop :: (Double -> Double -> Bool) -> Expression
numBoolBinop op = PrimitiveProcedure proc
    where proc ((PrimNum val1):(PrimNum val2):[]) = 
              PrimBool $ op val1 val2

boolBinop :: (Bool -> Bool -> Bool) -> Expression
boolBinop op = PrimitiveProcedure proc
    where proc ((PrimBool val1):(PrimBool val2):[]) = 
              PrimBool $ op val1 val2

globalBindings :: [Binding]
globalBindings = [("+", numBinop (+)),
                  ("-", numBinop (-)),
                  ("*", numBinop (*)),
                  ("/", numBinop (/)),
                  ("<", numBoolBinop (<)),
                  (">", numBoolBinop (>)),
                  ("=", numBoolBinop (==)),
                  ("||", boolBinop (||)),
                  ("&&", boolBinop (&&))]

globalEnvironment :: Environment
globalEnvironment = extendEnvironment globalBindings emptyEnvironment


-- REPL --

repl :: IO()
repl = do
    globalEnvRef <- newIORef globalEnvironment
    acceptInput globalEnvRef

acceptInput :: EnvRef -> IO ()
acceptInput env = do
  putStr "> "
  input <- getLine
  if (input == "quit")
     then return ()
     else handleInput input env

handleInput :: String -> EnvRef -> IO()
handleInput input envRef = do

  env <- readIORef envRef

  let parsedExp = maybeBuildExpression env (SP.parseSexp input)
  evaledExp <- eval parsedExp envRef

  putStrLn $ show evaledExp
  acceptInput envRef

maybeBuildExpression :: Environment -> Either P.ParseError SP.SchemeVal -> Expression
maybeBuildExpression _ (Left parseError) = Error parseError
maybeBuildExpression env (Right schemeVal) = buildExpression env schemeVal

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
      "set!" -> Assignment var val
          where ((Variable var):val:[]) = map (buildExpression env) params
      "lambda" -> Lambda paramNames (map (buildExpression env) body)
          where
            ((SP.SchemeList paramList):body) = params
            extractVarName (SP.SchemeAtom var) = var
            paramNames = map extractVarName (paramList)
      otherwise -> Application proc (map (buildExpression env) params)
          where proc = lookupBinding op env
