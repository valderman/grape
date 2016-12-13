{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | Code generation for expressions and statements.
module CG where
import Control.Monad.State
import Data.Monoid
import Exp
import Stm
import Pat hiding (Exp, Alg (..))

data Env = Env
  { envNameSupply :: Name
  , envStatements :: [String]
  }

emptyEnv :: Env
emptyEnv = Env
  { envNameSupply = 0
  , envStatements = mempty
  }

-- | Emit a statement.
emit :: String -> CGM ()
emit stm = modify $ \s -> s {envStatements = envStatements s <> [stm]}

-- | Declare a new variable.
newVar :: CGM (Var a)
newVar = do
  s <- get
  put s {envNameSupply = succ $ envNameSupply s}
  return $ V $ envNameSupply s

-- | Capture all emitted statements.
capture :: CGM a -> CGM ([String], a)
capture m = do
  s <- get
  put s {envStatements = mempty}
  x <- m
  s' <- get
  put s' {envStatements = envStatements s}
  return (envStatements s', x)

newtype CGM a = CGM {unCGM :: State Env a}
  deriving (Functor, Applicative, Monad, MonadState Env)

runCGM :: CGM a -> (Env, a)
runCGM = swap . flip runState emptyEnv . unCGM
  where swap (a, b) = (b, a)

class CG a where
  cg :: a -> CGM String

instance CG (Exp a) where
  cg = pure . cgExp

instance CG (Var a) where
  cg = pure . cgVar

instance CG (Stm a) where
  cg = fmap (unlines . fst) . capture . cgStm

cgVar :: Var a -> String
cgVar (V v) = 'v' : show v

cgExp :: Exp a -> String
cgExp (Const n)    = show n
cgExp (Bool b)     = if b then "1" else "0"
cgExp (B2I b)      = cgExp b
cgExp (BOp op a b) = mconcat ["(", cgExp a, cgBOp op, cgExp b, ")"]
cgExp (Var v)      = cgVar v
cgExp (Exp.Alg v)  = cgVar v
cgExp (Undef)      = "0"

cgBOp :: BOp a b -> String
cgBOp Add = "+"
cgBOp Sub = "-"
cgBOp Mul = "*"
cgBOp Div = "/"
cgBOp Mod = "%"
cgBOp Eq  = "=="
cgBOp Neq = "!="
cgBOp Gt  = ">"
cgBOp Lt  = "<"
cgBOp Ge  = ">="
cgBOp Le  = "<="
cgBOp And = "&&"
cgBOp Or  = "||"

cgStm :: Stm a -> CGM a
cgStm (Return x) = return x
cgStm (Bind m f) = do
  x <- cgStm m
  cgStm (f x)
cgStm (Print s) = do
  emit $ "printf(\"" <> s <> "\\n\");"
cgStm (PrintN n) = do
  emit $ "printf(\"%d\\n\", " <> cgExp n <> ");"
cgStm (Scan) = do
  v <- newVar
  emit $ "scanf(\"%d\", &" <> cgVar v <> ");"
  return (Var v)
cgStm (Set v x) = do
  emit $ cgVar v <> " = " <> cgExp x <> ";"
cgStm (Get v) = do
  Var <$> cgStm (NewRef (Var v))
cgStm (NewRef x) = do
  v <- newVar
  cgStm (Set v x)
  return v
cgStm (Read ptr off) = do
  v <- newVar
  emit $ cgVar v <> " = ((unsigned long long*)(" <> cgExp ptr <> "))[" ++ show off <> "];"
  return (Var v)
cgStm (Write ptr off x) = do
  emit $ "((unsigned long long*)(" <> cgExp ptr <> "))[" <> show off <> "] = " <> cgExp x <> ";"
cgStm (Alloca n) = do
  v <- newVar
  emit $ cgVar v <> " = (unsigned long long)alloca(8*" <> show n <> ");"
  return (Var v)
cgStm (If c th el) = do
  v <- newVar
  th' <- unlines . fst <$> capture (cgStm $ th >>= Set v)
  el' <- unlines . fst <$> capture (cgStm $ el >>= Set v)
  emit $ "if(" <> cgExp c <> "){\n" <> th' <> "} else {\n" <> el' <> "}"
  return (Var v)
cgStm (Die) = do
  cgStm $ Print "<suicide>"
  emit $ "exit(1);"
  return Undef

cgProg :: Stm () -> String
cgProg = wrap . runCGM . cg
  where
    wrap (env, s) = unlines
      [ "#include <alloca.h>"
      , "#include <stdio.h>"
      , "#include <stdlib.h>"
      , "int main() {"
      , unlines ["long long v" ++ show v ++ ";" | v <- [0 .. envNameSupply env-1]]
      , s
      , "}"
      ]
