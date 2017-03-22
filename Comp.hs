-- | Compile Grape programs.
module Comp where
import Control.Shell
import Stm
import CG

-- | Compile the given program and save the binary under the given name.
compile :: FilePath -> Stm () -> Shell ()
compile out prog = inCustomTempDirectory (takeDirectory out) $ do
  output "prog.c" $ cgProg prog
  run "gcc" ["-o" ++ (".." </> out), "prog.c"]

-- | Compile and run a program. The binary is removed afterward.
compileAndRun :: Stm () -> Shell ()
compileAndRun prog = inTempDirectory $ do
  compile "./a.out" prog
  run "./a.out" []
