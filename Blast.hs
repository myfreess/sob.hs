module Blast where

import Formula hiding (not)

satisfiable :: FormulaT -> Bool
-- 暴力穷举
