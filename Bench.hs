{-# LANGUAGE TypeFamilies #-}

import System.Environment
import Text.Read (readEither)
import System.Exit
import Text.Printf
import Data.Maybe
import System.CPUTime
import Data.List (foldl')
import Data.Functor (void)

import qualified Numeric.RoundedSimple as M
import qualified Numeric.Rounded.Simple as C

import Data.Kind

data Flag = NumRuns Int
          | Prec Int
          | Bench BenchDesc

data Option = Option
            { flag :: Char
            , args :: [String]
            , description :: String
            , parse :: [String] -> Either String (Flag,[String])
            }

instance Show Option where
  show o = printf "  -%c%-10s %s\n" (flag o) o_args (description o)
    where o_args = concat $ map (' ':) $ args o

argError :: Char -> String -> String
argError c s = "error parsing option '-" ++ [c] ++ "': " ++ s

singleArg :: Char -> (String -> Either String Flag) -> [String] -> Either String (Flag,[String])
singleArg c _ [] = Left $ argError c "missing argument"
singleArg c f (a:as) = case f a of Right x -> Right (x,as)
                                   Left  y -> Left $ argError c y

opts :: [Option]
opts = [ Option { flag = 'b'
                , args = ["BENCH"]
                , description =
                    "select benchmark to run, one of: " ++
                    (drop 2 $ concat $ fmap ((", " ++) . desc) benchmarks)
                , parse = singleArg 'b' $ \a ->
                    case lookup a (map (\d -> (desc d,d)) benchmarks) of
                      Nothing -> Left $ "'" ++ a ++ "' is not a valid benchmark"
                      Just b  -> Right $ Bench b
                }
       , Option { flag = 'h', args = [], description = "prints this help message"
                , parse = \_ -> Left $ "Options [default]:\n" ++ (init $ concat $ map show opts) }
       , Option { flag = 'n', args = ["N"], description = "set number of iterations"
                , parse = singleArg 'n' $ \a -> readEither a >>= return . NumRuns }
       , Option { flag = 'p', args = ["P"], description = "set offset precision (bits additional to 2*N)"
                , parse = singleArg 'p' $ \a -> readEither a >>= return . Prec }
       ]

parseArgs :: [String] -> Either String [Flag]
parseArgs [] = Right []
parseArgs (['-',x]:xs) =
  case lookup x $ map (\o -> (flag o,o)) opts of
    Nothing -> Left $ "unknown flag '-" ++ [x] ++ "'"
    Just o -> do
      (f,ys) <- parse o xs
      parseArgs ys >>= Right . (f:)
parseArgs (x:_) = Left $ "unknown flag '" ++ x ++ "'"

bench' :: IO t -> IO (t,Double)
bench' f = do
  start <- getCPUTime
  r <- f
  end <- getCPUTime
  return $ (r,(fromInteger (end - start)) / (10**12))

bench :: IO t -> IO t
bench f = do
  (r,t) <- bench' f
  printf "time: %.6f sec\n" t
  return r

{- tried to get this more generic, but don't know how type families
   are supposed to be used

data Z
data S x

type family RI a :: * where
  RI Z = C.Rounded
  RI (S Z) = M.Rounded
-}

type Prec  = Int
type Times = Int

class RR r where
  add :: Int -> r -> r -> r
  sub :: Int -> r -> r -> r
  mul :: Int -> r -> r -> r
  div :: Int -> r -> r -> r
  neg :: Int -> r -> r
  fromInteger' :: Prec -> Integer -> r
  fromRational' :: Prec -> Rational -> r
  toString :: Prec -> r -> String

instance RR C.Rounded where
  add = C.add_ C.TowardNearest
  sub = C.sub_ C.TowardNearest
  mul = C.mul_ C.TowardNearest
  div = C.div_ C.TowardNearest
  neg = C.negate_ C.TowardNearest
  fromInteger' = C.fromInteger' C.TowardNearest
  fromRational' = C.fromRational' C.TowardNearest
  toString _ = C.show'

instance RR M.Rounded where
  add = M.add_ M.TowardNearest
  sub = M.sub_ M.TowardNearest
  mul = M.mul_ M.TowardNearest
  div = M.div_ M.TowardNearest
  neg = M.negate_ M.TowardNearest
  fromInteger' = M.fromInteger' M.TowardNearest
  fromRational' = M.fromRational' M.TowardNearest
  toString = M.toString

logmap_step :: RR r => r -> r -> Prec -> r
logmap_step c x p = c `mulp` x `mulp` (one `subp` x)
  where
    mulp = mul p
    subp = sub p
    one  = fromInteger' 1 1

run_logmap' :: RR r => r -> r -> Prec -> Times -> IO ()
run_logmap' c x0 p_off n = putStrLn $ toString digits $ xn
  where
    digits = min 30 $ p_off `Prelude.div` 3
    mu s = 2*s
    p = p_off + mu n
    step_prec s = p - mu s
    precisions = map step_prec [0..n-1]
    xn = foldl' (logmap_step c) x0 precisions

run_negate' :: RR r => r -> Prec -> Times -> IO ()
run_negate' x p_off n = putStrLn $ toString 10 $ foldl' (flip neg) x $ take n $ repeat p_off
  -- or (foldl' (\y _ -> (neg p_off y)) x [1..n])
  -- instead of the foldl' construction using (iterate (neg p_off) x !! n) is
  -- 4x slower, why?

data Which = Claude | Michal

type Benchmark = Which -> Prec -> Times -> IO ()

run_logmap :: Benchmark
run_logmap Claude = run_logmap' (fromRational' 4 (375/100) :: C.Rounded) (fromRational' 1 (1/2) :: C.Rounded)
run_logmap Michal = run_logmap' (fromRational' 4 (375/100) :: M.Rounded) (fromRational' 1 (1/2) :: M.Rounded)

run_negate :: Benchmark
run_negate Claude = run_negate' (fromInteger' 1 1 :: C.Rounded)
run_negate Michal = run_negate' (fromInteger' 1 1 :: M.Rounded)

both :: (Which -> IO t) -> IO (t,t)
both f = do
  c <- f Claude
  m <- f Michal
  return (c,m)

type BenchDesc = (String,Benchmark,[Flag])

run1 :: [Flag] -> BenchDesc -> IO ()
run1 userFlags (desc,b,def) =
  do
    putStrLn $ desc ++ " p: " ++ (show p) ++ ", n: " ++ (show n)
    void $ both $ \w -> bench $ b w p n
    putStrLn ""
  where
    flags = def ++ userFlags
    nr (NumRuns _) = True
    nr _ = False
    pr (Prec _) = True
    pr _ = False
    NumRuns n = last $ filter nr flags
    Prec p = last $ filter pr flags

run :: [Flag] -> [BenchDesc] -> IO ()
run userFlags = foldl' (>>) (pure ()) . fmap (run1 userFlags) . filterB
  where
    br (Bench _) = True
    br _ = False
    filterB = case map (\(Bench b) -> b) $ filter br userFlags of
      [] -> id
      xs -> const xs

desc :: BenchDesc -> String
desc (d,_,_) = d

benchmarks :: [BenchDesc]
benchmarks = [ ("logmap",run_logmap,[Prec 53,NumRuns    25000])
             , ("negate",run_negate,[Prec  1,NumRuns 10000000])
             ]

main :: IO ()
main = getArgs >>= either die (flip run $ benchmarks) . parseArgs
