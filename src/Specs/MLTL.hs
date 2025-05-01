-- | MLTL - Mission-time linear temporal logic
-- This is like MTL-over-naturals but over finite traces
-- and slightly different semantics. Also MLTL does not have
-- a next (X) operator.
module Specs.MLTL where
import           Control.Exception
import           Control.Exception     (IOException, SomeException, bracket,
                                        catch, evaluate, finally, try)
import           Control.Monad         (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Math
import           Parse.LTLParser       (parseLTL)
import qualified Specs.LTL             as LTL
import           System.Exit           (ExitCode (..))
import           System.IO
import           System.Process

-- all temporal operators have bounded intervals associated
type Interval = (Int, Int)

-- | A MLTL formula
data Formula =
    Top -- always true
  | Bottom -- always false
  | Atom String -- ^ p
  | Not Formula -- ^ !phi
  | Or Formula Formula -- ^ phi1 | phi2
  | And Formula Formula -- ^ phi1 & phi2
  | Implies Formula Formula -- ^ phi1 -> phi2
  | Future Interval Formula -- F phi
  | Globally Interval Formula -- G phi
  | Until Interval Formula Formula --phi1 U phi2
  deriving (Eq) -- using structural equivalence for Eq

instance Complementable Formula where
  complement f = Not f

instance Intersectable Formula where
  intersect f1 f2 = f1 `And` f2

instance Show Formula where
  show f = case f of
    Top -> "(top | !top)"
    Bottom -> "(bottom & !bottom)"
    Atom name -> name
    Not f' -> "!(" <> show f' <>")"
    Or f1 f2 -> "(" <> show f1 <> ") | (" <> show f2 <> ")"
    And f1 f2 -> "(" <> show f1 <> ") & (" <> show f2 <> ")"
    Implies f1 f2 -> "(" <> show f1 <> ") -> (" <> show f2 <> ")"
    Future (a, b) f' -> "F[" <> show a <> "," <> show b <>"](" <> show f' <>")"
    Globally (a, b) f' -> "G[" <> show a <> "," <> show b <>"](" <> show f' <>")"
    Until (a, b) f1 f2 -> "(" <> show f1 <> ") U[" <> show a <> "," <> show b <>  "](" <> show f2 <> ")"


-- | convert a MLTL formula to LTL, by using an external tool called MLTLConvertor
toLTL :: Formula -> IO (Either String LTL.Formula)
toLTL f = bracket
  -- startup code
  (createProcess (proc "MLTLConvertor" ["-ltl", show f])
      { std_in  = NoStream
      , std_out = CreatePipe
      , std_err = CreatePipe
      })
  -- cleanup code
  (\(mhin, mhout, mherr, ph) -> do
    mapM_ safeClose [mhin, mhout, mherr]
    terminateProcess ph
    _ <- waitForProcess ph
    pure ()
  )
  -- worker code
  (\(_mhin, mhout, mherr, _ph) -> do
      out <- safeReadAndClose mhout
      --putStrLn $ "converted to ltl"
      err <- safeReadAndClose mherr

      parsed <- try (evaluate (parseLTL out)) :: IO (Either SomeException LTL.Formula)
      pure $ case parsed of
        Right ltl -> Right ltl
        Left ex   -> Left $ "Failed to parse LTL: " ++ show ex
  )
  where
    safeReadAndClose :: Maybe Handle -> IO String
    safeReadAndClose Nothing  = pure ""
    safeReadAndClose (Just h) = do
      out <- BS.hGetContents h
      BS.length out `seq` hClose h
      pure (BS8.unpack out)

    safeClose :: Maybe Handle -> IO ()
    safeClose Nothing  = pure ()
    safeClose (Just h) = hClose h `catch` handler
      where
        handler :: IOException -> IO ()
        handler _ = pure ()

