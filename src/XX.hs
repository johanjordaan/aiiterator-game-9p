module XX where

import Shuffle

import Control.Monad.Except
import Control.Exception

type Error = String
type IOExceptT = ExceptT Error IO




runMeXX :: Int -> Int -> IO ()
runMeXX size n = do {
  r1 <- runExceptT (initMe size);   -- runExceptT removes the ExceptT wrapper
  r2 <- evaluate $ updateMe2 n r1;
  r3 <- evaluate $ updateMe2 n r2;

--  r2 <- updateMe 2 r1;
  print r1;
  print r2;
  print r3;

}

initMe :: Int -> IOExceptT [Int]
initMe size =
  if size <= 0
    then throwError "size to small <=0"
    else liftIO (shuffle [0..size-1])

_updateMe :: Int -> [Int] -> [Int]
_updateMe n l = n:l


updateMe :: Int -> [Int] -> Either Error [Int]
updateMe n l =
  if n == 0
    then Left "zero is invalid"
    else Right (_updateMe n l)


updateMe2 :: Int -> Either Error [Int] -> Either Error [Int]
updateMe2 n el = do {
  l <- el;
  if n == 0
    then Left "zero is invalid"
    else Right (_updateMe n l)
}
