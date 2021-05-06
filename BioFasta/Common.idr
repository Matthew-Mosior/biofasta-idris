module BioFasta.Common

import Data.List
import Data.Maybe
import Data.Strings
import BioCore.Sequence


||| Splits a character list into a list of newline separated character lists.
|||
||| ```idris example
||| lines' (unpack "\rA BC\nD\r\nE\n")
||| ```
public export
lines' : List Char -> List (List Char)
lines' [] = []
lines' s  = case break isNL s of
              (l, s') => l :: case s' of
                                []       => []
                                _ :: s'' => lines' (assert_smaller s s'')

||| The groupBy function returns a list of lists such that the concatenation
||| of the list is equal to the argument, and each sublist contains only
||| elements that are equal according to the user-supplied predicate.
|||
||| ```idris example
||| groupBy (==) [1, 1, 2, 3, 3]
||| ```
|||
public export
groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy _ [] = []
groupBy p' (x'::xs') =
    let (ys',zs') = go p' x' xs'
    in (x' :: ys') :: zs'
    where
        go : (a -> a -> Bool) -> a -> List a -> (List a, List (List a))
        go p z (x::xs) =
            let (ys,zs) = go p x xs
            in case p z x of
                True => (x :: ys, zs)
                False => ([], (x :: ys) :: zs)
        go _ _ [] = ([], [])
--groupBy : (a -> a -> Bool) -> List a -> List (List a)
--groupBy _ [] = []
--groupBy p list@(x :: xs) =
--  let (ys, zs) = span (p x) xs in
--    (x :: ys) :: groupBy p (assert_smaller list zs)
