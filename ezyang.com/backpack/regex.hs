unit regex where
    module Regex where
        -- | A type of regular expressions.
        data Reg = Eps
                 | Sym Char
                 | Alt Reg Reg
                 | Seq Reg Reg
                 | Rep Reg

        -- | Check if a regular expression 'Reg' matches a 'String'
        accept :: Reg -> String -> Bool
        accept Eps       u = null u
        accept (Sym c)   u = u == [c]
        accept (Alt p q) u = accept p u || accept q u
        accept (Seq p q) u =
            or [accept p u1 && accept q u2 | (u1, u2) <- splits u]
        accept (Rep r) u =
            or [and [accept r ui | ui <- ps] | ps <- parts u]

        -- | Given a string, compute all splits of the string.
        -- E.g., splits "ab" == [("","ab"), ("a","b"), ("ab","")]
        splits :: String -> [(String, String)]
        splits [] = [([], [])]
        splits (c:cs) = ([], c:cs):[(c:s1,s2) | (s1,s2) <- splits cs]

        -- | Given a string, compute all possible partitions of
        -- the string (where all partitions are non-empty).
        -- E.g., partitions "ab" == [["ab"],["a","b"]]
        parts :: String -> [[String]]
        parts [] = [[]]
        parts [c] = [[[c]]]
        parts (c:cs) = concat [[(c:p):ps, [c]:p:ps] | p:ps <- parts cs]

unit main where
    dependency regex
    module Main where
        import Regex
        nocs = Rep (Alt (Sym 'a') (Sym 'b'))
        onec = Seq nocs (Sym 'c')
        -- | The regular expression which tests for an even number of cs
        evencs = Seq (Rep (Seq onec onec)) nocs
        main = print (accept evencs "acc")
