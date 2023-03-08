module Args.Status (
    isEmpty
) where

isEmpty :: [String] -> Bool
isEmpty []  = True
isEmpty _   = False