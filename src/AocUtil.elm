module AocUtil exposing (..)

fromJust x = case x of
    Just xx -> xx
    Nothing -> Debug.todo "fromJust"
