module AocUtil exposing (..)

fromJust x = case x of
    Just xx -> xx
    Nothing -> Debug.todo "fromJust"

fromOk x = case x of
    Ok xx -> xx
    Err e ->
        let
            _ = e |> Debug.log "Err"
        in
            Debug.todo "fromOk"

indexFilter pred = List.indexedMap (\i c -> if (pred i) then Just c else Nothing) >> List.filterMap identity
