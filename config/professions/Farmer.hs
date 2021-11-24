module Farmer where

    produce :: Inventory -> [Production]
    produce inv | food' >= 10 = []
                | wood' > 0 && work' > 0 && tools' >= 1 = toolsProduction
                | wood' > 0 && work' > 0 = noToolsProduction
                | otherwise = minimumProduction
       where food' = quantity inv food
             wood' = quantity inv wood
             work' = quantity inv work
             tools' = quantity inv tools
             toolsProduction = [
                   Consume wood 1
                 , Consume work 1
                 , ConsumeChance tools 1 0.1
                 , Produce food (workFactor * woodFactor * 6)
                 ]
             noToolsProduction = [
                   Consume wood 1
                 , Consume work 1
                 , Produce food (workFactor * woodFactor * 3)

             ]
             minimumProduction = [
                 Produce food 1
             ]
             workFactor = min work' 1
             woodFactor = min wood' 1
