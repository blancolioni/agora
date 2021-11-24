--  module Professions where

    data Commodity = Food | Metal | Ore | Tools | Wood | Work deriving (Eq,Show)

    data Production = Consume Commodity Float 
                    | ConsumeChance Commodity Float Float
                    | Produce Commodity Float
            deriving (Show)

    --  type Inventory = [(Commodity, Float)]


    quantity :: [(Commodity, Float)] -> Commodity -> Float
    quantity inv item = let p (item',_) = item == item'
                        in case filter p inv of
                             ((_,x):_) -> x
                             [] -> 0
        

    executeProduce :: String -> Float -> IO ()
    executeProduce item quant = putStrLn $ "produce " ++ show quant ++ " " ++ item

    executeConsume :: String -> Float -> Float -> IO ()
    executeConsume item quant chance = putStrLn $ "consume " ++ show quant ++ " " ++ item 
                                                  ++ (if chance < 1 then " with chance " ++ show (chance * 100) ++ "%" else "")

    executeProduction :: [Production] -> IO ()
    executeProduction = mapM_ exec
       where exec (Consume commodity quantity) = executeConsume (show commodity) quantity 1
             exec (ConsumeChance commodity quantity chance) = executeConsume (show commodity) quantity chance
             exec (Produce commodity quantity) = executeProduce (show commodity) quantity


    farmerProduction :: [(Commodity, Float)] -> [Production]
    farmerProduction inv | food >= 10 = []
                         | wood > 0 && work > 0 && tools >= 1 = toolsProduction
                         | wood > 0 && work > 0 = noToolsProduction
                         | otherwise = minimumProduction
       where food = quantity inv Food
             wood = quantity inv Wood
             work = quantity inv Work
             tools = quantity inv Tools
             toolsProduction = [
                   Consume Wood 1
                 , Consume Work 1
                 , ConsumeChance Tools 1 10
                 , Produce Food (workFactor * woodFactor * 6)
                 ]
             noToolsProduction = [
                   Consume Wood 1
                 , Consume Work 1
                 , Produce Food (workFactor * woodFactor * 3)

                 ]
             minimumProduction = [
                 Produce Food 1
                 ]
             workFactor = min work 1
             woodFactor = min wood 1

    testInv1 :: [(Commodity, Float)]
    testInv1 = [(Food,4),(Wood,2),(Work,0.5),(Tools,1)]
