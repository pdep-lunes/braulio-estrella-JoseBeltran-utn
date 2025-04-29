module Main where

import Lib

main :: IO ()
main = do
    let personajeEspinosa = UnPersonaje "Espina" "Bola Espinosa" "Granada de Espinas" True 1500 "Colega"
    print (bolaEspinosa personajeEspinosa)




