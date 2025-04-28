module Lib () where

import Text.Show.Functions ()

data Personaje = unPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    estado :: String
}

doble :: Int -> Int
doble x = x * 2

bolaEspinosa :: Personaje -> Int
bolaEspinosa unPersonaje 
    | resultadoVidaNegativa unPersonaje = 0
    | otherwise = cantidadVida unPersonaje - 1000

-- resultadoVidaNegativa: cantidadVida unPersonaje <= 1000

lluviaDeTuercas :: Personaje -> Int
lluviaDeTuercas unPersonaje
    | esColega unPersonaje = cantidadVida unPersonaje + 800
    | esContrincante unPersonaje = cantidadVida unPersonaje `div` 2
    | otherwise = cantidadVida unPersonaje
