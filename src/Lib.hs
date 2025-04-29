module Lib (bolaEspinosa, Personaje(UnPersonaje)) where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    estado :: String
} deriving (Show)

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje 
    | cantidadVida unPersonaje <= 1000 = unPersonaje {cantidadVida = 0}
    | otherwise = restoMilVida unPersonaje

restoMilVida :: Personaje -> Personaje
restoMilVida unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje - 1000} 

--lluviaDeTuercas :: Personaje -> Personaje
--lluviaDeTuercas unPersonaje
--   | esColega unPersonaje = unPersonaje {cantidadVida + 800}
--   | esContrincante unPersonaje = cantidadVida unPersonaje `div` 2
--   | otherwise = cantidadVida unPersonaje 


