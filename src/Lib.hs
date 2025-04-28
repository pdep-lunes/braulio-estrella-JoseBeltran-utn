module Lib () where

import Text.Show.Functions ()

data Personaje = unPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int
}

doble :: Int -> Int
doble x = x * 2

bolaEspinosa :: Personaje -> Int
bolaEspinosa unPersonaje
    | 