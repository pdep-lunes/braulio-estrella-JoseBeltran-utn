module Lib () where

import Text.Show.Functions ()
data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadVida :: Int,
    estado :: String
} deriving (Show)

type RadioExplosion = Int
type Personajes = [Personaje]

jugadores :: [Personaje]
jugadores = [espina, pamela, jose]

--FUNCIONES SECUNDARIAS

esColega :: Personaje -> Bool
esColega unPersonaje = estado unPersonaje == "Colega" 

esContrincate :: Personaje -> Bool
esContrincate unPersonaje = estado unPersonaje == "Contrincante"

muere :: Personaje -> Personaje
muere unPersonaje = unPersonaje {cantidadVida = 0}

dobleVida :: Personaje -> Personaje
dobleVida unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje * 2}

hacerDanio :: Personaje -> Personaje
hacerDanio unPersonaje 
    | esContrincate unPersonaje && cantidadVida unPersonaje <= 1000 = muere unPersonaje
    | esContrincate unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje - 1000} 
    | otherwise = unPersonaje

desactivarSuper :: Personaje -> Personaje
desactivarSuper unPersonaje = unPersonaje {superPoderActivo = False}

activarSuper :: Personaje -> Personaje
activarSuper unPersonaje = unPersonaje {superPoderActivo = True}

modificarNombre :: Personaje -> Personaje
modificarNombre unPersonaje = unPersonaje {nombre = nombre unPersonaje ++ " Espina Estuvo aqui"}

-- PODERES

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = hacerDanio unPersonaje

lluviaDeTuercas :: Personaje -> String-> Personaje
lluviaDeTuercas unPersonaje tipoDelluvia
    | tipoDelluvia == "sanadoras" && esColega unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje + 800}
    | tipoDelluvia == "dañinas" && esContrincate unPersonaje = unPersonaje {cantidadVida = cantidadVida unPersonaje `div` 2}
    | otherwise = unPersonaje  

granadaDeEspinas :: Personaje -> RadioExplosion -> Personaje
granadaDeEspinas unPersonaje radioExplosion 
    | radioExplosion > 3 && cantidadVida unPersonaje <= 800 && esContrincate unPersonaje = desactivarSuper.muere.modificarNombre $ unPersonaje
    | radioExplosion > 3 = modificarNombre unPersonaje 
    | otherwise = bolaEspinosa unPersonaje 

torretaCurativa :: Personaje -> Personaje
torretaCurativa suAliado
    | esColega suAliado = activarSuper.dobleVida $ suAliado
    | otherwise = suAliado

-- PERSONAJES

espina :: Personaje  
espina = UnPersonaje {
        nombre = "Espina",
        poderBasico = "Bola Espinosa",
        superPoder = "Granada de espinas",
        superPoderActivo = True,
        cantidadVida = 4800,
        estado = "Colega"
}

pamela :: Personaje  
pamela = UnPersonaje {
        nombre = "Pamela",
        poderBasico = "Lluvia De Tuercas sanadoras",
        superPoder = "Torreta Curativa",
        superPoderActivo = False,
        cantidadVida = 9600,
        estado = "Colega"
}

jose :: Personaje
jose = UnPersonaje{
    nombre = "jose",
    poderBasico = "x",
    superPoder = "z",
    superPoderActivo = True,
    cantidadVida = 750,
    estado = "Contrincante"
}

-- REPORTES

atacarConPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConPoderEspecial atacante victima 
    | superPoderActivo atacante = usarPoderes atacante victima
    | otherwise = victima

usarPoderes :: Personaje -> Personaje -> Personaje
usarPoderes atacante victima
    | nombre atacante == "Espina" = granadaDeEspinas (bolaEspinosa victima) 5
    | nombre atacante == "Pamela" = lluviaDeTuercas (torretaCurativa victima) "sanadoras"
    | otherwise = victima

quienEstaEnLasUltimas :: Personajes -> [String]
quienEstaEnLasUltimas unosPersonajes = map nombre(tienePocaVida unosPersonajes)

tienePocaVida :: Personajes -> Personajes
tienePocaVida personajes = filter ((<= 800) . cantidadVida) personajes 










