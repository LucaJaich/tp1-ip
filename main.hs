-- module Main where

import Test.HUnit
import Solucion

run = runTestTT testSuiteEj1

testSuiteEj1 = test [
    "nombresDeUsuarios 1 (sin usuarios)" ~: (nombresDeUsuarios redVacia) ~?= [],
    "nombresDeUsuarios 2 (1 usuario)" ~: (nombresDeUsuarios redUnUsuario) ~?= ["Juli"],
    "nombresDeUsuarios 3 (comun)" ~: (nombresDeUsuarios redA) ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 4 (repetidos)" ~: (nombresDeUsuarios redRepetidos) ~?= ["Juli", "Santi", "Luca", "Diego"],

    "amigosDe 1 (vacio)" ~: (amigosDe redVacia usuario1) ~?= [],
    "amigosDe 2 (relaciones de un lado)" ~: (amigosDe redAmigosDe2 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 3 (relaciones del otro lado)" ~: (amigosDe redAmigosDe3 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 4 (no hay relaciones)" ~: (amigosDe redAmigosDe4 usuario1) ~?= [],
    "amigosDe 5 (comun, lados variados)" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario4]
    ]


-- USUARIOS
usuario1 = (1, "Juli")
usuario2 = (2, "Santi")
usuario3 = (3, "Luca")
usuario4 = (4, "Diego")
usuario5 = (5, "Diego")

usuariosA = [usuario1, usuario2, usuario3, usuario4]


-- REDES
redVacia = ([],[],[])
redUnUsuario = ([usuario1], [], [])

-- redA
relacionesA = [(usuario1, usuario2), (usuario3, usuario1), (usuario2, usuario4), (usuario5, usuario3)]
redA = (usuariosA, relacionesA, [])

-- redRepetidos
usuariosRepetidos = [usuario1, usuario2, usuario3, usuario4, usuario5]
redRepetidos = (usuariosRepetidos, [], [])

--redSinRelaciones
redSinRelaciones = (usuariosA, [], [])

-- redAmigosDe2 (las relaciones van de un lado)
relacionesAmigosDe2 = [(usuario1, usuario2), (usuario1, usuario3), (usuario3, usuario2), (usuario1, usuario4)]
redAmigosDe2 = (usuariosA, relacionesAmigosDe2, [])

-- redAmigosDe3 (las relaciones van del otro lado)
relacionesAmigosDe3 = [(usuario2, usuario1), (usuario3, usuario1), (usuario3, usuario2), (usuario4, usuario1)]
redAmigosDe3 = (usuariosA, relacionesAmigosDe3, [])

-- redAmigosDe4 (no hay relaciones de usuario1)
relacionesAmigosDe4 = [(usuario2, usuario3), (usuario4, usuario2)]
redAmigosDe4 = (usuariosA, relacionesAmigosDe4, [])