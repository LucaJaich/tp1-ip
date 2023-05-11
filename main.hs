-- module Main where

import Test.HUnit
import Solucion

run = runTestTT testSuiteEj1

testSuiteEj1 = test [
    "nombresDeUsuarios 1 (sin usuarios)" ~: (nombresDeUsuarios redVacia) ~?= [],
    "nombresDeUsuarios 2 (1 usuario)" ~: (nombresDeUsuarios redUnUsuario) ~?= ["Juli"],
    "nombresDeUsuarios 3 (comun)" ~: (nombreDeUsuarios redA) ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 4 (repetidos)" ~: (nombresDeUsuarios redRepetidos) ~?= ["Juli", "Santi", "Luca", "Diego"]
    ]


-- USUARIOS
usuario1 = (1, "Juli")
usuario2 = (2, "Santi")
usuario3 = (3, "Luca")
usuario4 = (4, "Diego")
usuario5 = (5, "Diego")


-- REDES
redVacia = ([],[],[])
redUnUsuario = ([usuario1], [], [])

-- redA
usuariosA = [usuario1, usuario2, usuario3, usuario4]
redA = (usuariosA, [], [])

-- redRepetidos
usuariosRepetidos = [usuario1, usuario2, usuario3, usuario4, usuario5]
redRepetidos = (usuariosRepetidos, [], [])