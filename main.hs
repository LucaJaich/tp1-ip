-- module Main where

import Test.HUnit
import Solucion

run = runTestTT testSuiteEj1

testSuiteEj1 = test [
    "nombresDeUsuarios 1 (sin usuarios)" ~: (nombresDeUsuarios redVacia) ~?= [],
    "nombresDeUsuarios 2 (1 usuario)" ~: (nombresDeUsuarios redUnUsuario) ~?= ["Juli"],
    "nombresDeUsuarios 3 (comun)" ~: (nombresDeUsuarios redA) ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 4 (repetidos)" ~: (nombresDeUsuarios redRepetidos) ~?= ["Juli", "Santi", "Luca", "Diego"],

    "amigosDe 1 (sin relaciones)" ~: (amigosDe redSinRelaciones usuario1) ~?= [],
    "amigosDe 2 (relaciones de un lado)" ~: (amigosDe redAmigosDe2 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 3 (relaciones del otro lado)" ~: (amigosDe redAmigosDe3 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 4 (no hay relaciones)" ~: (amigosDe redAmigosDe4 usuario1) ~?= [],
    "amigosDe 5 (comun, lados variados)" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario4],
    -- agregar caso juli

    "cantidadDeAmigos 1 (sin relaciones)" ~: (cantidadDeAmigos redSinRelaciones usuario1) ~?= 0,
    "cantidadDeAmigos (no hay relaciones de usuario)" ~: (cantidadDeAmigos redAmigosDe4 usuario1) ~?= 0,
    "cantidadDeAmigos 3 (comun)" ~: (cantidadDeAmigos redA usuario3) ~?= 2,
    -- consultar en la practica

    "usuarioConMasAmigos 1 (sin relaciones)" ~: expectAny (usuarioConMasAmigos redSinRelaciones) usuariosA,
    "usuarioConMasAmigos 2 (comun)" ~: (usuarioConMasAmigos redUsuarioConMasAmigos2) ~?= usuario2,
    "usuarioConMasAmigos 3 (misma cantidad)" ~: expectAny (usuarioConMasAmigos redUsuarioConMasAmigos3) [usuario1, usuario2],
    
    "estaRobertoCarlos 1 (red vacia)" ~: (estaRobertoCarlos redVacia) ~?= False
    -- LUCA agrega los casos paja ;D 
    -- caso no hay relaciones
    --      falso
    --      verdadero
    --      2 verdaderos


    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

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

-- usuarioConMasAmigos2 (usuario2 mas amigos)
relacionesUsuarioConMasAmigos2 = [(usuario2, usuario1), (usuario1, usuario3), (usuario2, usuario3), (usuario4, usuario2)]
redUsuarioConMasAmigos2 = (usuariosA, relacionesUsuarioConMasAmigos2, [])

-- usuarioConMasAmigos3 (usuario1 y 2 con mismos amigos)
relacionesUsuarioConMasAmigos3 = [(usuario2, usuario1), (usuario1, usuario3), (usuario2, usuario4)]
redUsuarioConMasAmigos3 = (usuariosA, relacionesUsuarioConMasAmigos3, [])