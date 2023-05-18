-- TODO: 
--       - PREGUNTAR POR REPETIDOS EN LOS TESTS
--       - ARREGLAR LO DE USUARIOSA Y USUARIO5
--       - Arreglar Naming de las redes (numeros)

-- module Main where

import Test.HUnit
import Solucion

run = runTestTT allTests

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
    -- agregar caso juli (usuarios repetidos)
    -- usuarios repetidos

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

allTests = test [
    "resto" ~: testSuiteEj1,
    "Ej 6" ~: testSuiteEj6,
    "Ej 7" ~: testSuiteEj7,
    "Ej 8" ~: testSuiteEj8
 ]

testSuiteEj6 = test [
    "publicacionesDe 1 (red sin publicaciones)" ~: (publicacionesDe redSinPublicaciones usuario1) ~?= [],
    "publicacionesDe 2 (usuario sin publicaciones)" ~: (publicacionesDe redPublicacionesDe1 usuario1) ~?= [],
    "publicacionesDe 3 (usuario con publicaciones)" ~: (publicacionesDe redPublicacionesDe2 usuario2) ~?= [publicacion2_1, publicacion2_2],
    "publicacionesDe 4 (2 usuarios con misma pub y likes)" ~: (publicacionesDe redPublicacionesDe3 usuario2) ~?= [publicacion_igual, publicacion2_2]
 ]

testSuiteEj7 = test [
    "publicacionesQueLeGustanA 1 (red sin publicaciones)" ~: (publicacionesQueLeGustanA redSinPublicaciones usuario1) ~?= [],
    -- ninguna publi tiene likes
    "publicacionesQueLeGustanA 2 (usuario sin likes)" ~: (publicacionesQueLeGustanA redPublicacionesQueLeGustanA1 usuario1) ~?= [],
    "publicacionesQueLeGustanA 3 (usuario con likes)" ~: (publicacionesQueLeGustanA redPublicacionesQueLeGustanAComun usuario2) ~?= [publicacion1_1, publicacion3_4]
 ]
 
testSuiteEj8 = test [
    "lesGustanLasMismasPublicaciones 1 (red sin publicaciones)" ~: (lesGustanLasMismasPublicaciones redSinPublicaciones usuario1 usuario2) ~?= True,
    "lesGustanLasMismasPublicaciones 2 (usuario1 y usuario2 sin likes)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones1 usuario1 usuario2) ~?= True,
     -- ninguna publi tiene likes
    "lesGustanLasMismasPublicaciones 3 (compara mismo usuario)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones3 usuario1 usuario1) ~?= True,
    "lesGustanLasMismasPublicaciones 4 (comun, verdadero)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones4 usuario3 usuario4) ~?= True,
    "lesGustanLasMismasPublicaciones 5 (comun, falso)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones5 usuario3 usuario4) ~?= False
    -- likes en desorden es lo mismo (?)
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

-- redSinPublicaciones
redSinPublicaciones = (usuariosA, [], [])

-- redPublicacionesDe1 (usuario1 sin publicaciones)
publicaciones1 = [publicacion2_1, publicacion2_2, publicacion3_1]
redPublicacionesDe1 = (usuariosA, [], publicaciones1)

-- redPublicacionesDe2 (usuario2 con publicaciones)
publicaciones2 = [publicacion2_1, publicacion2_2, publicacion3_1, publicacion1_4]
redPublicacionesDe2 = (usuariosA, [], publicaciones2)

-- redPublicacionesDe3 (2 usuarios con misma pub y likes)
publicacion_igual = (usuario2, "Lorem ipsum dolor sit amet", [usuario5, usuario4])
publicaciones3 = [publicacion3_1, publicacion1_2, publicacion_igual, publicacion2_2]
redPublicacionesDe3 = (usuariosA, [], publicaciones3)

-- redPublicacionesQueLeGustanA1 (usuario1 sin likes)
publicacionesQueLeGustanA1 = [publicacion2_1, publicacion2_4, publicacion3_1]
redPublicacionesQueLeGustanA1 = (usuariosA, [], publicacionesQueLeGustanA1)

-- redPublicacionesQueLeGustanA2 (usuario2 con likes)
publicacionesQueLeGustanAComun = [publicacion1_1, publicacion2_1, publicacion3_4, publicacion3_2]
redPublicacionesQueLeGustanAComun = (usuariosA, [], publicacionesQueLeGustanAComun)

-- redLesGustanLasMismasPublicaciones1 (usuario1 y usuario2 sin likes)
publicacionesLesGustanLasMismasPublicaciones1 = [publicacion2_1, publicacion2_4, publicacion5_4]
redLesGustanLasMismasPublicaciones1 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones1)

-- redLesGustanLasMismasPublicaciones3 (compara mismo usuario)
publicacionesLesGustanLasMismasPublicaciones3 = [publicacion2_2, publicacion2_4, publicacion1_2, publicacion4_4]
redLesGustanLasMismasPublicaciones3 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones3)

-- redLesGustanLasMismasPublicaciones4 (comun, verdadero)
publicacionesLesGustanLasMismasPublicaciones4 = [publicacion2_1, publicacion4_2, publicacion3_4]
redLesGustanLasMismasPublicaciones4 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones4)

-- redLesGustanLasMismasPublicaciones5 (comun, falso)
publicacionesLesGustanLasMismasPublicaciones5 = [publicacion2_1, publicacion4_2, publicacion3_4, publicacion1_2]
redLesGustanLasMismasPublicaciones5 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones5)

-- redLesGustanLasMismasPublicaciones6 (mismos likes en desorden)
publicacionesLesGustanLasMismasPublicaciones6 = []


--PUBLIACIONES
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2])
publicacion1_2 = (usuario1, "Lorem ipsum dolor sit amet", [usuario5, usuario4])
publicacion1_3 = (usuario1, "Vivamus et risus at purus pharetra", [usuario4, usuario2, usuario5, usuario3])
publicacion1_4 = (usuario1, "Maecenas pellentesque nulla vel", [usuario3])
publicacion1_5 = (usuario1, "Batata y queso sin nulla vel", [])
publicacion1_UnicoUsuario = (usuario1, "Estoy en soledad", [])


publicacion2_1 = (usuario2, "Phasellus eget tellus eu", [usuario3, usuario4])
publicacion2_2 = (usuario2, "Morbi nec orci", [usuario1, usuario3])
publicacion2_3 = (usuario2, "Nullam sed risus et", [usuario3, usuario5, usuario1])
publicacion2_4 = (usuario2, "Sed viverra est at augu", [usuario4, usuario5])
publicacion2_5 = (usuario2, "Morbi nec orci", [])

publicacion3_1 = (usuario3, "Morbi dapibus neque", [usuario5, usuario2])
publicacion3_2 = (usuario3, "Duis ac neque quis", [usuario1, usuario5, usuario4])
publicacion3_3 = (usuario3, "Nullam eget quam", [usuario1, usuario5])
publicacion3_4 = (usuario3, "Leo id eleifend tempus", [usuario2, usuario4, usuario5, usuario3])
publicacion3_5 = (usuario3, "Trato de que eget quam", [])

publicacion4_1 = (usuario4, "Aliquam tincidunt", [usuario1, usuario3])
publicacion4_2 = (usuario4, "Integer sed augue et", [usuario5, usuario2])
publicacion4_3 = (usuario4, "Morbi vitae dui id turpis", [])
publicacion4_4 = (usuario4, "Sed dapibus purus", [usuario5, usuario1])
publicacion4_5 = (usuario4, "Sed flota purus", [])

publicacion5_1 = (usuario5, "Quisque id ante", [usuario1, usuario2])
publicacion5_2 = (usuario5, "Etiam sempe", [usuario1, usuario4])
publicacion5_3 = (usuario5, "Mauris eget ex a", [usuario3, usuario1])
publicacion5_4 = (usuario5, "Este es mi primer post", [usuario3, usuario4])
publicacion5_5 = (usuario5, "Este seria igual mientras primer post", [])