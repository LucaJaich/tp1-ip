module Testeos where
import Test.HUnit
import SolucionDiego
--import SolucionJuli

run = runTestTT allTests

allTests = test [
    "nombresDeUsuarios" ~: testEj1,
    "publicacionesDe" ~: testEj6

    ]

testEj1 = test [
    "nombresDeUsuarios 1 (comun)" ~: nombresDeUsuarios redA ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 2 (sin usuarios)" ~: nombresDeUsuarios redVacia ~?= [],
    "nombresDeUsuarios 3 (repetidos)" ~: nombresDeUsuarios redRepetidos ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 4 (1 usuario)" ~: nombresDeUsuarios redUnUsuario ~?= ["Juli"]
    ]

testEj6 = test [
    "publicacionesDe 1 " ~: publicacionesDe redA usuario1 ~?= [publicacion1_1, publicacion1_3],
    "publicacionesDe 2 (red sin publi)" ~: publicacionesDe redUsuarios usuario2 ~?= [],
    "publicacionesDe 3 (usuario sin publi) " ~: publicacionesDe redA  usuario2 ~?= []
    
    ]

testEj7 = test [
    "publicacionesQueLeGustanA 1 " ~: publicacionesQueLeGustanA redA usuario2 ~?= [publicacion1_1, publicacion3_1],
    "publicacionesQueLeGustanA 2 (ningun like) " ~: publicacionesQueLeGustanA redA usuario3 ~?= [],
    "publicacionesQueLeGustanA 3 (red sin publi) " ~: publicacionesQueLeGustanA redRepetidos usuario4 ~?= []

    ]


-- USUARIOS
usuario1 = (1, "Juli")
usuario2 = (2, "Santi")
usuario3 = (3, "Luca")
usuario4 = (4, "Diego")
usuario5 = (5, "Diego")

--PUBLIACIONES
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2])
publicacion1_2 = (usuario1, "Lorem ipsum dolor sit amet", [usuario5, usuario4])
publicacion1_3 = (usuario1, "Vivamus et risus at purus pharetra", [usuario4, usuario2])
publicacion1_4 = (usuario1, "Maecenas pellentesque nulla vel", [usuario5, usuario3])

publicacion2_1 = (usuario2, "Phasellus eget tellus eu", [usuario3, usuario4])
publicacion2_2 = (usuario2, "Morbi nec orci", [usuario1, usuario3])
publicacion2_3 = (usuario2, "Nullam sed risus et", [usuario4, usuario1])
publicacion2_4 = (usuario2, "Sed viverra est at augu", [usuario3, usuario5])

publicacion3_1 = (usuario3, "Morbi dapibus neque", [usuario5, usuario2])
publicacion3_2 = (usuario3, "Duis ac neque quis", [usuario1, usuario5])
publicacion3_3 = (usuario3, "Nullam eget quam", [usuario1, usuario5])
publicacion3_4 = (usuario3, "Leo id eleifend tempus", [usuario2, usuario4]) 

publicacion4_1 = (usuario4, "Aliquam tincidunt", [usuario1, usuario3])
publicacion4_2 = (usuario4, "Integer sed augue et", [usuario5, usuario2])
publicacion4_3 = (usuario4, "Morbi vitae dui id turpis", [])
publicacion4_4 = (usuario4, "Sed dapibus purus", [usuario5, usuario1])

publicacion5_1 = (usuario5, "Quisque id ante", [usuario1, usuario2])
publicacion5_2 = (usuario5, "Etiam sempe", [usuario1, usuario4])
publicacion5_3 = (usuario5, "Mauris eget ex a", [usuario3, usuario1])
publicacion5_4 = (usuario5, "Este es mi primer post", [usuario3, usuario2])

--RELACIONES
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

-- REDES
redVacia = ([],[],[])
redUnUsuario = ([usuario1], [], [])

--redSinUs
redUsuarios = (usuariosA,[],[])

redA = (usuariosA, relacionesA, publicacionesA)
-- redA
usuariosA = [usuario1, usuario2, usuario3, usuario4]
publicacionesA = [publicacion1_1, publicacion1_3, publicacion3_1, publicacion3_2, publicacion4_3]
relacionesA = [relacion1_2, relacion1_3, relacion2_3, relacion3_4]

-- redRepetidos
usuariosRepetidos = [usuario1, usuario2, usuario3, usuario4, usuario5]
redRepetidos = (usuariosRepetidos, [], [])


