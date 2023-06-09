import Test.HUnit
import Solucion

main = runTestTT allTests

allTests = test [
    "Ej 1" ~: testSuiteEj1,
    "Ej 2" ~: testSuiteEj2,
    "Ej 3" ~: testSuiteEj3,
    "Ej 4" ~: testSuiteEj4,
    "Ej 5" ~: testSuiteEj5,
    "Ej 6" ~: testSuiteEj6,
    "Ej 7" ~: testSuiteEj7,
    "Ej 8" ~: testSuiteEj8,
    "Ej 9" ~: testSuiteEj9,
    "Ej 10" ~: testSuiteEj10
 ]

-- Los nombres de las redes o son autoexplicativos o tienen el nombre del ejercicio + el nro test
-- Ejemplo: "nombreDeEj nroDeTest (contexto)" ~: (nombreDeFuncion parametros) ~?= resultadoEsperado

testSuiteEj1 = test [
    "nombresDeUsuarios 1 (red sin usuarios)" ~: (nombresDeUsuarios redVacia) ~?= [],
    "nombresDeUsuarios 2 (1 solo usuario)" ~: (nombresDeUsuarios redUnUsuario) ~?= ["Juli"],
    "nombresDeUsuarios 3 (sin repetidos)" ~: (nombresDeUsuarios redSinRepetidos) ~?= ["Juli", "Santi", "Luca", "Diego"],
    "nombresDeUsuarios 4 (comun, nombres repetidos)" ~: (nombresDeUsuarios redA) ~?= ["Juli", "Santi", "Luca", "Diego"]
 ]

testSuiteEj2 = test [
    "amigosDe 1 (red sin relaciones)" ~: (amigosDe redSinRelaciones usuario1) ~?= [],
    "amigosDe 2 (relaciones de un lado)" ~: (amigosDe redAmigosDe2 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 3 (relaciones del otro lado)" ~: (amigosDe redAmigosDe3 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe 4 (usuario sin relaciones)" ~: (amigosDe redAmigosDe4 usuario1) ~?= [],
    "amigosDe 5 (comun, lados variados)" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario4],
    "amigosDe 6 (dos usuarios con el mismo nombre)" ~: (amigosDe redAmigosDe6 usuario2) ~?= [(1, "Juli"),(4,"Diego"),(5,"Diego")]
 ]

testSuiteEj3 = test [
    "cantidadDeAmigos 1 (red sin relaciones)" ~: (cantidadDeAmigos redSinRelaciones usuario1) ~?= 0,
    "cantidadDeAmigos 2 (usuario sin relaciones)" ~: (cantidadDeAmigos redAmigosDe3 usuario5) ~?= 0,
    "cantidadDeAmigos 3 (comun)" ~: (cantidadDeAmigos redA usuario3) ~?= 2
 ]

testSuiteEj4 = test [
    "usuarioConMasAmigos 1 (red sin relaciones)" ~: expectAny (usuarioConMasAmigos redSinRelaciones) usuariosA,
    "usuarioConMasAmigos 2 (comun)" ~: (usuarioConMasAmigos redUsuarioConMasAmigos2) ~?= usuario2,
    "usuarioConMasAmigos 3 (misma cantidad de amigos)" ~: expectAny (usuarioConMasAmigos redUsuarioConMasAmigos3) [usuario1, usuario2]
 ]

testSuiteEj5 = test [
    "estaRobertoCarlos 1 (red vacia)" ~: (estaRobertoCarlos redVacia) ~?= False,
    "estaRobertoCarlos 2 (red sin relaciones)" ~: (estaRobertoCarlos redSinRelaciones) ~?= False,
    "estaRobertoCarlos 3 (red pocos amigos)" ~: (estaRobertoCarlos redA) ~?= False,
    "estaRobertoCarlos 4 (red diez amigos)" ~: (estaRobertoCarlos redDiezAmigos) ~?= False,
    "estaRobertoCarlos 5 (red mas de diez amigos)" ~: (estaRobertoCarlos redMasDeDiezAmigos) ~?= True,
    "estaRobertoCarlos 6 (red dos usuarios tienen mas de diez amigos)" ~: (estaRobertoCarlos redMuchosAmigos) ~?= True
 ]

testSuiteEj6 = test [
    "publicacionesDe 1 (red sin publicaciones)" ~: (publicacionesDe redSinPublicaciones usuario1) ~?= [],
    "publicacionesDe 2 (usuario sin publicaciones)" ~: (publicacionesDe redPublicacionesDe2 usuario1) ~?= [],
    "publicacionesDe 3 (usuario con publicaciones)" ~: (publicacionesDe redPublicacionesDe3 usuario2) ~?= [publicacion2_1, publicacion2_2],
    "publicacionesDe 4 (2 publis con mismo texto y likes pero distinto autor)" ~: (publicacionesDe redPublicacionesDe4 usuario2) ~?= [publicacion_igual, publicacion2_2]
 ]

testSuiteEj7 = test [
    "publicacionesQueLeGustanA 1 (red sin publicaciones)" ~: (publicacionesQueLeGustanA redSinPublicaciones usuario1) ~?= [],
    "publicacionesQueLeGustanA 2 (usuario sin likes)" ~: (publicacionesQueLeGustanA redLeGustanA2 usuario1) ~?= [],
    "publicacionesQueLeGustanA 3 (usuario con likes)" ~: (publicacionesQueLeGustanA redLeGustanA3 usuario2) ~?= [publicacion1_1, publicacion3_4],
    "publicacionesQueLeGustanA 4 (publicaciones sin likes)" ~: (publicacionesQueLeGustanA redPublicacionesSinLikes usuario1) ~?= [],
    "publicacionesQueLeGustanA 5 (publicacion con 2 likes de un mismo usuario)" ~: (publicacionesQueLeGustanA redDosVecesMeGusta usuario1) ~?= [publicacionDosVecesMeGusta]
 ]
 
testSuiteEj8 = test [
    "lesGustanLasMismasPublicaciones 1 (red sin publicaciones)" ~: (lesGustanLasMismasPublicaciones redSinPublicaciones usuario1 usuario2) ~?= True,
    "lesGustanLasMismasPublicaciones 2 (usuario1 y usuario2 no pusieron likes)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones2 usuario1 usuario2) ~?= True,
    "lesGustanLasMismasPublicaciones 3 (compara mismo usuario)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones3 usuario1 usuario1) ~?= True,
    "lesGustanLasMismasPublicaciones 4 (comun, verdadero)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones4 usuario3 usuario4) ~?= True,
    "lesGustanLasMismasPublicaciones 5 (comun, falso)" ~: (lesGustanLasMismasPublicaciones redLesGustanLasMismasPublicaciones5 usuario3 usuario4) ~?= False,
    "lesGustanLasMismasPublicaciones 6 (publicaciones sin likes)" ~: (lesGustanLasMismasPublicaciones redPublicacionesSinLikes usuario1 usuario2) ~?= True
 ]

testSuiteEj9 = [
    "tieneUnSeguidorFiel 1 (red sin publicaciones)" ~: (tieneUnSeguidorFiel redSinPublicaciones usuario1) ~?= False,
    "tieneUnSeguidorFiel 2 (usuario sin publicaciones)" ~: (tieneUnSeguidorFiel redTieneUnSeguidorFiel2 usuario1) ~?= False,
    "tieneUnSeguidorFiel 3 (publicaciones sin likes)" ~: (tieneUnSeguidorFiel redPublicacionesSinLikes usuario1) ~?= False,
    "tieneUnSeguidorFiel 4 (usuario es su 'seguidor fiel')" ~: (tieneUnSeguidorFiel redAutoLikes usuario1) ~?= False,
    "tieneUnSeguidorFiel 5 (comun, verdadero)" ~: (tieneUnSeguidorFiel redTieneUnSeguidorFiel5 usuario5) ~?= True,
    "tieneUnSeguidorFiel 6 (comun, falso)" ~: (tieneUnSeguidorFiel redTieneUnSeguidorFiel6 usuario5) ~?= False
 ]

testSuiteEj10 = test [
    "existeSecuenciaDeAmigos 1 (red sin relaciones)" ~: (existeSecuenciaDeAmigos redSinRelaciones usuario1 usuario2) ~?= False,
    "existeSecuenciaDeAmigos 2 (red un usuario, sin amigos)" ~: (existeSecuenciaDeAmigos redUnUsuario usuario1 usuario1) ~?= False,
    "existeSecuenciaDeAmigos 3 (comun, verdadero)" ~: (existeSecuenciaDeAmigos redA usuario1 usuario4) ~?= True,
    "existeSecuenciaDeAmigos 4 (comun, son amigos directos)" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    "existeSecuenciaDeAmigos 5 (comun, falso porque no hay camino)" ~: (existeSecuenciaDeAmigos redExisteSecuenciaDeAmigos5 usuario1 usuario5) ~?= False,
    "existeSecuenciaDeAmigos 6 (usuario consigo mismo en cadena)" ~: (existeSecuenciaDeAmigos redExisteSecuenciaDeAmigos6 usuario1 usuario1) ~?= True,
    "existeSecuenciaDeAmigos 7 (algun usuario no está en relaciones)" ~: (existeSecuenciaDeAmigos redExisteSecuenciaDeAmigos6 usuario1 usuario5) ~?= False,
    "existeSecuenciaDeAmigos 8 (usuario consigo mismo, no tiene amigos)" ~: (existeSecuenciaDeAmigos redExisteSecuenciaDeAmigos8 usuario1 usuario1) ~?= False,
    "existeSecuenciaDeAmigos 9 (usuario consigo mismo, tiene un amigo)" ~: (existeSecuenciaDeAmigos redExisteSecuenciaDeAmigos9 usuario1 usuario1) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


usuariosA = [usuario1, usuario2, usuario3, usuario4, usuario5]

-- REDES
redVacia = ([],[],[])
redUnUsuario = ([usuario1], [], [])

-- redA
relacionesA = [(usuario1, usuario2), (usuario3, usuario1), (usuario2, usuario4), (usuario5, usuario3)]
redA = (usuariosA, relacionesA, [])

-- redSinRepetidos
usuariosSinRepetidos = [usuario1, usuario2, usuario3, usuario4]
redSinRepetidos = (usuariosSinRepetidos, [], [])

--redes sin relaciones ni publicaciones (son iguales pero se usan para distintos casos)
redSinRelaciones = (usuariosA, [], [])
redSinPublicaciones = (usuariosA, [], [])

-- redAmigosDe2 (las relaciones van de un lado)
relacionesAmigosDe2 = [(usuario1, usuario2), (usuario1, usuario3), (usuario3, usuario2), (usuario1, usuario4)]
redAmigosDe2 = (usuariosA, relacionesAmigosDe2, [])

-- redAmigosDe3 (las relaciones van del otro lado)
relacionesAmigosDe3 = [(usuario2, usuario1), (usuario3, usuario1), (usuario3, usuario2), (usuario4, usuario1)]
redAmigosDe3 = (usuariosA, relacionesAmigosDe3, [])

-- redAmigosDe4 (no hay relaciones de usuario1)
relacionesAmigosDe4 = [(usuario2, usuario3), (usuario4, usuario2)]
redAmigosDe4 = (usuariosA, relacionesAmigosDe4, [])

-- redAmigosDe6 (dos usuarios con el mismo nombre)
relacionesAmigosDe6 = relacionesA ++ [(usuario2, usuario5)]
redAmigosDe6 = (usuariosA, relacionesAmigosDe6, [])

-- usuarioConMasAmigos2 (usuario2 mas amigos)
relacionesUsuarioConMasAmigos2 = [(usuario2, usuario1), (usuario1, usuario3), (usuario2, usuario3), (usuario4, usuario2)]
redUsuarioConMasAmigos2 = (usuariosA, relacionesUsuarioConMasAmigos2, [])

-- usuarioConMasAmigos3 (usuario1 y 2 con mismos amigos)
relacionesUsuarioConMasAmigos3 = [(usuario2, usuario1), (usuario1, usuario3), (usuario2, usuario4)]
redUsuarioConMasAmigos3 = (usuariosA, relacionesUsuarioConMasAmigos3, [])

--redDiezAmigos (usuario tiene diez amigos)
usuariosDiezAmigos= [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesDiezAmigos =[(usuario1, usuario6), (usuario2, usuario6), (usuario3, usuario6), (usuario4, usuario6), (usuario5, usuario6), (usuario7, usuario6), (usuario8, usuario6), (usuario9, usuario6), (usuario10, usuario6), (usuario11, usuario6)]
redDiezAmigos = (usuariosDiezAmigos, relacionesDiezAmigos, [])

--redMasDeDiezAmigos (usuario tiene mas de diez amigos)
usuariosMasDeDiezAmigos= [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesMasDeDiezAmigos =[(usuario1, usuario6), (usuario2, usuario6), (usuario3, usuario6), (usuario4, usuario6), (usuario5, usuario6), (usuario7, usuario6), (usuario8, usuario6), (usuario9, usuario6), (usuario10, usuario6), (usuario11, usuario6), (usuario12, usuario6)]
redMasDeDiezAmigos = (usuariosMasDeDiezAmigos, relacionesMasDeDiezAmigos, [])

--redMuchosAmigos (varios usuarios tienen muchos amigos)
usuariosMuchosAmigos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesMuchosAmigos = [(usuario1, usuario2), (usuario1, usuario6), (usuario1,usuario7), (usuario2, usuario6), (usuario2,usuario7), (usuario3, usuario6), (usuario3,usuario7), (usuario4, usuario6), (usuario4,usuario7), (usuario5, usuario6), (usuario5,usuario7), (usuario7, usuario6), (usuario8, usuario6), (usuario8,usuario7), (usuario9, usuario6), (usuario9,usuario7), (usuario10, usuario6), (usuario10,usuario7), (usuario11, usuario6),(usuario11,usuario7), (usuario12, usuario6), (usuario12,usuario7)]
redMuchosAmigos = (usuariosMuchosAmigos, relacionesMuchosAmigos, [])

-- existeSecuenciaDeAmigos5 (no hay camino entre usuario1 y usuario5)
relacionesExisteSecuenciaDeAmigos5 = [(usuario1, usuario2), (usuario3, usuario4), (usuario4, usuario5), (usuario3, usuario5)]
redExisteSecuenciaDeAmigos5 = (usuariosA, relacionesExisteSecuenciaDeAmigos5, [])

-- existeSecuenciaDeAmigos6 (usuario consigo mismo)
relacionesExisteSecuenciaDeAmigos6 = [(usuario1, usuario2), (usuario2, usuario3), (usuario3, usuario4), (usuario4, usuario1)]
redExisteSecuenciaDeAmigos6 = (usuariosA, relacionesExisteSecuenciaDeAmigos6, [])

-- existeSecuenciaDeAmigos8 (usuario consigo mismo, no tiene amigos)
relacionesExisteSecuenciaDeAmigos8 = [(usuario2, usuario3), (usuario3, usuario4)]
redExisteSecuenciaDeAmigos8 = (usuariosA, relacionesExisteSecuenciaDeAmigos8, [])

-- existeSecuenciaDeAmigos9 (usuario consigo mismo, tiene un amigo)
relacionesExisteSecuenciaDeAmigos9 = [(usuario1, usuario2)]
redExisteSecuenciaDeAmigos9 = (usuariosA, relacionesExisteSecuenciaDeAmigos9, [])

-- redPublicacionesSinLikes
publicacionesSinLikes = [publicacion1_5, publicacion1_6, publicacion2_5,publicacion3_5,publicacion4_5, publicacion5_5]
redPublicacionesSinLikes = (usuariosA, [], publicacionesSinLikes)

-- redPublicacionesDe2 (usuario1 sin publicaciones)
publicaciones2 = [publicacion2_1, publicacion2_2, publicacion3_1]
redPublicacionesDe2 = (usuariosA, [], publicaciones2)

-- redPublicacionesDe3 (usuario2 con publicaciones)
publicaciones3 = [publicacion2_1, publicacion2_2, publicacion3_1, publicacion1_4]
redPublicacionesDe3 = (usuariosA, [], publicaciones3)

-- redPublicacionesDe4 (2 usuarios con misma pub y likes)
publicacion_igual = (usuario2, "Lorem ipsum dolor sit amet", [usuario5, usuario4]) -- igual a publicacion1_2
publicaciones4 = [publicacion3_1, publicacion1_2, publicacion_igual, publicacion2_2]
redPublicacionesDe4 = (usuariosA, [], publicaciones4)

-- redLeGustanA2 (usuario1 sin likes)
publicacionesLeGustanA2 = [publicacion2_1, publicacion2_4, publicacion3_1]
redLeGustanA2 = (usuariosA, [], publicacionesLeGustanA2)

-- redLeGustanA3 (usuario2 con likes)
publicacionesLeGustanA3 = [publicacion1_1, publicacion2_1, publicacion3_4, publicacion3_2]
redLeGustanA3 = (usuariosA, [], publicacionesLeGustanA3)

-- redDosVecesMeGusta (publicacion con 2 likes de un mismo usuario)
publicacionDosVecesMeGusta = (usuario1, "Tini tini tini", [usuario1,usuario1])
redDosVecesMeGusta = (usuariosA, [], [publicacionDosVecesMeGusta])

-- redLesGustanLasMismasPublicaciones2 (usuario1 y usuario2 sin likes)
publicacionesLesGustanLasMismasPublicaciones2 = [publicacion2_1, publicacion2_4, publicacion5_4]
redLesGustanLasMismasPublicaciones2 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones2)

-- redLesGustanLasMismasPublicaciones3 (compara mismo usuario)
publicacionesLesGustanLasMismasPublicaciones3 = [publicacion2_2, publicacion2_4, publicacion1_2, publicacion4_4]
redLesGustanLasMismasPublicaciones3 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones3)

-- redLesGustanLasMismasPublicaciones4 (comun, verdadero)
publicacionesLesGustanLasMismasPublicaciones4 = [publicacion2_1, publicacion4_2, publicacion3_4]
redLesGustanLasMismasPublicaciones4 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones4)

-- redLesGustanLasMismasPublicaciones5 (comun, falso)
publicacionesLesGustanLasMismasPublicaciones5 = [publicacion2_1, publicacion4_2, publicacion3_4, publicacion1_2]
redLesGustanLasMismasPublicaciones5 = (usuariosA, [], publicacionesLesGustanLasMismasPublicaciones5)

-- redTieneUnSeguidorFiel2 (usuario no tiene publicaciones)
publicacionesTieneUnSeguidorFiel2 = [publicacion2_1, publicacion2_4, publicacion5_4]
redTieneUnSeguidorFiel2 = (usuariosA, [], publicacionesTieneUnSeguidorFiel2)

-- redAutoLikes (usuario es su 'seguidor fiel')
pub1 = (usuario1, "Este es mi primer post", [usuario2, usuario1])
pub2 = (usuario1, "Lorem ipsum dolor sit amet", [usuario5, usuario4, usuario1])
pub3 = (usuario1, "Te estas portando mal, seras debuggeada", [usuario1])
publicacionesAutoLikes = [pub1, pub2, pub3]
redAutoLikes = (usuariosA, [], publicacionesAutoLikes)

-- redTieneUnSeguidorFiel5 (comun, verdadero)
publicacionesTieneUnSeguidorFiel5 = [publicacion5_1, publicacion5_2, publicacion5_3]
redTieneUnSeguidorFiel5 = (usuariosA, [], publicacionesTieneUnSeguidorFiel5)

-- redTieneUnSeguidorFiel6 (comun, falso)
publicacionesTieneUnSeguidorFiel6 = [publicacion5_1, publicacion5_2, publicacion5_3, publicacion5_4]
redTieneUnSeguidorFiel6 = (usuariosA, [], publicacionesTieneUnSeguidorFiel6)


-- USUARIOS
usuario1 = (1, "Juli")
usuario2 = (2, "Santi")
usuario3 = (3, "Luca")
usuario4 = (4, "Diego")
usuario5 = (5, "Diego")
usuario6 = (6, "Juan")
usuario7 = (7, "Pedro")
usuario8 = (8, "Pablo")
usuario9 = (9, "Maria")
usuario10 = (10, "Ana")
usuario11 = (11, "Sofia")
usuario12 = (12, "Florencia")


--PUBLIACIONES
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2])
publicacion1_2 = (usuario1, "Lorem ipsum dolor sit amet", [usuario5, usuario4])
publicacion1_3 = (usuario1, "Vivamus et risus at purus pharetra", [usuario4, usuario2, usuario5, usuario3])
publicacion1_4 = (usuario1, "Maecenas pellentesque nulla vel", [usuario3])
publicacion1_5 = (usuario1, "Batata y queso sin nulla vel", [])
publicacion1_6 = (usuario1, "Estoy en soledad", []) -- otra mas sin likes


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