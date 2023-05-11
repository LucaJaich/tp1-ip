module Solucion where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--Ejercicio 1

--pertenece recibe un elemento y una lista y devuelve True si el elemento esta incluido en la lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

--sinRepetidos recibe una lista y te la devuelve sin ningun repetidos
sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = (sinRepetidos xs)
                    | otherwise = x : (sinRepetidos xs)

--listaDeNombres recibe una lista de usuarios y devuelve una lista con esos nombres sin los Ids
listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres (u:us) = (nombreDeUsuario u) : (listaDeNombres us) 

--nombresDeUsuarios recibe una red social y devuelve los nombres de usuarios 
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = sinRepetidos (listaDeNombres (usuarios rs))

--Ejercicio 2

encontrarRelaciones :: [Relacion] -> Usuario -> [Usuario]
encontrarRelaciones [] _ = []
encontrarRelaciones (r:rs) u | u == fst r = (snd r) : (encontrarRelaciones rs u)
                             | u == snd r = (fst r) : (encontrarRelaciones rs u)
                             | otherwise = encontrarRelaciones rs u

-- describir qué hace la función: Devuelve los amigos (usuarios con los que se relaciono) del Usuario en esta Red Social
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs u = encontrarRelaciones (relaciones rs) u

--Ejercicio 3

--largoDe recibe una lista y te devuelve su longitud
largoDe :: [t] -> Int
largoDe [] = 0
largoDe (x:xs) = 1 + largoDe xs

-- describir qué hace la función: Devuelve la cantidad de amigos que tiene el Usuario en esta Red Social
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = largoDe (amigosDe rs u)

--Ejercicio 4

--masAmigos recibe una red social y una lista con los usuarios de esa red social y nos devuelve quien tiene mas amigos
masAmigos :: RedSocial -> [Usuario] -> Usuario
masAmigos rs (x:xs) | xs == [] = x
                    | cantidadDeAmigos rs x > cantidadDeAmigos rs (masAmigos rs xs) = x
                    | otherwise = masAmigos rs xs

-- describir qué hace la función: recibe una red social y nos dice quien es el usuario que tiene mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = masAmigos rs (usuarios rs)

--Ejercicio 5

-- describir qué hace la función: queremos saber si en esta Red Social alguien tiene mas de un millon de amigos 
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos red) > 10 = True
                      | otherwise = False

{-
-- describir qué hace la función: Devuelve las publicaciones del usuario que nos interesa en la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = buscoPublicaciones red usuario (publicaciones red) []

buscoPublicaciones :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion] -> [Publicacion]
buscoPublicaciones red usuario publicaciones publicaciones_usuario_interes| publicaciones == [] = publicaciones_usuario_interes
                                                                          | usuarioDePublicacion (head publicaciones) == usuario = buscoPublicaciones red usuario (tail publicaciones) (publicaciones_usuario_interes ++ [head publicaciones])
                                                                          | otherwise = buscoPublicaciones red usuario (tail publicaciones) publicaciones_usuario_interes
-}

-- describir qué hace la función: Devuelve las publicaciones del usuario que nos interesa en la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usuario = buscoPublicaciones red usuario (publicaciones red)

buscoPublicaciones :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion]
buscoPublicaciones red us (x:xs) | xs == [] && usuarioDePublicacion x == us = xs
                                 | xs == [] = []
                                 | usuarioDePublicacion x == us = x : buscoPublicaciones red us xs
                                 | otherwise = buscoPublicaciones red us xs

-- describir qué hace la función: Devuelve las publicaciones que le gustaron al usuario que nos interesa
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red usuario = buscoLikes red usuario (publicaciones red) []

buscoLikes :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion] -> [Publicacion]
buscoLikes red usuario publicaciones publicaciones_que_le_gustaron | publicaciones == [] = publicaciones_que_le_gustaron
                                                                   | pertenece usuario (likesDePublicacion (head publicaciones)) = buscoLikes red usuario (tail publicaciones) (publicaciones_que_le_gustaron ++ [head publicaciones])
                                                                   | otherwise = buscoLikes red usuario (tail publicaciones) publicaciones_que_le_gustaron 


-- describir qué hace la función: Es verdadero si los dos usuarios le dieron me gusta a las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red usuario1 usuario2 | mismosElementos (publicacionesQueLeGustanA red usuario1) (publicacionesQueLeGustanA red usuario2) = True
                                                      | otherwise = False

mismosElementos :: [Publicacion] -> [Publicacion] -> Bool
mismosElementos publicaciones_usuario1 publicaciones_usuario2 | estaIncluida publicaciones_usuario1 publicaciones_usuario2, estaIncluida publicaciones_usuario2 publicaciones_usuario1 = True
                                                              | otherwise = False

estaIncluida :: [Publicacion] -> [Publicacion] -> Bool
estaIncluida publicaciones_usuario1 publicaciones_usuario2 | publicaciones_usuario1 == [] = True
                                                           | pertenece (head publicaciones_usuario1) publicaciones_usuario2 = estaIncluida (tail publicaciones_usuario1) publicaciones_usuario2
                                                           | otherwise = False

-- describir qué hace la función: Es verdadero si existe un usuario que le dio me gusta en todas sus publicaciones
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red usuario | publicacionesDe red usuario == [] = False
                                | otherwise = veoMeGustas red usuario (publicacionesDe red usuario) (amigosDe red usuario)

veoMeGustas :: RedSocial -> Usuario -> [Publicacion] -> [Usuario] -> Bool
veoMeGustas red usuario publicaciones_del_usuario amigos | amigos == [] = False
                                                         | publicaciones_del_usuario == [] = True
                                                         | pertenece (head amigos) (likesDePublicacion (head publicaciones_del_usuario)) = veoMeGustas red usuario (tail publicaciones_del_usuario) amigos
                                                         | otherwise = veoMeGustas red usuario (publicacionesDe red usuario) (tail amigos)
                                        

-- describir qué hace la función: Es verdadero si los dos usuarios tienen algun amigo en comun
--un usuario tiene amigos en comun con si mismo???
--si ese usuario tiene algun amigo da true. si no tiene ningun amigo pero pertenece a la red social da false
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2 | tienenAmigosEnComun (amigosDe red usuario1 ++ amigosDe red usuario2) = True
                                              | otherwise = False

tienenAmigosEnComun :: (Eq t) => [t] -> Bool
tienenAmigosEnComun [] = False
tienenAmigosEnComun (x:xs) | estaRepetido x xs == True = tienenAmigosEnComun xs
                           | otherwise = True

estaRepetido :: (Eq t) => t -> [t] -> Bool
estaRepetido x xs | xs == [] = True
                  | x == head xs = False
                  | otherwise = estaRepetido x (tail xs)

{-
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Julieta")
usuario7 = (7, "Luca")
usuario8 = (8, "Diego")
usuario9 = (9, "Santiago")
usuario10 = (10, "Martina")
usuario11 = (11, "Sofia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion3_5 = (usuario3, usuario5)
relacion3_6 = (usuario3, usuario6)
relacion3_7 = (usuario3, usuario7)
relacion3_8 = (usuario3, usuario8)
relacion3_9 = (usuario3, usuario9)
relacion3_10 = (usuario3, usuario10)
relacion3_11 = (usuario3, usuario11)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1]
relacionesC = []
publicacionesC = [publicacion1_4]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11]
relacionesD = [relacion1_3, relacion2_3 , relacion3_5, relacion3_4, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11]
publicacionesD = [publicacion3_2]
redD = (usuariosD, relacionesD, publicacionesD)


redVacia = ([],[],[])

-}