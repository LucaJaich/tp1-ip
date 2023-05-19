-- Completar con los datos del grupo
--
-- Nombre de Grupo: Esternocleidomastoideo
-- Integrante 1: Luca Jaichenco, jaichencol@gmail.com, 591/22
-- Integrante 2: Julieta Cavalieri, julieta_cavalieri@yahoo.com, 1816/21
-- Integrante 3: Santiago Bassani, santiagobassani2000@gmail.com, 211/23
-- Integrante 4: Diego Lima Zecconi, diego.lima.zecconi@gmail.com, 839/22


module Solucion where

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


----------------- EJ 1 ------------------------

-- describir qué hace la función: Devuelve una lista con los nombres de los usuarios de la red social, sin repetir
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = sinRepetidos (listaDeNombres (usuarios rs))

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres (u:us) = (nombreDeUsuario u) : (listaDeNombres us)

sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = (sinRepetidos xs)
                    | otherwise = x : (sinRepetidos xs)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

-----------------------------------------------
----------------- EJ 2 ------------------------

-- describir qué hace la función: Devuelve una lista con los amigos de un usuario, o sea con los 
--                                usuarios con los que se relaciona
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs u = encontrarRelaciones (relaciones rs) u

encontrarRelaciones :: [Relacion] -> Usuario -> [Usuario]
encontrarRelaciones [] _ = []
encontrarRelaciones (r:rs) u | u == fst r = (snd r) : (encontrarRelaciones rs u)
                             | u == snd r = (fst r) : (encontrarRelaciones rs u)
                             | otherwise = encontrarRelaciones rs u

-----------------------------------------------
----------------- EJ 3 ------------------------

-- describir qué hace la función: Devuelve la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = largoDe (amigosDe rs u)

largoDe :: [t] -> Int
largoDe [] = 0
largoDe (x:xs) = 1 + largoDe xs

-----------------------------------------------
----------------- Ej 4 ------------------------

-- describir qué hace la función: Devuelve el usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = masAmigos rs (usuarios rs)

masAmigos :: RedSocial -> [Usuario] -> Usuario
masAmigos rs (x:xs) | xs == [] = x
                    | cantidadDeAmigos rs x > cantidadDeAmigos rs (masAmigos rs xs) = x
                    | otherwise = masAmigos rs xs

-----------------------------------------------
----------------- Ej 5 ------------------------

-- describir qué hace la función: Devuelve True si el usuario con mas amigos tiene mas de 10 amigos 
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | usuarios rs == [] = False
                     | cantidadDeAmigos rs (usuarioConMasAmigos rs) > 10 = True
                     | otherwise = False

-----------------------------------------------
----------------- Ej 6 ------------------------

-- describir qué hace la función: Devuelve una lista con las publicaciones de un usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rs u = publicacionesPorUsuario (publicaciones rs) u

publicacionesPorUsuario :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesPorUsuario [] _ = []
publicacionesPorUsuario (p:ps) u | (usuarioDePublicacion p) == u = p : publicacionesPorUsuario ps u
                                 | otherwise = publicacionesPorUsuario ps u

-----------------------------------------------
----------------- Ej 7 ------------------------

-- describir qué hace la función: Devuelve una lista con las publicaciones que le gustan a un usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rs u = sinRepetidos (publicacionesQueLeGustanA2 (publicaciones rs) u)

publicacionesQueLeGustanA2 :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanA2 [] _ = []
publicacionesQueLeGustanA2 (p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA2 ps u
                                    | otherwise = publicacionesQueLeGustanA2 ps u

-----------------------------------------------
----------------- Ej 8 ------------------------

-- describir qué hace la función: Devuelve True si dos usuarios tienen las mismas publicaciones que 
--                                les gustan, no importa el orden
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = (todosPertenecen pubsU1 pubsU2) && (todosPertenecen pubsU2 pubsU1)
                                where pubsU1 = publicacionesQueLeGustanA rs u1 
                                      pubsU2 = publicacionesQueLeGustanA rs u2

todosPertenecen :: (Eq t) => [t] -> [t] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) ys | pertenece x ys = todosPertenecen xs ys
                          | otherwise = False

-----------------------------------------------
----------------- Ej 9 ------------------------

-- describir qué hace la función: Devuelve True si un usuario tiene al menos un seguidor que le gustan 
--                                todas sus publicaciones
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs u | largoDe (publicacionesDe rs u) < 1 = False
                         | otherwise = esAlgunoSeguidorFiel rs (usuarios rs) u

esAlgunoSeguidorFiel :: RedSocial -> [Usuario] -> Usuario -> Bool
esAlgunoSeguidorFiel _ [] _ = False
esAlgunoSeguidorFiel rs (u:us) u1 | (leGustanTodas u (publicacionesDe rs u1)) && (u /= u1) = True
                                  | otherwise = esAlgunoSeguidorFiel rs us u1

leGustanTodas :: Usuario -> [Publicacion] -> Bool
leGustanTodas u [] = True
leGustanTodas u (p:ps) = pertenece u (likesDePublicacion p) && leGustanTodas u ps

-----------------------------------------------
----------------- Ej 10 ------------------------

-- describir qué hace la función: Devuelve True si existe una secuencia de amigos que conecte a dos usuarios, 
--                                la secuencia tiene que tener al menos 2 relaciones
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 | (u1 == u2) && ((cantidadDeAmigos rs u1) > 0) = True
                                 | otherwise = analizarAmigos rs (amigosDe rs u1) u2 []

analizarAmigos :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
analizarAmigos _ [] _ _ = False
analizarAmigos rs (amigo:amigos) uObj usVistos | pertenece amigo usVistos = analizarAmigos rs amigos uObj usVistos
                                               | amigo == uObj = True
                                               | otherwise = (analizarAmigos rs (amigosDe rs amigo) uObj nuevosUs) 
                                                 || (analizarAmigos rs amigos uObj nuevosUs)
                                                 where nuevosUs = amigo : usVistos