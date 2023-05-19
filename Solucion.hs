-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU


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

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = (sinRepetidos xs)
                    | otherwise = x : (sinRepetidos xs)

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres (u:us) = (nombreDeUsuario u) : (listaDeNombres us) 

-- describir qué hace la función: Devuelve una lista con los nombres de los usuarios de la red social, sin repetir
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = sinRepetidos (listaDeNombres (usuarios rs))

-----------------------------------------------
----------------- EJ 2 ------------------------

encontrarRelaciones :: [Relacion] -> Usuario -> [Usuario]
encontrarRelaciones [] _ = []
encontrarRelaciones (r:rs) u | u == fst r = (snd r) : (encontrarRelaciones rs u)
                             | u == snd r = (fst r) : (encontrarRelaciones rs u)
                             | otherwise = encontrarRelaciones rs u

-- describir qué hace la función: Devuelve una lista con los amigos de un usuario, o sea con los 
--                                usuarios con los que se relaciona
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs u = encontrarRelaciones (relaciones rs) u

-----------------------------------------------
----------------- EJ 3 ------------------------

largoDe :: [t] -> Int
largoDe [] = 0
largoDe (x:xs) = 1 + largoDe xs

-- describir qué hace la función: Devuelve la cantidad de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = largoDe (amigosDe rs u)

-----------------------------------------------
----------------- Ej 4 ------------------------

masAmigos :: RedSocial -> [Usuario] -> Usuario
masAmigos rs (x:xs) | xs == [] = x
                    | cantidadDeAmigos rs x > cantidadDeAmigos rs (masAmigos rs xs) = x
                    | otherwise = masAmigos rs xs

-- describir qué hace la función: Devuelve el usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = masAmigos rs (usuarios rs)

-----------------------------------------------
----------------- Ej 5 ------------------------

-- describir qué hace la función: Devuelve True si el usuario con mas amigos tiene mas de 10 amigos 
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) > 10 = True
                     | otherwise = False

-----------------------------------------------
----------------- Ej 6 ------------------------

publicacionesPorUsuario :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesPorUsuario [] _ = []
publicacionesPorUsuario (p:ps) u | (usuarioDePublicacion p) == u = p : publicacionesPorUsuario ps u
                                 | otherwise = publicacionesPorUsuario ps u

-- describir qué hace la función: Devuelve una lista con las publicaciones de un usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rs u = publicacionesPorUsuario (publicaciones rs) u

-----------------------------------------------
----------------- Ej 7 ------------------------

publicacionesQueLeGustanA2 :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanA2 [] _ = []
publicacionesQueLeGustanA2 (p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA2 ps u
                                    | otherwise = publicacionesQueLeGustanA2 ps u

-- describir qué hace la función: Devuelve una lista con las publicaciones que le gustan a un usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rs u = publicacionesQueLeGustanA2 (publicaciones rs) u

-----------------------------------------------
----------------- Ej 8 ------------------------

todosPertenecen :: (Eq t) => [t] -> [t] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) ys | pertenece x ys = todosPertenecen xs ys
                          | otherwise = False

-- describir qué hace la función: Devuelve True si dos usuarios tienen las mismas publicaciones que 
--                                les gustan, no importa el orden
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = (todosPertenecen pubsU1 pubsU2) && (todosPertenecen pubsU2 pubsU1)
                                where pubsU1 = publicacionesQueLeGustanA rs u1 
                                      pubsU2 = publicacionesQueLeGustanA rs u2 

-----------------------------------------------
----------------- Ej 9 ------------------------

leGustanTodas :: Usuario -> [Publicacion] -> Bool
leGustanTodas u [] = True
leGustanTodas u (p:ps) = pertenece u (likesDePublicacion p) && leGustanTodas u ps

esAlgunoSeguidorFiel :: RedSocial -> [Usuario] -> Usuario -> Bool
esAlgunoSeguidorFiel _ [] _ = False
esAlgunoSeguidorFiel rs (u:us) u1 | (leGustanTodas u (publicacionesDe rs u1)) && (u /= u1) = True
                                  | otherwise = esAlgunoSeguidorFiel rs us u1

-- describir qué hace la función: Devuelve True si un usuario tiene al menos un seguidor que le gustan 
--                                todas sus publicaciones
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs u | largoDe (publicacionesDe rs u) < 1 = False
                         | otherwise = esAlgunoSeguidorFiel rs (usuarios rs) u

-----------------------------------------------
----------------- Ej 10 ------------------------

analizarAmigos :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
analizarAmigos _ [] _ _ = False
analizarAmigos rs (amigo:amigos) uObj usVistos | pertenece amigo usVistos = analizarAmigos rs amigos uObj usVistos
                                               | amigo == uObj = True
                                               | otherwise = (analizarAmigos rs (amigosDe rs amigo) uObj nuevosUs) 
                                                 || (analizarAmigos rs amigos uObj nuevosUs)
                                                 where nuevosUs = amigo : usVistos

-- describir qué hace la función: Devuelve True si existe una secuencia de amigos que conecte a dos usuarios, 
--                                la secuencia tiene que tener al menos 2 relaciones
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 | pertenece u2 (amigosDe rs u1) = False
                                 | otherwise = analizarAmigos rs (amigosDe rs u1) u2 []