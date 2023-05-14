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

nombresDeUsuarios :: RedSocial -> [String] -- Lista de nombres en red pero repetidos
nombresDeUsuarios red | null (usuarios red) = []
                      | otherwise = nombreDeUsuario (head (usuarios red)) : nombresDeUsuarios (tail (usuarios red),relaciones red, publicaciones red)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us | null (relaciones red) = []
                | us == fst (head (relaciones red)) = snd (head (relaciones red)) : amigosDe (usuarios red,tail (relaciones red), publicaciones red) us
                | us == snd (head (relaciones red)) = fst (head (relaciones red)) : amigosDe (usuarios red,tail (relaciones red), publicaciones red) us
                | otherwise = amigosDe (usuarios red,tail (relaciones red), publicaciones red) us
-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us = longitud (amigosDe red us)

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario -- consigue la maxima cant de amigos de la red y la compara con la cant de amigos de cada usuario para averiguar a quien le pertenece 
usuarioConMasAmigos red | maximo (cantAmigosRed red) == cantidadDeAmigos red (head (usuarios red)) = head (usuarios red)
                        | otherwise = usuarioConMasAmigos (tail (usuarios red),relaciones red, publicaciones red)

cantAmigosRed :: RedSocial -> [Int] -- lista de cantidad de amigos en la red por ordenados por orden de lista de usuarios
cantAmigosRed red | null (usuarios red) = []
              | otherwise = cantidadDeAmigos red (head (usuarios red)) : cantAmigosRed (tail (usuarios red),relaciones red, publicaciones red)

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | null (usuarios red) = False
                      | cantidadDeAmigos red (head (usuarios red)) >=  10 = True
                      | otherwise =  estaRobertoCarlos (tail (usuarios red),relaciones red, publicaciones red)

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us | null(publicaciones red) = [] 
                       | us == usuarioDePublicacion(head(publicaciones red)) = head(publicaciones red) : publicacionesDe(usuarios red, relaciones red, tail(publicaciones red)) us
                       | otherwise = publicacionesDe(usuarios red, relaciones red, tail(publicaciones red)) us

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us | null (publicaciones red) = []
                                 | pertenece us (likesDePublicacion (head (publicaciones red))) = head(publicaciones red) : publicacionesQueLeGustanA(usuarios red, relaciones red, tail (publicaciones red)) us
                                 | otherwise = publicacionesQueLeGustanA(usuarios red, relaciones red, tail (publicaciones red)) us


-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined



--- Funciones Auxiliares

pertenece :: (Eq t)=> t ->[t]-> Bool
pertenece e l | null l = False
              | head l == e = True
              | otherwise = pertenece e (tail l)

longitud :: [t] -> Int
longitud n | null n = 0
           | otherwise = longitud (tail n) + 1

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = max x (maximo xs)
