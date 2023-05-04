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


----------------- EJ 1 ------------------------

existeEnLista :: String -> [String] -> Bool -- Tambien hacerlo genérico
existeEnLista _ [] = False
existeEnLista n (x:xs) | n == x = True
                       | otherwise = existeEnLista n xs

sinRepetidos :: [String] -> [String] -- TODO! Como lo hago generico?
sinRepetidos [] = []
sinRepetidos (x:xs) | existeEnLista x xs = (sinRepetidos xs)
                    | otherwise = x : (sinRepetidos xs)

listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres (u:us) = (nombreDeUsuario u) : (listaDeNombres us) 

-- describir qué hace la función: .....
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = sinRepetidos (listaDeNombres (usuarios rs))

-----------------------------------------------
----------------- EJ 2 ------------------------

encontrarRelaciones :: [Relacion] -> Usuario -> [Usuario]
encontrarRelaciones [] _ = []
encontrarRelaciones (r:rs) u | u == fst r = (snd r) : (encontrarRelaciones rs u)
                             | u == snd r = (fst r) : (encontrarRelaciones rs u)
                             | otherwise = encontrarRelaciones rs u

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs u = encontrarRelaciones (relaciones rs) u

-----------------------------------------------
----------------- EJ 3 ------------------------

largoDe :: [t] -> Int
largoDe [] = 0
largoDe (x:xs) = 1 + largoDe xs

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = largoDe (amigosDe rs u)

-----------------------------------------------

listaDeCantidadesDeAmigos :: [Relacion] -> [Int]

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
