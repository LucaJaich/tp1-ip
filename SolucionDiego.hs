module SolucionDiego where


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

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red usu | null (relaciones red)  = []
                 | usu == fst(head(relaciones red)) = snd(head(relaciones red)) : amigosDe recuRelaciones usu
                 | usu == snd(head(relaciones red)) = fst(head(relaciones red)) : amigosDe recuRelaciones usu
                 | otherwise = amigosDe recuRelaciones usu

                 where recuRelaciones = (usuarios red, tail (relaciones red), publicaciones red) 



-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red usu = longitud (amigosDe red usu)

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red usu | null (publicaciones red) = [] 
                        | usu == usuarioPubli = publicacionUsu : publicacionesDe recuPublicaciones usu
                        | otherwise = publicacionesDe recuPublicaciones usu 

                        where usuarioPubli = usuarioDePublicacion(head(publicaciones red))
                              publicacionUsu = (head(publicaciones red))
                              recuPublicaciones = (usuarios red, relaciones red, tail(publicaciones red))

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red usu | null (publicaciones red) = []
                                  | pertenece usu usuariosLike = publicacionLike : publicacionesQueLeGustanA recuLikePubli usu
                                  | otherwise = publicacionesQueLeGustanA recuLikePubli usu

                                  where usuariosLike = likesDePublicacion(head(publicaciones red))
                                        publicacionLike = (head(publicaciones red))
                                        recuLikePubli = (usuarios red, relaciones red, tail(publicaciones red))


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

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usu1 usu2 = undefined


