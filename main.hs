-- DATOS Y SHOW
type Pixel = (Integer, Integer, Integer)
type PixelDelta = (Integer, Integer, Integer)
type Frame = [[Pixel]]

data Video = Iniciar Frame | Agregar Frame Video deriving Eq
instance Show Video
   where show (Iniciar f) = mostrarFrame f
         show (Agregar f v) = (mostrarFrame f) ++ "\n" ++ (show v)

type FrameComprimido = [(Integer, Integer, PixelDelta)]

-- Limitación encontrada: si un video comprimido es una lista de frames (comprimidos o no), para la lista así modelada es imposible definir una función reverse similar a las listas nativas, ya que carecemos de un constructor que acepte como primer frame un frame comprimido.
-- Como no tiene sentido un video comprimido que contenga solo frames comprimidos, una solución posible es modelarlo así:
-- type PaqueteFrames = ([FrameComprimido], Frame)
-- data VideoComprimido = IniciarComp Frame | AgregarNormal Frame VideoComprimido | IniciarPaq PaqueteFrames | AgregarPaq PaqueteFrames VideoComprimido
-- Esto además facilita la descompresión si la compresión fue implementada comparando contra el último frame no comprimido.

data VideoComprimido = IniciarComp Frame | AgregarNormal Frame VideoComprimido | AgregarComprimido FrameComprimido VideoComprimido
instance Show VideoComprimido
   where show (IniciarComp f) = "INICIAL \n" ++ mostrarFrame f
         show (AgregarNormal f v) = "NO COMPRIMIDO \n" ++ (mostrarFrame f) ++ "\n" ++ (show v)
         show (AgregarComprimido f v) = "COMPRIMIDO \n" ++ (mostrarFrameComprimido f) ++ "\n" ++ (show v)

mostrarFrame :: Frame -> String
mostrarFrame [] = ""
mostrarFrame (x:xs) = (show x) ++ "\n" ++ (mostrarFrame xs)

mostrarFrameComprimido :: FrameComprimido -> String
mostrarFrameComprimido [] = ""
mostrarFrameComprimido (x:xs) = "\t" ++ (show x) ++ "\n" ++ (mostrarFrameComprimido xs)

-- Ejercicio 1/5
ultimoFrame :: Video -> Frame
ultimoFrame (Iniciar frame) = frame
ultimoFrame (Agregar frame video) = frame

-- Ejercicio 2/5
norma :: (Integer, Integer, Integer) -> Float
norma (x, y, z) = sqrt $ fromInteger (x^2 + y^2 + z^2)

-- Ejercicio 3/5
type Posicion = (Integer, Integer)

pixelsDiferentesEnFrame :: Frame -> Frame -> Float -> FrameComprimido
pixelsDiferentesEnFrame frameBase frameActual u = comprimirFrame frameBase frameActual u (0, 0)

comprimirFrame :: Frame -> Frame -> Float -> Posicion -> FrameComprimido
comprimirFrame [] _ _ _ = []
comprimirFrame ([]:filas) (_:filas') u (fila, _) = comprimirFrame filas filas' u (fila+1, 0)
comprimirFrame ((px:pxs):filas) ((px':px's):filas') u (fila, columna)
	| norma pixelDelta > u = (fila, columna, pixelDelta) : restoDelFrameComprimido
	| otherwise = restoDelFrameComprimido
	where
		restoDelFrameComprimido = comprimirFrame (pxs:filas) (px's:filas') u (fila, columna+1)
		pixelDelta = diferenciaPixeles px' px

diferenciaPixeles :: Pixel -> Pixel -> PixelDelta
diferenciaPixeles (r, g, b) (r', g', b') = (r-r', g-g', b-b')

-- Ejercicio 4/5
-- Al decidir si comprimir o no un frame, se compara al mismo con el último frame no comprimido.
comprimir :: Video -> Float -> Integer -> VideoComprimido
comprimir (Iniciar frame) _ _ = IniciarComp frame
comprimir video u n = fst $ comprimir' video u n

comprimir' :: Video -> Float -> Integer -> (VideoComprimido, Frame)
comprimir' (Iniciar frame) _ _ = ((IniciarComp frame), frame)
comprimir' (Agregar frame video) u n =
	let
		(videoComprimido, frameBase) = comprimir' video u n
		frameComprimido = pixelsDiferentesEnFrame frameBase frame u
		sonFramesMuyDistintos = (fromIntegral $ length frameComprimido) > n
	in
		if sonFramesMuyDistintos
		then ((AgregarNormal frame videoComprimido), frame)
		else ((AgregarComprimido frameComprimido videoComprimido), frameBase)

-- Ejercicio 5/5
separarFramesComprimidos :: VideoComprimido -> ([FrameComprimido], VideoComprimido)
separarFramesComprimidos videoComprimido@(IniciarComp _) = ([], videoComprimido)
separarFramesComprimidos videoComprimido@(AgregarNormal _ _) = ([], videoComprimido)
separarFramesComprimidos (AgregarComprimido frameComprimido videoComprimido) =
	let (siguientesFramesComprimidos, restoDelVideo) = separarFramesComprimidos videoComprimido
	in (frameComprimido : siguientesFramesComprimidos, restoDelVideo)

obtenerFrame :: VideoComprimido -> Frame
obtenerFrame (IniciarComp frame) = frame
obtenerFrame (AgregarNormal frame videoComprimido) = frame

agregarFrames :: [Frame] -> Video -> Video
agregarFrames [] video = video
agregarFrames (frame:frames) video = Agregar frame $ agregarFrames frames video

descomprimir :: VideoComprimido -> Video
descomprimir (IniciarComp frame) = Iniciar frame
descomprimir (AgregarNormal frame videoComprimido) = Agregar frame $ descomprimir videoComprimido
descomprimir videoComprimido@(AgregarComprimido _ _) = 
	let
		(framesComprimidos, restoDelVideoComprimido) = separarFramesComprimidos videoComprimido
		framesDescomprimidos = map (aplicarCambio $ obtenerFrame restoDelVideoComprimido) framesComprimidos
	in agregarFrames framesDescomprimidos $ descomprimir restoDelVideoComprimido

-- Funciones provistas por la cátedra
sumarCambios :: FrameComprimido -> FrameComprimido -> FrameComprimido
sumarCambios fc1 fc2 = [(i, j, sumar deltas (busqueda i j fc2)) | (i, j, deltas) <- fc1] ++
                       [(i, j, deltas) | (i, j, deltas) <- fc2, busqueda i j fc1 == (0,0,0)]
-- *Main> sumarCambios [(1,1,(2,2,2)),(2,2,(0,0,-1))] [(1,1,(-3,-3,-3)), (1,2,(1,1,1))]
-- [(1,1,(-1,-1,-1)),(2,2,(0,0,-1)),(1,2,(1,1,1))]

aplicarCambio :: Frame -> FrameComprimido -> Frame
aplicarCambio f fc = [ [nuevoVal f i j fc| j <- [0..length (f !! i) - 1]] | i <- [0..length f - 1]]
  where nuevoVal f i j fc = sumar ((f !! i) !! j) (busqueda (fromIntegral i) (fromIntegral j) fc)
--  *Main> aplicarCambio [[(1,1,1),(2,2,2)],[(3,3,3),(4,4,4)]] [(0, 1, (1,2,3))]
--  [[(1,1,1),(3,4,5)],[(3,3,3),(4,4,4)]]

busqueda :: Integer -> Integer -> FrameComprimido -> PixelDelta
busqueda i j [] = (0, 0, 0)
busqueda i j ((x, y, c) : cs) | x == i && j == y = c
                            | otherwise = busqueda i j cs

sumar :: PixelDelta -> PixelDelta -> PixelDelta
sumar (x,y,z) (x2,y2,z2) =  (x+x2,y+y2,z+z2)

-- PRUEBAS
p3 :: Pixel
p3 = (3,3,3)

p0 :: Pixel
p0 = (0,0,0)

-- Video 0:
f0 = [[p0, p0, p0], [p3, p3, p3]]
f1 = [[p3, p3, p3], [p3, p3, p3]]
video0 = Agregar f1 (Agregar f0 (Iniciar f0))

-- Video 1:  En la versión comprimida, todos los frames son comprimidos (salvo el inicial).
-- ACLARACIÓN: Son comprimidos solo los frames 2 y 4 por la técnica utilizada -ver ejercicio 4-, en contraposición a comparar siempre frames contiguos

v1f1 :: Frame
v1f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f2 :: Frame
v1f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v1f3 :: Frame
v1f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v1f4 :: Frame
v1f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v1 :: Video
v1 = Agregar v1f4 (Agregar v1f3 (Agregar v1f2 (Iniciar v1f1)))

v1Comp :: VideoComprimido
v1Comp = comprimir v1 1 6


-- Video 2:  En la versión comprimida, sólo los frames 2 y 4 son comprimidos

v2f1 :: Frame
v2f1 = [[p3, p3, p0, p0, p0],
	  [p3, p3, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f2 :: Frame
v2f2 = [[p0, p0, p0, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p3, p3, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0]]

v2f3 :: Frame
v2f3 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p3, p3, p3],
	  [p0, p0, p3, p3, p0],
	  [p0, p0, p0, p0, p0]]

v2f4 :: Frame
v2f4 = [[p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p0],
	  [p0, p0, p0, p0, p3],
	  [p0, p0, p0, p3, p3],
	  [p0, p0, p0, p3, p3]]


v2 :: Video
v2 = Agregar v2f4 (Agregar v2f3 (Agregar v2f2 (Iniciar v2f1)))

v2Comp :: VideoComprimido
v2Comp = comprimir v2 1 6

												

