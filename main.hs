-- DATOS Y SHOW
type Pixel = (Integer, Integer, Integer)
type PixelDelta = (Integer, Integer, Integer)
type Frame = [[Pixel]]

data Video = Iniciar Frame | Agregar Frame Video deriving Eq
instance Show Video
   where show (Iniciar f) = mostrarFrame f
         show (Agregar f v) = (mostrarFrame f) ++ "\n" ++ (show v)

type FrameComprimido = [(Integer, Integer, PixelDelta)]
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
pixelsDiferentesEnFrame frameBase frameActual u = pixelsDiferentesEnFrame' frameBase frameActual u (0, 0)

pixelsDiferentesEnFrame' :: Frame -> Frame -> Float -> Posicion -> FrameComprimido
pixelsDiferentesEnFrame' [] _ _ _ = []
pixelsDiferentesEnFrame' ([]:filas) (_:filas') u (fila, _) = pixelsDiferentesEnFrame' filas filas' u (fila+1, 0)
pixelsDiferentesEnFrame' ((px:pxs):filas) ((px':px's):filas') u (fila, columna)
	| norma pixelDelta > u = (fila, columna, pixelDelta) : pixelsDiferentesDelRestoDelFrame
	| otherwise = pixelsDiferentesDelRestoDelFrame
	where
		pixelsDiferentesDelRestoDelFrame = pixelsDiferentesEnFrame' (pxs:filas) (px's:filas') u (fila, columna+1)
		pixelDelta = diferenciaPixeles px' px

diferenciaPixeles :: Pixel -> Pixel -> PixelDelta
diferenciaPixeles (r, g, b) (r', g', b') = (r-r', g-g', b-b')

-- Ejercicio 4/5
type CompresionParcial = (Frame, VideoComprimido) -- el frame es el último agregado al video comprimido y se usa para comparar
type EstadoCompresion = (CompresionParcial, Video) -- el video es lo que resta procesar

procesarFrame :: Frame -> CompresionParcial -> Float -> Integer -> CompresionParcial
procesarFrame frame (frameBase, videoComprimido) u n
	| sonFramesMuyDistintos = (frame, (AgregarNormal frame videoComprimido))
	| otherwise = (frameBase, (AgregarComprimido frameComprimido videoComprimido))
	where
		frameComprimido = pixelsDiferentesEnFrame frameBase frame u
		sonFramesMuyDistintos = (fromIntegral $ length frameComprimido) > n
	
comprimir :: Video -> Float -> Integer -> VideoComprimido
comprimir (Iniciar frame) _ _ = IniciarComp frame
comprimir (Agregar frame video) u n = comprimir' ((frame, (IniciarComp frame)), video) u n
	
comprimir' :: EstadoCompresion -> Float -> Integer -> VideoComprimido
comprimir' (compresionParcial, (Iniciar frame)) u n = 
	snd $ procesarFrame frame compresionParcial u n
comprimir' (compresionParcial, (Agregar frame video)) u n =
	comprimir' ((procesarFrame frame compresionParcial u n), video) u n

-- Ejercicio 5/5
separarFramesComprimidos :: VideoComprimido -> ([FrameComprimido], VideoComprimido)
separarFramesComprimidos videoComprimido@(IniciarComp _) = ([], videoComprimido)
separarFramesComprimidos videoComprimido@(AgregarNormal _ _) = ([], videoComprimido)
separarFramesComprimidos (AgregarComprimido frameComprimido videoComprimido) =
	let (framesComprimidosDelTail, restoDelVideo) = separarFramesComprimidos videoComprimido
	in (frameComprimido : framesComprimidosDelTail, restoDelVideo)

-- Asume que el siguiente frame no está comprimido porque es para un contexto especial de descompresión
obtenerFrame :: VideoComprimido -> Frame
obtenerFrame (IniciarComp frame) = frame
obtenerFrame (AgregarNormal frame videoComprimido) = frame

-- Invierte el orden de los frames
agregarFrames :: [Frame] -> Video -> Video
agregarFrames [] video = video
agregarFrames (frame:frames) video = agregarFrames frames (Agregar frame video)

descomprimir :: VideoComprimido -> Video
descomprimir (IniciarComp frame) = Iniciar frame
descomprimir (AgregarNormal frame videoComprimido) = descomprimir' videoComprimido (Iniciar frame)
descomprimir videoComprimido@(AgregarComprimido _ _) = 
	let (framesComprimidos, restoDelVideoComprimido) = separarFramesComprimidos videoComprimido in
	let (frame:frames) = map (aplicarCambio $ obtenerFrame restoDelVideoComprimido) framesComprimidos
	in descomprimir' restoDelVideoComprimido (agregarFrames frames (Iniciar frame))

descomprimir' :: VideoComprimido -> Video -> Video
descomprimir' (IniciarComp frame) video = Agregar frame video
descomprimir' (AgregarNormal frame videoComprimido) video = descomprimir' videoComprimido (Agregar frame video)
descomprimir' videoComprimido@(AgregarComprimido _ _) video =
	let ([framesComprimidos], restoDelVideoComprimido) = separarFramesComprimidos videoComprimido in
	let frames = map (aplicarCambio $ obtenerFrame restoDelVideoComprimido) [framesComprimidos]
	in descomprimir' restoDelVideoComprimido (agregarFrames frames video)

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

-- Video 1:  En la versión comprimida, todos los frames son comprimidos (salvo el inicial)

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

												

