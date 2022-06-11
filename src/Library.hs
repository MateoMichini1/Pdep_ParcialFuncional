module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{-
Nombre: Mateo Michini
Legajo: 175816-0
-}



-- ========= Auxiliares =======================================================================================

posicion:: (Eq a)=>a -> [a] -> Number
posicion elemento  = length.(takeWhile ((/=) elemento $) )

mapCondicional::(b -> b) -> (b -> Bool) -> [b] -> [b]
mapCondicional transformacion condicion = map (transformarSegun transformacion condicion)

transformarSegun transformacion condicion elemento
    | condicion elemento = transformacion elemento
    | otherwise = elemento

-- ========= Punto 1 =======================================================================================

-- i 

type Identificador = String

data Cuenta = Cuenta{
identificador::Identificador,
saldo::Number
}deriving (Show,Eq)

type Bloque = [(Identificador,Transaccion)]

type Transaccion = Cuenta->Cuenta

type Blockchain = [Bloque]

--ii
-- funcion auxliar para no repetir logica
intervenirSaldo  cantidad  cuenta = cuenta{ saldo = saldo cuenta + cantidad}

--funciones de transaccion

pago ::  Number ->Transaccion
pago   numero = intervenirSaldo  (-numero)  

cobranza :: Number ->Transaccion
cobranza  numero  = intervenirSaldo  numero

mineria:: Transaccion
mineria  = cobranza  25


-- ========= Punto 2 =======================================================================================

--i
punto2_i:: String -> Cuenta -> Bool
punto2_i  identificadorDado  = ((==) identificadorDado).identificador 

--ii
punto2_ii  :: (Cuenta -> Bool) ->  [Cuenta] -> Cuenta
punto2_ii condicion = head.(filter condicion)

--iii

punto2_iii :: (Cuenta -> Bool) -> [Cuenta] -> [Cuenta]
punto2_iii condicion listaDeCuentas = filter funcion listaDeCuentas 
              where posicionPrimerElemento = posicion  (punto2_ii condicion  listaDeCuentas) listaDeCuentas
                    funcion cuenta =   (posicion cuenta listaDeCuentas) /= posicionPrimerElemento 
                       
{-
Se saca el primer elemento que cumple la condicion mediante la  obtencion de su posicion con una funcion auxiliar.
Se realiza un filter   que devuelven las cuentas que no tienen dicha posicion-}

-- ========= Punto 3 =======================================================================================



punto3 identificadorDado  listaDeCuentas funcion = mapCondicional   funcion  (condicionMismoIdentificador identificadorDado $) listaDeCuentas

condicionMismoIdentificador identificadorDado  cuenta =  (((==) identificadorDado ).identificador) $ cuenta



-- ========= Punto 4 =======================================================================================


punto4 :: Bloque->[Cuenta]->[Cuenta]
punto4 bloque listaDeCuentas    = (++) ((map  (uncurry ejecucion) ) $  bloque)  (filter cuentaNoAlterada listaDeCuentas )
            where ejecucion identificador transaccion = (punto2_ii (condicionMismoIdentificador identificador)).(punto3 identificador listaDeCuentas) $    transaccion                 
                  cuentaNoAlterada cuenta             = not.( (elem.identificador) cuenta ).fst.unzip $ bloque



{-

Primero se devuelven la lista de cuentas afectadas por el bloque. Luego, las no afectadas .
Se afecta el orden.

Aclaracion
 Se unsa la funcion del puntp 2 ii yaque saumimos que solo uno cumple la condicion de tener el mismo identificador.
 Por lo cual va a ser el primero y unico.
 Por lo cual se modifica una cuenta de una lista con la funciond del punto 3 y luego se extrae esa misma con la funcion
 del punto 2 ii

 -}

 

-- ========= Punto 5 =======================================================================================

punto5 :: [Cuenta] -> Bool
punto5  =  all (( >=0).saldo $) 


-- ========= Punto 6 =======================================================================================
punto6:: Blockchain-> [Cuenta] -> Bool
punto6 blockchain  = ( foldr1 (&&)).(map (punto5) ).mapInversoBlock blockchain 
            where mapInverso [ ] _ = []
                  mapInversoBlock (bloque:bloques) listaDecuentas   = ( punto4 bloque $ listaDecuentas  ): mapInverso bloques listaDecuentas 




-- ========= Punto 7 =======================================================================================
funcionSinPudor :: [[a1]] -> (a1 -> Number, a2 -> a2) -> a2 -> a2
funcionSinPudor x y z
        | (length . filter even . map (fst y) $ head x) > 10 = id z
        | otherwise = snd y z


{-
 La funcionSinPudor recibe 3 parametros y maneja 3 tipos de datos. Ademas, la misma se divide en 2 partes por guardas.

Para empezar se observa que  se realiza un condicional al comparar con un numero
por lo que el resultado de la composicion de funcion en la primera parte tiene que ser tambien un numero. 

En primer lugar,  sabemos que el primer parametro
es una lista de listas ya que en la primera parte (composicion de funciones) se toma su primer elmento y ademas despues se hace un map.
En consecuencua, como ambos necesitan listas, se tiene una lista con sublistas de tipo generico A. 

Luego, el map nombrado anteriormente  se realiza con una funcion extraida de la tupla del segundo parametro que nos permite deducir que
el segundo parametro "y" es una tupla.   En adicion, se deduce que esa funcion ingresa un generico de tipo A y
 devuelve un numero ya que luego se realiza un filter con un even que requiere datos de tipo numerico.

Por otra parte,  podemos deducir que z (tercer parametro pasado parcialente) es de OTRO tipo generico B que es lo que devuelve el ID. 

Finalmente, la  segunda parte de la tupla  tambien es una funcion. Esta que recibe un dato generico de tipo B, cual se debe a que espera como input el z(tercer parametro) 
que es de tipo b,y devuelve otro de tipo B, ya que  el "id z" nos marca el tipo que tiene que ser B. Ademas,
porque al usar guardas sabemos que  tiene que ser del mismo tipo.


Entonces tenemos un tipo A, un tipo B y un tercero de tipo Number.
En adicion la funcion recible una lista de sublistas de tipo A, una tupla  de 2 funciones y un dato generico de tipo B.

-}

