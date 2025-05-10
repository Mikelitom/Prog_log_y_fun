-- Definición del tipo de datos para un árbol binario
-- 'a' es un parámetro de tipo genérico, lo que permite crear árboles de cualquier tipo
data BinaryTree a = Empty                  -- Representa un árbol vacío
                  | Node a (BinaryTree a) (BinaryTree a)  -- Representa un nodo con:
                                                          -- 1. Un valor
                                                          -- 2. Un subárbol izquierdo
                                                          -- 3. Un subárbol derecho
                  deriving (Show)  -- Permite imprimir el árbol directamente

-- Función para insertar un valor en el árbol binario
-- Usa restricción de tipo 'Ord a' para permitir comparaciones
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
-- Caso base: si el árbol está vacío, crea un nuevo nodo con el valor
insert x Empty = Node x Empty Empty

-- Caso recursivo para insertar en un árbol no vacío
-- Maneja tres posibles escenarios de comparación
insert x (Node value left right)
    | x == value = Node value left right  -- Si el valor ya existe, no se inserta duplicado
    | x < value  = Node value (insert x left) right  -- Si el valor es menor, inserta en subárbol izquierdo
    | x > value  = Node value left (insert x right)  -- Si el valor es mayor, inserta en subárbol derecho

-- Función para buscar un valor en el árbol
-- Devuelve un booleano indicando si el valor está presente
search :: Ord a => a -> BinaryTree a -> Bool
-- Caso base: árbol vacío, valor no encontrado
search _ Empty = False

-- Caso recursivo para buscar en un árbol no vacío
search x (Node value left right)
    | x == value = True   -- Valor encontrado en el nodo actual
    | x < value  = search x left   -- Busca en el subárbol izquierdo si el valor es menor
    | x > value  = search x right  -- Busca en el subárbol derecho si el valor es mayor

-- Función para realizar un recorrido en orden (inorder traversal)
-- Recorre el árbol de menor a mayor
inorderTraversal :: BinaryTree a -> [a]
-- Caso base: árbol vacío devuelve lista vacía
inorderTraversal Empty = []

-- Caso recursivo para recorrido en orden
-- 1. Recorre subárbol izquierdo
-- 2. Añade valor del nodo actual
-- 3. Recorre subárbol derecho
-- Usa concatenación de listas (++)
inorderTraversal (Node value left right) = 
    inorderTraversal left ++ [value] ++ inorderTraversal right

-- Función principal para demostrar el uso del árbol binario
main :: IO ()
main = do
    -- Crea un árbol insertando valores: 5, 3, 7, 1, 4
    -- Usa composición de funciones para insertar múltiples valores
    let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 4 Empty))))
    
    -- Imprime el árbol completo
    putStrLn "Árbol creado:"
    print tree
    
    -- Demuestra la función de búsqueda
    putStrLn "\nBúsqueda de valores (3, 6):"
    print $ search 3 tree  -- Debería imprimir True (valor presente)
    print $ search 6 tree  -- Debería imprimir False (valor ausente)
    
    -- Muestra el recorrido en orden del árbol
    putStrLn "\nRecorrido en orden:"
    print $ inorderTraversal tree  -- Debería imprimir [1,3,4,5,7]