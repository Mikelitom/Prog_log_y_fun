-- Definición del tipo de datos para un árbol ternario
-- Cada nodo tiene un valor y tres subárboles (izquierdo, medio, derecho)
data TernaryTree a = Empty 
                   | Node a (TernaryTree a) (TernaryTree a) (TernaryTree a)
                   deriving (Show)

-- Función para insertar un valor en el árbol ternario
-- Usa una estrategia de inserción basada en comparaciones
insert :: Ord a => a -> TernaryTree a -> TernaryTree a
insert x Empty = Node x Empty Empty Empty
insert x (Node value left middle right)
    | x == value = Node value left middle right  -- No permite duplicados
    | x < value  = Node value (insert x left) middle right
    | x > value  = Node value left middle (insert x right)

-- Función para buscar un valor en el árbol ternario
search :: Ord a => a -> TernaryTree a -> Bool
search _ Empty = False
search x (Node value left middle right)
    | x == value = True
    | x < value  = search x left
    | x > value  = search x right

-- Función para realizar un recorrido en orden (inorder traversal) para árbol ternario
-- Recorre: subárbol izquierdo, nodo actual, subárbol medio, nodo actual, subárbol derecho
inorderTraversal :: TernaryTree a -> [a]
inorderTraversal Empty = []
inorderTraversal (Node value left middle right) = 
    inorderTraversal left ++ [value] ++ inorderTraversal middle ++ [value] ++ inorderTraversal right

-- Función para contar el número de nodos en el árbol
size :: TernaryTree a -> Int
size Empty = 0
size (Node _ left middle right) = 1 + size left + size middle + size right

-- Función para encontrar la profundidad máxima del árbol
depth :: TernaryTree a -> Int
depth Empty = 0
depth (Node _ left middle right) = 
    1 + maximum [depth left, depth middle, depth right]

-- Ejemplo de uso
main :: IO ()
main = do
    -- Crear un árbol ternario con algunos valores
    let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 4 (insert 6 Empty)))))
    
    putStrLn "Árbol ternario creado:"
    print tree
    
    putStrLn "\nBúsqueda de valores:"
    print $ search 3 tree  -- Debería imprimir True
    print $ search 6 tree  -- Debería imprimir True
    print $ search 8 tree  -- Debería imprimir False
    
    putStrLn "\nRecorrido en orden:"
    print $ inorderTraversal tree
    
    putStrLn "\nInformación del árbol:"
    putStrLn $ "Número de nodos: " ++ show (size tree)
    putStrLn $ "Profundidad del árbol: " ++ show (depth tree)