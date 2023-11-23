-- = é equivalencia
-- funções e parâmetros começam com minúscula 
-- tipos começam com maiúscula
-- último tipo é o tipo de retorno, demais são os tipos de parâmetros
-- a == b a igual que b
-- a /= b a diferente que b

polinomio :: Int -> Int
polinomio x = x*x + 10*x + 2


maior :: Int -> Int -> Int
maior a b = if a >= b
  then a
  else b


-- usando guarda(|) agora:
-- otherwise é o default
-- indentação é importante
maiorg :: Int -> Int -> Int
maiorg a b 
      | a > b = a
      | a < b = b
      | otherwise = 0


charcase :: Char -> String
charcase ch | ch >= 'a' && ch <= 'z' = "Minusculo"
            | ch >= 'A' && ch <= 'Z' = "Maiusculo"
            | otherwise = "Desconhecido"


-- definições locais (where)
areaheron :: Float -> Float -> Float -> Float
areaheron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
  where
    s = (a+b+c)/2


--let definições in expressão
areaheron2 :: Float -> Float -> Float -> Float
areaheron2 a b c = let s = (a+b+c)/2 
                   in sqrt (s*(s-a)*(s-b)*(s-c))


--recursão: caso base e caso recursivo
divrec :: Int -> Int -> Int
divrec a b
    | b > a = a
    | b == a = 0
    | otherwise = divrec (a-b) b


--recursão em cauda: economiza recursos e aumenta eficiência
fibocauda :: Int -> Int -> Int -> Int
fibocauda n a1 a2
  | n == 0 = a1
  | n == 1 = a2
  | n > 1 = fibocauda (n-1) a2 (a1+a2)


-- listas
-- ls1[1 .. 10] numeros de 1 a 10
-- ls2[1,3 .. 10] impares de 1 a 10
-- ls2[10,8 .. 0] pares de 10 a 0
-- operador de construção(:) -> 6:8:[] resulta em [6,8]
-- ++ concatena listas
compr :: [Int] -> Int
compr [] = 0
compr (h:t) = 1 + compr t


--possuichar (h:t) -> h é a cabeça e t são os elementos menos a cabeça
possuichar :: [Char] -> Char -> Bool
possuichar [] ch = False
possuichar (h:t) ch | h == ch = True
                    | otherwise = possuichar t ch


qsort :: [Int] -> [Int]
qsort [] = []
qsort (h:t) = qsort [y | y <- t, y < h] -- menores que o pivo
              ++ [h] -- o proprio pivo
              ++ qsort [y | y <- t, y >= h] -- maiores que o pivo


-- Tuplas
--type: novo nome a um tipo
--em duplas pode usar fst e snd para first e second
type NomeAluno = String
type MediaNota = Int
type Aluno = (NomeAluno, MediaNota)
type Turma = [Aluno]

aprovados :: Turma -> Int -> [NomeAluno]
aprovados tma nota = [nome | (nome, media) <- tma, media >= nota]


--casamento de padrões
-- o chamado padrao curinga: _
opp :: (Int, (Int, Int)) -> Int
opp z = if fst z == 1
        then fst (snd z) + snd (snd z) -- primeira resposta
        else if fst z == 2
        then fst (snd z) - snd (snd z) -- segunda resposta
        else 0 -- resposta padrão

--agora com curinga:
opp2 :: (Int, (Int, Int)) -> Int
opp2 (1, (a, b)) = a + b
opp2 (2, (a, b)) = a - b
opp2 _ = 0

-- Funções de Alta Ordem: aquelas que operam sobre outras funções
dobra :: Int -> Int
dobra n = n + n

quadrado :: Int -> Int
quadrado n = n * n

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (prim:outros) = (dobra prim) : (dobraLista outros)

--agora com alta ordem para poder usar dobra ou quadrado na lista
-- map
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (prim:outros) = (f prim) : (mapInt f outros)

-- filtro
filtro :: (Int -> Bool) -> [Int] -> [Int]
filtro f [] = []
filtro f (cab:cauda)
  | (f cab) == True = cab : (filtro f cauda)
  | otherwise = filtro f cauda

pares :: Int -> Bool
pares x = (mod x 2 == 0)

impares :: Int -> Bool
impares x = (mod x 2 == 1)

--ja existem a map e filter
-- funções λ ou funções anônimas: (\x -> x+1) 1 => 2