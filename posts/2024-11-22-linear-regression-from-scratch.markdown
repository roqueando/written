---
title: Regressão Linear com Haskell
author: roqueando
---

<div class='content'>
<p class="title is-2"> Primeiro de tudo, o que é uma Regressão Linear? </p>

Bom, isso é uma técnica de estatística e econometria que consite em "modelar" uma relação entre duas váriaveis ou mais (conhecida também como regressão linear multivariada). Em outras palavras, é basicamente uma equação linear que nós aprendemos no ensino médio, junto a geometria, probabilidade e álgebra linear (onde mexemos com plano cartesiano e espaços 2D), e pensávamos que nunca iríamos utilizar isso futuramente.

Lembra quando fazíamos equações de primeiro grau: $$f(x) = ax + b$$? Descobri que isso se chama também como equação da linha, ou equação linear, na qual traça uma linha num espaço 2D. Isso é bem utilizado em análise de dados e no campo científico! Dentro desse espaço essa linha pode cortar ou não alguns pontos de interesse que são chamados de `pontos de dados` (irei explicar em breve), e qualquer ação que você queira fazer com eles, você precisará de utilizar algebra linear.

Sendo assim, a algebra linear é uma ferramenta poderosa para o entendimento da regressão linear. E com isso utilizaremos alguns pontos da mesma para explicar sobre esse algoritmo.

<br/>
<p class="title is-2">Destrinchando o algoritmo</p>
A explicação mais direta é que usamos a regressão para aprender os melhores `parâmetros`, que vão ser utilizados para traçar uma reta entre os `pontos de dados` e melhor separar eles. Essa separação nos dá vários resultados que podemos usar de diversas formas, uma delas é saber classificar os pontos de dados em qual "categoria" ou "classificação" eles possuem, outra para prever um certo número.

O algoritmo de regressão linear tem o mesmo formato que a equação linear, porém em notação matemática fica parecendo isso: $$f(x_i) = \theta_1 + \theta_2 x_i$$, o que não parece ser assustador, porém se fosse apenas isso seria fácil. Nessa equação temos alguns símbolos algébricos para que possamos entender e avançar para o próximo passo.
<ul>
    <li>
        $$\theta_1$$ e $$\theta_2$$: O nome desse símbolo é Theta e geralmente eles são o que chamamos de parâmetros, e embaixo deles são os índices, que nesse caso são apenas dois. Os parâmetros nesse contexto, diferentes dos argumentos e parâmetros de uma função de programação, são argumentos que não são manipuláveis pelo programador e sim pelo algoritmo em si.
    </li>
    <li>
        $$x_i$$: Essa é nossa entrada, geralmente ele é um conjunto $$X_n$$ onde $$n$$ é a quantidade de amostras (dados) que esse conjunto tem.
    </li>
</ul>
Com isso a temos ainda mais alguns passos, pois depois de você ter alguns parâmetros como você saberia que o algoritmo conseguiu chegar na resposta certa? Para isso temos o que chamamos de "Função de Custo" ou "Função de Perda". Se você precisa saber a quanto falta para chegar de um ponto a outro você faz a diferença entre seu ponto atual e o ponto que você quer chegar!

Assim, temos uma das formas mais comum para função de perda: $$MSE = \frac{1}{n} \sum_{i=0}^{n}(Y_i -\hat{Y}_i)^2$$

O `MSE` significa Minimum Squared Error (ou Erro Quadrático Médio) e é uma função de perda que calcula a diferença entre $Y_i$ (o valor certo que queremos alcançar) e $\hat{Y}_i$ (valor que previmos), após isso ele eleva ao quadrado, fazendo com que valores negativos "brilhem", ou seja, apareçam mais, e é feito a somatória dos resultados.

A razão para essas funções existirem é que no final ela nos da um número que diz o quão longe estamos do valor que queremos alcançar, então quanto mais perto de 0 menos estamos errando.

<p class="title is-2">Otimizando parâmetros</p>

Além de calcular a função de custo, temos os otimizadores, que vão dizer como podemos aproximar nossa perda até zero. Uma forma muito primitiva (ou mais simples para aprendizado) é o uso das derivadas:
$$f'(x) = \lim_{h \to 0} = \frac{f(x + h) - f(x)}{h}$$

Derivadas por definição é taxa de variação instantânea em relação de $y$ em relação a $x$, mas o que isso significa na real? Bom, é como descobrimos como atualizar nossos parâmetros. A Derivada serve para calcular a distância correta da função de custo e o resultado nós subtraimos do valor dos parâmetros $\theta_1$ e $\theta_2$ (pois estamos aproximando a zero).

<p class="title is-2">"Haskellizando" o algoritmo</p>

Agora que temos práticamente a base, vamos começar desenvolvendo alguns tipos. Haskell é bastante conhecido por sua extensibilidade para tipagem e como podemos modelar dados a partir deles (por isso o nome Tipos de Dados Algébricos).

Num arquivo `Types.hs` teremos esse formato:
```haskell
module Linha.Types where

newtype Coefficients = Coefficients (Float, Float) deriving Show
newtype Datapoint = Datapoint (Float, Float)
newtype TrainingSet = TrainingSet [Datapoint]

data = LinearConfig
    { learningRate :: Float,
      trainingSet :: TrainingSet,
      iterations :: Int
    }
type LearningRate = Float
```
Alguns tipos são como alias para melhor leitura do código como: `Coefficients`, `Datapoint` e `TrainingSet`, que são os parâmetros, um ponto de dado e o conjunto de dados de treino. Uma boa prática em haskell é colocar a configuração de algum algoritmo dentro de um tipo de dado algébrico (o tipo `data`), pois conseguimos resgatar fácil esses valores depois.

O `LinearConfig` configura o que precisamos para de fato fazer esse algoritmo rodar:
<ul>
    <li>
        `learningRate`: é a taxa de aprendizado, em muitas notações matemáticas ela pode está apresentada como a letra grega $\alpha$ (alpha). Essa taxa determina o quão devagar ou rápido queremos que nosso algoritmo aprenda. Aqui devemos ter cuidado pois um valor muito baixo faz com que o algoritmo demore a convergir, e um valor muito alto pode causar `overfitting`.
    </li>
    <li>
        `iterations`: por não se tratar de uma implementação que é inteligente o suficiente para ter uma condição de parada, forçamos o algoritmo a seguir uma quantidade limitada de iterações. Normalmente há uma condição de parada interna para que não seja necessário colocar iterações.
    </li>
</ul>


<p class="title is-3">Parte 1: Função Wrapper</p>

Em Haskell a melhor forma de você expressar o código é ter um wrapper, uma função que vai ser o ponto de entrada. Aqui teremos uma função para executar de fato a regressão linear, chamada de `run`.
```haskell
run :: Coefficients -> LinearConfig -> Coefficients
run cs (LinearConfig _ _ 0) = cs
run cs linearConfig = run params newLinearConfig
    where
        params = newParams cs lr ts
        newLinearConfig = LinearConfig { learningRate = lr, trainingSet = ts, iterations = (it - 1) }
        (LinearConfig lr ts it) = linearConfig
```

Agora vamos por partes:
```haskell
run :: Coefficients -> LinearConfig -> Coefficients
```
Isso define o que nossa função vai ser, ela recebe coeficientes (parâmetros), uma configuração do algoritmo e retorna novos coeficientes

```haskell
run cs (LinearConfig _ _ 0) = cs
```
Como sabemos o `LinearConfig` ele é composto por 3 atributos: learning rate, training set e iterations, em Haskell conseguimos pegar exatamente um valor por pattern matching e ver sobre uma condição de parada quando a iteração chega a 0, assim fazendo ele retornar os coeficientes gerados.

```haskell
run cs linearConfig = run params newLinearConfig
    where
        params = newParams cs lr ts
        newLinearConfig = LinearConfig { learningRate = lr, trainingSet = ts, iterations = (it - 1) }
        (LinearConfig lr ts it) = linearConfig
```
Aqui vemos a primeira ideia de recursão, pois em Haskell não existe for loops. Então rodamos em recursividade passando parametros e uma nova configuração. Em Haskell podemos também definir dentro do corpo da função, novas funções e variaveis fazendo com que fique fácil atribuir quem faz o que.

Portanto temos a variável `params` que consiste em criar novos parametros (veremos essa função mais adiante) passando os coeficientes atuais, o learning rate e o training set. Temos também a váriavel `newLinearConfig` que apenas gera uma nova configuração passando os atributos necessário e também diminuindo 1 passo na iteração atual, logo se for 10 iterações ela vira 9, e por fim temos apenas um pattern matching para usar os valores da configuração.

<p class="title is-3">Parte 2: Gerando novos parâmetros</p>

Agora precisamos para cada iteração gerar coeficientes novos, e para isso vamos também falar sobre função de custo para saber o quão longe do certo estamos.
```haskell
newParams :: Coefficients -> LearningRate -> TrainingSet -> Coefficients
newParams cs lr ts = Coefficients (newp0, newp1)
    where
      deltas = map (calculateDelta cs) ts' -- mais adiante vamos ver sobre o delta
      newp0 = p0 - lr * avg deltas
      newp1 = p1 - lr * avg adjustedDeltas
      adjustedDeltas = adjustDelta deltas ts'
      Coefficients (p0, p1) = cs
      TrainingSet ts' = ts
```

Começamos com a tipagem que a função `newParams` espera: Coeficientes (novos ou randomizados de começo), a taxa de aprendizado, o conjunto de treino e irá retornar novos parâmetros.A parte interessante é na definição das variaveis que permite o cálculo de novos parâmetros
```haskell
    where
      deltas = map (calculateDelta cs) ts'
```

Primeiro definimos os deltas (a função de perda para sabermos a diferença entre $x$ e $y$), que nada mais é que um map de uma lista passando o conjunto de treino e uma função para ser aplicada nessa lista. O quote nessa váriavel, é um padrão no Haskell e também na matemática quando temos variáveis com mesmo significado porém com conteúdos diferentes, nesse caso o `ts'` é quando faz o pattern matching para desacoplar o valor do construtor que definimos como tipo.

```haskell
  newp0 = p0 - lr * avg deltas
  newp1 = p1 - lr * avg adjustedDeltas
  adjustedDeltas = adjustDelta deltas ts'
```
Aqui temos a criação dos novos coeficientes, perceba que o valor é atualizado com a taxa de aprendizado vezes a média dos deltas.

```haskell
calculateDelta :: Coefficients -> Datapoint -> Float
calculateDelta params dps = p0 + p1 * x - y
  where
    Coefficients (p0, p1) = thetas
    Datapoint (x, y) = dps

adjustDelta :: [Float] -> [Datapoint] -> [Float]
adjustDelta deltas datapoints = map (uncurry (*)) zipped
  where
    xs = map (\(Datapoint (x, _)) -> x) datapoints
    zipped = zip deltas xs

```

Aqui temos as duas funções que calculam o delta. Na regressão linear temos uma convenção sobre o segundo parâmetro que para ele não ter o mesmo valor do primeiro multiplicamos por $x^(i)$ que resulta no valor do delta ajustado. Assim o calculo de delta comum é a execução da lógica $$\theta_0 + \theta_1 * x $$ subtraindo o $y$, e o valor ajustado é aplicar literalmente a multiplicação do $x^i$.

```haskell
avg :: [Float] -> Float
avg xs = realToFrac (sum xs) / genericLength xs
```
Por fim temos a uma função auxiliar para cálculo da média de uma lista de floats.


<p class="title is-3">Parte 3: Rodando o algoritmo</p>
Uma das formas de rodar esse código poderia ser pelo próprio módulo Main do seu projeto (se você ficou perdido de como criar um projeto Haskell em breve faço um artigo sobre).

```haskell
inference :: Float -> Coefficients -> Float
inference x cs = p0 + p1 * x
  where
    Coefficients (p0, p1) = cs

main :: IO ()
main = do
  let params = Coefficients (0, 0)
  let lr = 0.3
  let ts = TrainingSet [Datapoint (0, 50), Datapoint (1, 60), Datapoint (2, 70)]
  let iters = 500
  let lc = LinearConfig { learningRate = lr, trainingSet = ts, iterations = iters }
  let cs = run params lc
  let found = inference 3 cs
  print $ show cs
  print $ show found
```

Primeiro definimos uma função de inferência, ou seja rodar o modelo usado passando um input qualquer e os coeficientes gerados pelo modelo. Após isso criamos um conjunto de treino que se você perceber, possui um padrão que queremos que o modelo reconheça: ao passar o número 0 retorne 50, ao passar 1, retorne 60.
Então usamos a função de inferência junto aos novos coeficientes e printamos tanto os novos parâmetros quanto o resultado.

```
> Coefficients (49.99999,10.000008)
> 80
```


<p class="title is-2">Conclusão</p>
Com isso aprendemos que a base matemática te permite criar diversas coisas! Claro que esse foi um exemplo muito simples, mas alguns outros passos seriam aplicar isso em larga escala usando matrizes e vetores, ou aplicar redes neurais artificiais como perceptron usando bibliotecas como Tensorflow (que existe bindings para haskell!), são infinitas possibilidades.
Até a próxima! O ṣeun!

</div>
