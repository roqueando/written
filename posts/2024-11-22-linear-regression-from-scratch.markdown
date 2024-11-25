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
```
Alguns tipos são como alias para melhor leitura do código como: `Coefficients`, `Datapoint` e `TrainingSet`, que são os parâmetros, um ponto de dado e o conjunto de dados de treino. Uma boa prática em haskell é colocar a configuração de algum algoritmo dentro de um tipo de dado algébrico (o tipo `data`), pois conseguimos resgatar fácil esses valores depois.

O `LinearConfig` configura o que precisamos para de fato fazer esse algoritmo rodar:
<ul>
    <li>
        `learningRate`: é a taxa de aprendizado, em muitas notações matemáticas ela pode está apresentada como a letra grega $\alpha$ (alpha). Essa taxa determina o quão devagar ou rápido queremos que nosso algoritmo aprenda. Aqui devemos ter cuidado pois um valor muito baixo faz com que o algoritmo demore a convergir, e um valor muito alto pode causar `overfitting`.
    </li>
    <li>
        `iterations`: por não se tratar de uma implementação
    </li>
</ul>
</div>
