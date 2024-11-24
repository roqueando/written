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

O `MSE` significa Minimum Squared Error (ou Erro Quadrático Médio) é uma função de perda que calcula a diferença entre $Y_i$ (o valor certo que queremos alcançar) e $\hat{Y}_i$ (valor que previmos)

<p class="title is-2">Otimizadores</p>

Além 
</div>
