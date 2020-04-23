using Gadfly
using SpecialMatrices


p10  = linspace(9.5,0.5,10)
pd10 = ones(10)
p0   = [linspace(4.5,0.5,5); zeros(5)]

A = [p10'; pd10'; p0']
y = [1; 0; 0]

x = pinv(A) * y

print(sqrt(sum(x' * x)))


T1 = full(Toeplitz([zeros(9); 1; ones(9)]))
pdot = T1 * x

T2 = full(Toeplitz([zeros(9); linspace(0.5,9.5,10)]))
p = T2 * x


plotoutput = plot(layer(x = 1:10, y = x,    Geom.line(), Theme(default_color = colorant"red"))
                 ,layer(x = 1:10, y = p,    Geom.line(), Theme(default_color = colorant"green"))
                 ,layer(x = 1:10, y = pdot, Geom.line(), Theme(default_color = colorant"blue"))
                  )

draw(PNG("sec5_plots.png", 10cm, 7cm), plotoutput)
