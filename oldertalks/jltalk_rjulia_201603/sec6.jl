using DataFrames
using Gadfly
using Distributions

include("functions.jl")



N = 201
x = linspace(-1, 1, N)

mu    = zeros(N)
sigma = calc_covar(x, x)

### Need to make sigma postive-definite
d, v = eig(sigma)

d[d .< 1e-12] = 1e-12

sigma = v * diagm(d) * v'



gp_data = rand(MvNormal(mu, sigma), 50)

gp_plot1 = gp_data |> DataFrame |> matplot

draw(PNG("sec6_gp_simple.png", 10cm, 7cm), gp_plot1)




### Regression code
data_x = [-4 -3 -2 -1  0  1  2  4]
data_y = [-2  0  1  1  2  2 -1  1]

N = 201
x = linspace(-5, 5, N)


kxx_inv = inv(calc_covar(data_x, data_x))
Mu      = calc_covar(x, data_x) * kxx_inv * data_y'
Sigma   = calc_covar(x, x) - calc_covar(x, data_x) * kxx_inv * calc_covar(data_x, x)

### Need to make sigma postive-definite
Mu_vec   = Mu[:,1]
Sigma_PD = Sigma - minimum(eigvals(Symmetric(Sigma))) * I


gpreg_data = rand(MvNormal(Mu_vec, Sigma_PD), 100)

gp_plot2 = gpreg_data |> DataFrame |> matplot

draw(PNG("sec6_gpreg.png", 10cm, 7cm), gp_plot2)
