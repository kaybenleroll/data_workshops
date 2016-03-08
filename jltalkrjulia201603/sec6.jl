using Gadfly
using Distributions

function calc_covar(x, y)
    N1 = length(x)
    N2 = length(y)

    sigma = zeros(N1, N2)

    for i = 1:N1
        for j = 1:N2
            sigma[i, j] = exp(-0.5 * (abs(x[i] - y[j]) / 1)^2)
        end
    end

    return sigma
end


N = 201
x = linspace(-1, 1, N)

mu    = zeros(N)
sigma = calc_covar(x, x)

### Need to make sigma postive-definite
d, v = eig(sigma)

d[d .< 1e-12] = 1e-12

sigma = v * diagm(d) * v'



gp_data = rand(MvNormal(mu, sigma), 50)


### Regression code
data_x = [-4 -3 -2 -1  0  1  2  4]
data_y = [-2  0  1  1  2  2 -1  1]

N = 1001
x = linspace(-5, 5, N)

#kxx_inv <- solve(calc_covar(data_dt$x, data_dt$x));

#Mu    <- calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% data_dt$y;
#Sigma <- calc_covar(x_seq, x_seq) - calc_covar(x_seq, data_dt$x) %*% kxx_inv %*% calc_covar(data_dt$x, x_seq);

kxx_inv = inv(calc_covar(data_x, data_x))
Mu      = calc_covar(x, data_x) * kxx_inv * data_y'
Sigma   = calc_covar(x, x) - calc_covar(x, data_x) * kxx_inv * calc_covar(data_x, x)

### Need to make sigma postive-definite
d, v = eig(Sigma)

#d[d .< 1e-12] = 1e-12

#Sigma = v * diagm(d) * v'


gp_data = rand(MvNormal(Mu, Sigma), 100)
