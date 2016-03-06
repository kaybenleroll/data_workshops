
k1 = 1
k2 = 2

h = 0.01

A = [-k1   0 0;
      k1 -k2 0;
       0  k2 0]

A_update = eye(3) + h * A
n_steps  = 1000

x = zeros(3, n_steps)

x[:,1] = [1; 0; 0]

for i = 2:n_steps
    x[:,i] = A_update * x[:,i-1]
end
