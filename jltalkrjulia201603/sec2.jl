using Gadfly



G = [1.0 0.2 0.2; 0.1 2.0 0.4; 0.3 0.1 3.0];

N = size(G)[1];
K = 50; # Number of iterations of the circuit

gamma = 3.0;
alpha = 1.2;
sigma = 0.01;

A = ((alpha * gamma * G) .* (ones(3,3) - eye(3))) ./ repmat(diag(G), 1, 3);

b = alpha * gamma * diag(G);

p    = zeros(N, K);
SINR = zeros(N, K);


p[:,1]    = [0.1 0.1 0.1];
q         = sigma + (G - diagm(diag(G))) * p[:,1];
SINR[:,1] = diag(G)' * p[:,1] ./ q;

for i = 2:K
    p[:,i]    = A * p[:,i-1] + b;
    q         = sigma + (G - diagm(diag(G))) * p[:,i];
    SINR[:,i] = diag(G)' * p[:,i] ./ q;
end


plot(x = 1:K, y = p[1,:], Geom.line())
