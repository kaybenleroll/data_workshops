

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


function matplot(d::DataFrame)
    (row,col) = size(d)
    dStack = stack(d)
    dStack[:ndx] = rep(1:row,col)
    Gadfly.plot(dStack,x=:ndx,y=:value,group=:variable,Geom.line)
end
