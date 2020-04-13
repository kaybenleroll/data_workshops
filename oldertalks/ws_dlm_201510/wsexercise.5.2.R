source("lib.R");


x_plot    <- qplot(1:10, x,    geom = 'line', colour = I('black'), xlab = 'Timestep', ylim = c(-0.2, 0.2));
p_plot    <- qplot(1:10, p,    geom = 'line', colour = I('green'), xlab = 'Timestep', ylim = c(-0.5, 1.2));
pdot_plot <- qplot(1:10, pdot, geom = 'line', colour = I('blue'),  xlab = 'Timestep', ylim = c(-0.2, 0.3));

grid.arrange(x_plot, p_plot, pdot_plot, nrow = 3);
