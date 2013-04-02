function log_of_normal = log_of_normal(x, mu, sigma)

log_of_normal = -((x-mu).^2)./(2*sigma.^2) -log(sigma*sqrt(2*pi));