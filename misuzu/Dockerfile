FROM php:8.2-apache
COPY --from=composer:2.5.4 /usr/bin/composer /usr/bin/composer
RUN apt-get update && apt-get install -y \
    git \
    vim \
    unzip \
    && docker-php-ext-install pdo_mysql
