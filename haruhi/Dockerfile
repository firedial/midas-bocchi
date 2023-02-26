FROM php:8.2-apache
COPY --from=composer:2.5.4 /usr/bin/composer /usr/bin/composer
RUN apt-get update && apt-get install -y \
    git \
    vim \
    unzip \
    npm \
    && docker-php-ext-install pdo_mysql

RUN curl -fsSL https://deb.nodesource.com/setup_current.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g npm
